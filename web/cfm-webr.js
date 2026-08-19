// cfm-webr.js — bootstrapper webR og eksponerer window.CFM_R for app-koden.
//
// Strategi:
//   1. Last webR fra CDN (ESM-modul)
//   2. Boot session
//   3. Fetch alle R-filer og CSV-er fra /r-pkg/, stage dem i webR sitt
//      virtuelle filsystem under /cfm/
//   4. Source cfm-bootstrap.R → definerer cfm_draw_json()
//   5. Eksponer en JS-API:
//        window.CFM_R.ready       — Promise<void>
//        window.CFM_R.status      — "loading" | "ready" | "error"
//        window.CFM_R.statusText  — fremgang for UI
//        window.CFM_R.drawLife({min_age, max_age, gender}) → Promise<Object>
//      Plus en CustomEvent på document: "cfm-r-status" når status endrer seg.

(function () {
  const STATUS_EVT = 'cfm-r-status';

  const api = {
    status: 'loading',
    statusText: 'Initialiserer R…',
    ready: null,
    drawLife: null,
    rerollLife: null,
    _webR: null,
  };
  window.CFM_R = api;

  function setStatus(status, text) {
    api.status = status;
    api.statusText = text;
    document.dispatchEvent(new CustomEvent(STATUS_EVT, { detail: { status, text } }));
  }

  // Filer som må mountes i webR sin FS.
  const R_FILES = [
    'zzz.R', 'samplers.R', 'conditional.R', 'impossibility.R',
    'counterfact_me.R', 'constrained.R', 'narrate.R', 'print.R',
    'sources.R', 'audit.R', 'verify.R'
  ];

  // CSV-ene — listen samsvarer med .load_data() i cfm-bootstrap.R
  const CSV_FILES = [
    'age_distribution.csv', 'chronic_illness_prob.csv', 'counties.csv',
    'disability_by_age.csv', 'education_by_age.csv', 'education_levels.csv',
    'first_names.csv', 'first_names_cohort.csv', 'fylke_index_region.csv',
    'hobbies.csv', 'household_types.csv', 'housing_price_index.csv',
    'housing_prices.csv', 'immigrant_country_dist.csv',
    'immigration_start_year.csv', 'income_deciles.csv',
    'kommune_price_multiplier.csv', 'kommune_sentralitet.csv',
    'marital_status.csv', 'material_deprivation.csv', 'municipalities.csv',
    'n_children_by_cohort.csv', 'names_by_region.csv', 'nus_by_styrk.csv',
    'nus_detail_by_styrk.csv', 'nus_detailed.csv', 'nus_fields.csv',
    'occupations.csv', 'occupations_gender.csv', 'occupations_salary.csv',
    'party_baseline.csv', 'party_humor.csv', 'religion_baseline.csv',
    'religion_by_region.csv', 'religion_humor.csv', 'self_rated_health.csv',
    'social_isolation.csv', 'wealth_by_age.csv', 'wealth_deciles.csv',
    // Livsstil — lagt til i pakkeversjon 0.9.3/0.9.4
    'media_papers.csv', 'tv_hours.csv', 'sleep_hours.csv', 'diet_patterns.csv',
    'alcohol_patterns.csv', 'podcast_activity.csv', 'social_media_use.csv',
    // Kriminalitet — lagt til i 0.9.16
    'crime_victimization.csv', 'crime_safety_feeling.csv', 'minor_offences.csv'
  ];

  // Hjelpere: les innebygde assets (cfm-assets.js) hvis tilgjengelig,
  // ellers fall tilbake til runtime-fetch (kun nyttig under http-utvikling).
  // I den delte/bundlede fila finnes ALLTID window.CFM_ASSETS, saa ingen
  // fetch mot r-pkg/ skjer — derfor virker den ogsaa offline / fra file://.
  const A = (typeof window !== 'undefined' && window.CFM_ASSETS) || null;
  async function readR(name) {
    if (A && A.r && A.r[name] != null) return A.r[name];
    return (await fetch(`r-pkg/R/${name}`)).text();
  }
  async function readCSVText(name) {
    if (A && A.csv && A.csv[name] != null) return A.csv[name];
    return (await fetch(`r-pkg/extdata/${name}`)).text();
  }
  async function readBootstrap() {
    if (A && A.bootstrap) return A.bootstrap;
    return (await fetch('cfm-bootstrap.R')).text();
  }
  async function readReroll() {
    if (A && A.reroll) return A.reroll;
    return (await fetch('cfm-reroll.R')).text();
  }

  async function boot() {
    setStatus('loading', 'Laster webR-kjernen (~10 MB, cached etter forste gang)…');
    const { WebR } = await import('https://webr.r-wasm.org/latest/webr.mjs');
    const webR = new WebR();
    api._webR = webR;
    await webR.init();

    setStatus('loading', 'Henter R-pakkefiler…');
    // Mount /cfm/ og undermapper i webR-FS
    await webR.FS.mkdir('/cfm');
    await webR.FS.mkdir('/cfm/R');
    await webR.FS.mkdir('/cfm/extdata');

    // Når assets er innebygd, bruk deres egne nøkler som sannhet — da
    // følger nye CSV-er/R-filer med ved en pakkesync uten at listene
    // nedenfor må oppdateres i takt.
    const rList = (A && A.r) ? Object.keys(A.r) : R_FILES;
    const csvList = (A && A.csv) ? Object.keys(A.csv) : CSV_FILES;

    // Skriv R-filene (tekst)
    for (const f of rList) {
      const txt = await readR(f);
      const bytes = new TextEncoder().encode(txt);
      await webR.FS.writeFile(`/cfm/R/${f}`, bytes);
    }

    setStatus('loading', `Henter ${csvList.length} datafiler…`);
    let done = 0;
    await Promise.all(csvList.map(async (f) => {
      const txt = await readCSVText(f);
      await webR.FS.writeFile(`/cfm/extdata/${f}`, new TextEncoder().encode(txt));
      done++;
      if (done % 5 === 0) {
        setStatus('loading', `Henter datafiler (${done}/${csvList.length})…`);
      }
    }));

    setStatus('loading', 'Installerer jsonlite…');
    // jsonlite trengs for å serialisere R-objektet. Installer hvis ikke allerede.
    await webR.evalRVoid(`
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        webr::install("jsonlite")
      }
    `);

    setStatus('loading', 'Initialiserer CounterfactMe…');
    // Source bootstrap-scriptet. Vi sender innholdet inn via writeFile
    // istedenfor å fetche det inn i R, så det er én roundtrip færre.
    const bootR = await readBootstrap();
    await webR.FS.writeFile('/cfm/bootstrap.R', new TextEncoder().encode(bootR));
    // Reroll-motoren (cfm_draw_json + cfm_reroll_json) — sources sist av bootstrap.R.
    const rerollR = await readReroll();
    await webR.FS.writeFile('/cfm/reroll.R', new TextEncoder().encode(rerollR));
    await webR.evalRVoid(`source("/cfm/bootstrap.R")`);

    // Eksponer drawLife FØR vi melder "ready" — ellers vil status-lytteren
    // i app.jsx trigge en redraw før api.drawLife er satt (race → JS-fallback).
    api.drawLife = drawLife;
    api.rerollLife = rerollLife;
    setStatus('ready', 'CounterfactMe (R) klar.');
  }

  async function drawLife(opts = {}) {
    const { min_age = 0, max_age = 99, gender = 'any' } = opts;
    if (!api._webR) throw new Error('webR not booted');
    // Bygg R-uttrykket trygt — primitive typer, ingen string-injection
    const ageMin = Math.max(0, Math.min(99, parseInt(min_age, 10) || 0));
    const ageMax = Math.max(ageMin, Math.min(99, parseInt(max_age, 10) || 99));
    const g = gender === 'M' || gender === 'F' ? `"${gender}"` : 'NULL';

    const result = await api._webR.evalRString(
      `cfm_draw_json(min_age = ${ageMin}, max_age = ${ageMax}, gender = ${g}, lang = "no")`
    );
    return JSON.parse(result);
  }

  // Betinget om-trekning av én dimensjon. dim valideres mot whitelist;
  // ctxId er et heltall R ga oss; keep er pin-nøkler for brukerens låser.
  const REROLL_DIMS = new Set(['name', 'age', 'municipality', 'education',
    'occupation', 'income', 'marital_status', 'household', 'religion', 'party']);
  const KEEP_KEYS = new Set(['age', 'bg', 'sentr', 'edu', 'occ', 'nus', 'par',
    'sib', 'gp', 'inc', 'ms', 'household', 'ori', 'rel', 'party', 'housing',
    'wealth', 'children', 'name', 'mun', 'neet']);
  async function rerollLife(dim, ctxId, keep = []) {
    if (!api._webR) throw new Error('webR not booted');
    if (!REROLL_DIMS.has(dim)) throw new Error('ukjent reroll-dim: ' + dim);
    const id = parseInt(ctxId, 10);
    if (!Number.isFinite(id)) throw new Error('ugyldig ctx_id');
    const ks = keep.filter(k => KEEP_KEYS.has(k));
    const keepR = ks.length ? `c(${ks.map(k => `"${k}"`).join(', ')})` : 'character(0)';
    const result = await api._webR.evalRString(
      `cfm_reroll_json("${dim}", ${id}, keep = ${keepR}, lang = "no")`
    );
    return JSON.parse(result);
  }

  api.ready = boot().catch(err => {
    console.error('[CFM_R] boot failed', err);
    setStatus('error', 'Kunne ikke starte R: ' + (err && err.message || err));
    throw err;
  });
})();
