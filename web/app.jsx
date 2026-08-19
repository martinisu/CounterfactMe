// CounterfactMe app — vanilla JS for the main interactions, with a small
// React-based Tweaks panel mounted alongside.

const DIM_KEYS = [
  {key:"name", label:"Navn"},
  {key:"age", label:"Alder"},
  {key:"municipality", label:"Bosted"},
  {key:"origin", label:"Bakgrunn"},
  {key:"occupation", label:"Yrke"},
  {key:"education", label:"Utdanning"},
  {key:"field", label:"Studieretning"},
  {key:"income", label:"Inntekt"},
  {key:"housing", label:"Bolig"},
  {key:"household", label:"Husholdning"},
  {key:"marital", label:"Sivilstand"},
  {key:"religion", label:"Trosamfunn"},
  {key:"party", label:"Partipreferanse"},
  {key:"wealth", label:"Nettoformue"},
  {key:"bourdieu", label:"Klasseposisjon"},
  {key:"children", label:"Antall barn"},
  {key:"siblings", label:"Søsken"},
  {key:"hytte", label:"Hytte"},
  {key:"health", label:"Helse"}
];

// Hvilke dimensjoner som er meningsfulle ved en gitt alder. Mindreårige skal
// ikke se voksen-felter som klasseposisjon, partipreferanse, hytte osv. —
// disse fjernes helt fra kortet, ikke vist som "—".
function dimAppliesAt(key, age) {
  if (age == null) return true;
  if (age >= 18) return true;
  switch (key) {
    case "party":      return false; // R: .cond_party returnerer NA<18
    case "bourdieu":   return age >= 16;
    case "hytte":      return false; // barn eier ikke hytte
    case "wealth":     return false; // kids have 0 net wealth
    case "marital":    return false; // alltid "Ugift" — redundant
    case "household":  return false; // alltid "Bor hos foreldre" — redundant ift. Bolig
    case "children":   return false; // alltid 0 — gir ikke mening
    case "field":      return age >= 16; // ungdomsskole/vgs kan ha linje
    default: return true;
  }
}

const STATE = {
  filters: { gender: "any", county: "all", minAge: 0, maxAge: 99 },
  showProbability: true,
  showSources: true,
  locks: {},
  current: null,
  saved: []
};

// ---- Render counties ----
function renderCounties() {
  const sel = document.getElementById("f-county");
  CFM_DATA.counties.forEach(c => {
    const o = document.createElement("option");
    o.value = c.name;
    o.textContent = c.name;
    sel.appendChild(o);
  });
}

// ---- Filter wiring ----
function wireFilters() {
  document.getElementById("f-gender").addEventListener("change", e => {
    STATE.filters.gender = e.target.value; drawAndRender();
  });
  document.getElementById("f-county").addEventListener("change", e => {
    STATE.filters.county = e.target.value;
    if (STATE.locks.municipality) delete STATE.locks.municipality;
    drawAndRender();
  });
  document.getElementById("f-age-min").addEventListener("change", e => {
    STATE.filters.minAge = +e.target.value || 0; drawAndRender();
  });
  document.getElementById("f-age-max").addEventListener("change", e => {
    STATE.filters.maxAge = +e.target.value || 99; drawAndRender();
  });
}

// ---- R-bridge state ----
// Vi prefererer R-pakken når den er klar (cfm-webr.js setter window.CFM_R).
// Fallback til JS-sampleren (CFM.drawLife) brukes mens R laster, ved feil,
// eller når brukeren har satt locks (R-pakken støtter ikke locks).
const RBridge = {
  get ready() { return window.CFM_R && window.CFM_R.status === 'ready'; },
  get loading() { return window.CFM_R && window.CFM_R.status === 'loading'; },
  // Rejection-sampling: R-pakken aksepterer ikke county-filter, så vi
  // trekker opptil maxAttempts ganger til county matcher.
  async draw(filters) {
    const opts = {
      min_age: filters.minAge || 0,
      max_age: filters.maxAge || 99,
      gender: filters.gender && filters.gender !== 'any' ? filters.gender : 'any'
    };
    const wantCounty = filters.county && filters.county !== 'all' ? filters.county : null;
    const maxAttempts = wantCounty ? 40 : 1;
    let last = null;
    for (let i = 0; i < maxAttempts; i++) {
      const r = await window.CFM_R.drawLife(opts);
      const life = window.translateRLife(r);
      last = life;
      if (!wantCounty || life.county === wantCounty) return life;
    }
    return last; // ga opp; vis det vi fikk
  }
};

// Status-indikator i topbar
(function wireStatus() {
  const el = document.getElementById('r-status');
  if (!el) return;
  function render(status, text) {
    el.dataset.status = status;
    if (status === 'ready') el.textContent = 'Kilde · R-pakke';
    else if (status === 'error') el.textContent = 'Kilde · JS-fallback';
    else el.textContent = 'Kilde · laster R…';
    el.title = text || '';
  }
  // Initiell tilstand
  const cur = window.CFM_R?.status || 'loading';
  render(cur, window.CFM_R?.statusText);
  document.addEventListener('cfm-r-status', e => {
    render(e.detail.status, e.detail.text);
    // Når R blir klar, redraw det inneværende livet i R-shape
    if (e.detail.status === 'ready' && !STATE.current?._fromR && !STATE.userHasDrawn) {
      drawAndRender();
    }
  });
})();

// ---- Build a draw and put it in STATE.current ----
async function drawAndRender() {
  const useR = RBridge.ready && Object.keys(STATE.locks || {}).length === 0;
  let life;
  if (useR) {
    try {
      life = await RBridge.draw(STATE.filters);
      life._fromR = true;
    } catch (e) {
      console.warn('[CFM] R draw failed, falling back to JS sampler', e);
      life = CFM.drawLife({ filters: STATE.filters }, STATE.locks);
    }
  } else {
    life = CFM.drawLife({ filters: STATE.filters }, STATE.locks);
  }
  STATE.current = life;
  const card = document.getElementById('card');
  if (card) card.classList.remove('just-drawn', 'stamped');
  renderCard();
  renderProbability();
  renderWhy();
  // Story is delayed — typed out after the wheel + stamp finish.
  prepareStoryForTypewriter();
  // Lottery wheel → stamp animation
  if (card) {
    // remove any existing overlay
    card.querySelectorAll('.draw-overlay').forEach(el => el.remove());
    const overlay = document.createElement('div');
    overlay.className = 'draw-overlay';
    overlay.innerHTML = '<div class="lottery-wheel"></div>';
    card.appendChild(overlay);
    // Trigger card animation in the same frame
    requestAnimationFrame(() => {
      requestAnimationFrame(() => {
        card.classList.add('just-drawn');
      });
    });
    // Remove overlay after wheel + stamp finished
    setTimeout(() => overlay.remove(), 1850);
    // Brief card shake when the stamp slams down (~1600ms in)
    setTimeout(() => {
      if (!card.isConnected) return;
      card.style.animation = 'stampShake 180ms cubic-bezier(0.36,0.07,0.19,0.97)';
      setTimeout(() => { card.style.animation = ''; }, 200);
    }, 1620);
    // Story types itself out after the stamp lands.
    setTimeout(() => renderStory(), 2050);
    // Persist stamp visible after slam completes så det forblir på passet.
    setTimeout(() => {
      if (!card.isConnected) return;
      card.classList.add('stamped');
      const stampEl = card.querySelector('.stamp');
      if (stampEl) stampEl.style.opacity = '0.85';
    }, 1980);
  }
}

// ---- Card rendering ----
function fmtMoney(n) { return CFM.formatNok(n); }

// R-pakken (CounterfactMe v0.6.11) trekker kun fornavn — ingen etternavn-tabell.

function dimValueFor(key, life) {
  switch (key) {
    case "name": return { value: life.name, meta: life.gender === "F" ? "Kvinne" : "Mann" };
    case "age": return { value: `${life.age} år`, meta: ageBandLabel(life.age) };
    case "municipality": return { value: life.municipality, meta: life.munPop != null ? `${life.county} · ${life.munPop.toLocaleString("nb-NO").replace(/,/g," ")} innb.` : (life.county || "") };
    case "occupation": return { value: life.occupation, meta: occMeta(life) };
    case "education": return { value: life.education, meta: eduPositionLabel(life.eduCode) };
    case "field": return { value: life.field || "—", meta: life.field ? "NUS-fagfelt" : "Ingen" };
    case "income": return life.income_kid
      ? { value: life.income_label, meta: "Ukepenger" }
      : { value: fmtMoney(life.income_nok), meta: incomePositionLabel(life.income_nok) };
    case "household": return { value: life.household, meta: "" };
    case "marital": return { value: life.marital, meta: "" };
    case "religion": return { value: life.religion, meta: life.religion_humor || "" };
    case "party": {
      if (!life.party) return { value: "—", meta: "Under stemmealder" };
      return { value: life.party, meta: life.party_humor || "" };
    }
    case "wealth": return { value: fmtMoney(life.net_wealth), meta: (life.wealth_class || "") + (life.inheritance ? ` · arv ${(life.inheritance/1e6).toFixed(1)} M` : "") };
    case "bourdieu": {
      const b = life.bourdieu || {};
      if (!b.klasse || life.age < 16) return { value: "—", meta: life.age < 16 ? "For ung" : "" };
      return { value: b.klasse, meta: `Øk ${b.okonomisk} · Kul ${b.kulturell} · Sos ${b.sosial}` };
    }
    case "children": {
      const n = life.n_children;
      if (n == null) return { value: "—", meta: "" };
      if (life.age < 18) return { value: "—", meta: "For ung" };
      if (n === 0) return { value: "Ingen barn", meta: "" };
      return { value: n === 1 ? "1 barn" : `${n} barn`, meta: "" };
    }
    case "siblings": {
      const s = life.siblings;
      if (!s || s.count == null) return { value: "—", meta: "" };
      if (s.count === 0) return { value: "Enebarn", meta: "" };
      const names = (s.siblings || []).slice(0, 4).map(x => x.name).join(", ");
      return { value: s.count === 1 ? "1 søsken" : `${s.count} søsken`, meta: names };
    }
    case "hytte": {
      const h = life.hytte;
      if (!h) return { value: "Ingen hytte", meta: life.age < 25 ? "For ung" : "" };
      return { value: h.typeLabel, meta: `Verdt ${(h.value/1e6).toFixed(1)} M` };
    }
    case "health": {
      const h = life.health;
      if (!h || !h.selfRated) return { value: "—", meta: "" };
      const meta = h.chronic ? `Kronisk: ${h.chronicType}` : "Ingen kronisk lidelse";
      return { value: h.selfRated, meta };
    }
    case "origin": {
      const o = life.origin || {};
      if (o.kind === "norsk") return { value: "Norsk", meta: "To norskfødte foreldre" };
      const gen = o.generation === 2 ? "Andre­generasjon" : "Første­generasjon";
      return { value: o.country || "—", meta: gen };
    }
    case "housing": {
      const h = life.housing || {};
      if (h.status === "Eier") {
        const sqmTxt = h.sqm != null ? ` · ${h.sqm} m²` : "";
        const meta = h.currentValue != null
          ? `${h.purchaseYear ? `Kjøpt ${h.purchaseYear}${h.purchasePrice != null ? ` for ${(h.purchasePrice/1e6).toFixed(1)} M` : ""} · ` : ""}verdt ${(h.currentValue/1e6).toFixed(1)} M`
          : (h.label || "");
        return { value: `Eier${sqmTxt}`, meta };
      }
      if (h.status === "Leier") {
        const sqmTxt = h.sqm != null ? ` · ${h.sqm} m²` : "";
        const meta = h.rent != null ? `${h.rent.toLocaleString("nb-NO").replace(/,/g, " ")} kr/mnd` : "Leieforhold";
        return { value: `Leier${sqmTxt}`, meta };
      }
      return { value: h.status || "—", meta: h.label || "" };
    }
  }
  return { value: "—", meta: "" };
}
function ageBandLabel(age) {
  if (age <= 5) return "Småbarn";
  if (age <= 12) return "Skolebarn";
  if (age <= 19) return "Ungdom";
  if (age <= 29) return "Ung voksen";
  if (age <= 49) return "Voksen";
  if (age <= 66) return "Eldre voksen";
  if (age <= 79) return "Pensjonist";
  return "Eldre";
}
function occMeta(life) {
  if (life.occKind === "worker" && life.styrk1 != null) {
    const grp = ["Militære","Ledere","Akademiske","Høyskoleyrker","Kontor","Salg/service","Bønder/fiskere","Håndverkere","Prosess/maskin","Renhold/hjelp"];
    return `STYRK · ${grp[life.styrk1]}`;
  }
  if (life.occKind === "student") return "Under utdanning";
  if (life.occKind === "disabled") return "Trygd";
  if (life.occKind === "pensioner") return "Pensjonist";
  if (life.occKind === "kid") return "Lek";
  if (life.occKind === "teen") return "Ungdomsskole/vgs";
  return "";
}
function eduPositionLabel(code) {
  if (code <= 2) return "Grunnskolenivå";
  if (code <= 4) return "Videregående nivå";
  if (code <= 5) return "Fagskole / påbygg";
  if (code === 6) return "UH kort";
  if (code === 7) return "UH lang";
  if (code === 8) return "Forskerutdanning";
  return "Uoppgitt";
}
function incomePositionLabel(nok) {
  if (nok < 200000) return "D1 · under fattigdomsgrensen";
  if (nok < 280000) return "D2 · lav";
  if (nok < 340000) return "D3 · under medianen";
  if (nok < 410000) return "D4 · rett under medianen";
  if (nok < 480000) return "D5 · midt på treet";
  if (nok < 560000) return "D6 · over medianen";
  if (nok < 660000) return "D7 · over snittet";
  if (nok < 810000) return "D8 · godt over snittet";
  if (nok < 1100000) return "D9 · høyt";
  if (nok < 2000000) return "Topp 10 %";
  return "Topp 1 %";
}

function lifeId(life) {
  // 6-char "fingerprint" for the card
  const s = `${life.name}-${life.age}-${life.municipality}-${life.occupation}-${life.drawn_at}`;
  let h = 0;
  for (let i = 0; i < s.length; i++) { h = ((h << 5) - h + s.charCodeAt(i)) | 0; }
  return Math.abs(h).toString(36).slice(0, 6).toUpperCase();
}

function renderCard(targetId) {
  const root = document.getElementById(targetId || "card");
  const life = STATE.current;
  if (!life) return;

  const dimsHtml = DIM_KEYS.filter(d => dimAppliesAt(d.key, life.age)).map(d => {
    const v = dimValueFor(d.key, life);
    const locked = STATE.locks[lockKeyFor(d.key)] != null;
    // Skjul reroll-knappen for felter som ikke er meningsfullt-låsbare i sampleren.
    const lockable = lockKeyFor(d.key) && !["wealth","field"].includes(lockKeyFor(d.key));
    return `
      <div class="dim ${locked ? 'locked' : ''}" data-dim="${d.key}">
        <div class="dim-label">${d.label}</div>
        <div class="dim-value">${escapeHtml(v.value)}</div>
        ${v.meta ? `<div class="dim-meta">${escapeHtml(v.meta)}</div>` : ''}
        <div class="dim-actions">
          ${lockable ? `<button class="dim-btn ${locked ? 'is-locked' : ''}" data-act="lock" title="${locked ? 'Lås opp' : 'Lås feltet'}" aria-label="${locked ? 'Lås opp' : 'Lås feltet'}">
            ${locked
              ? `<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="7" width="10" height="7" rx="1"/><path d="M5 7V5a3 3 0 0 1 6 0v2"/></svg>`
              : `<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="7" width="10" height="7" rx="1"/><path d="M5 7V5a3 3 0 0 1 5.6-1.5"/></svg>`}
          </button>` : ''}
          <button class="dim-btn" data-act="reroll" title="Trekk dette feltet på nytt" aria-label="Trekk dette feltet på nytt">
            <svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><path d="M3 8a5 5 0 0 1 8.5-3.5L13 6"/><path d="M13 2.5V6h-3.5"/><path d="M13 8a5 5 0 0 1-8.5 3.5L3 10"/><path d="M3 13.5V10h3.5"/></svg>
          </button>
        </div>
      </div>`;
  }).join("");

  const init = (life.name[0] || "?").toUpperCase();
  const id = lifeId(life);

  const m = life.parents.mother;
  const f = life.parents.father;
  const motherYears = m.deathYear ? `(f. ${m.birthYear}, d. ${m.deathYear})` : `(f. ${m.birthYear})`;
  const fatherYears = f.deathYear ? `(f. ${f.birthYear}, d. ${f.deathYear})` : `(f. ${f.birthYear})`;

  // build a passport-style MRZ line from the life (kun fornavn — R-pakken trekker ikke etternavn)
  const mrz = (() => {
    const norm = s => (s || "").toUpperCase().replace(/Æ/g,"AE").replace(/Ø/g,"OE").replace(/Å/g,"AA").replace(/[^A-Z]/g,"<");
    const given = norm(life.name);
    const line1 = ("P<NOR" + given + "<<<<").padEnd(44, "<").slice(0,44);
    const yob = String((2026 - life.age)).slice(-2);
    const line2 = (id + "<NOR" + yob + "0101" + (life.gender === "F" ? "F" : "M") + "31041" + "<".repeat(14) + "0").padEnd(44, "<").slice(0,44);
    return [line1, line2];
  })();

  root.innerHTML = `
    <div class="passport-cover-band"></div>
    <div class="card-header">
      <div class="card-header-left">
        <svg class="riksvapen" viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="1.4">
          <path d="M4 22 L4 14 L9 18 L12 10 L16 16 L20 10 L23 18 L28 14 L28 22 Z" fill="currentColor" fill-opacity="0.18"/>
          <circle cx="4" cy="13" r="1.4" fill="currentColor"/>
          <circle cx="12" cy="9" r="1.4" fill="currentColor"/>
          <circle cx="20" cy="9" r="1.4" fill="currentColor"/>
          <circle cx="28" cy="13" r="1.4" fill="currentColor"/>
        </svg>
        <div>
          <h2 class="doc-title">KONGERIKET NORGE</h2>
          <div class="doc-sub">AlternaLiv · Statistisk borgerskap</div>
        </div>
      </div>
      <div class="doc-id">
        <div>NR. ${id}</div>
        <div>Type / Type · P</div>
        <div>Trukket · ${new Date(life.drawn_at).toLocaleString("nb-NO", {dateStyle:"short"})}</div>
      </div>
    </div>

    <div class="card-hero">
      <div class="portrait-wrap">
        <div class="portrait" data-init="${escapeHtml((life.name || "").trim().charAt(0).toUpperCase())}"></div>
      </div>
      <div class="hero-fields">
        <div class="hero-eyebrow">Fornavn / Given names</div>
        <h1 class="hero-name">${escapeHtml(life.name)}</h1>
        <div class="hero-sub" style="margin-top:14px;">${life.age} år · ${escapeHtml(life.municipality)} · ${life.gender === "F" ? "Kvinne / F" : "Mann / M"}</div>
      </div>
    <div class="stamp">
      <svg viewBox="0 0 150 150" xmlns="http://www.w3.org/2000/svg">
        <defs>
          <path id="topArc-${id}" d="M 15 75 a 60 60 0 0 1 120 0" />
          <path id="botArc-${id}" d="M 18 78 a 57 57 0 0 0 114 0" />
        </defs>
        <!-- outer ring with intentional small gap (worn stamp feel) -->
        <circle class="ring ring-outer" cx="75" cy="75" r="68" stroke-dasharray="420 8 0" />
        <circle class="ring ring-inner" cx="75" cy="75" r="58" />
        <!-- top arc: KONGERIKET NORGE -->
        <text class="arc-text">
          <textPath href="#topArc-${id}" startOffset="50%" text-anchor="middle">KONGERIKET ★ NORGE</textPath>
        </text>
        <!-- bottom arc: COUNTERFACT -->
        <text class="arc-text">
          <textPath href="#botArc-${id}" startOffset="50%" text-anchor="middle">ALTERNALIV</textPath>
        </text>
        <!-- crown -->
        <g class="crown" transform="translate(75 50)">
          <path d="M -16 4 L -16 -6 L -10 -2 L -6 -10 L 0 -4 L 6 -10 L 10 -2 L 16 -6 L 16 4 Z" fill="currentColor" fill-opacity="0.18" stroke-width="1.2"/>
          <circle cx="-16" cy="-7" r="1.5"/>
          <circle cx="-6" cy="-11" r="1.5"/>
          <circle cx="6" cy="-11" r="1.5"/>
          <circle cx="16" cy="-7" r="1.5"/>
          <line x1="-16" y1="6" x2="16" y2="6" stroke="currentColor" stroke-width="0.8"/>
        </g>
        <!-- date in center -->
        <text class="date" x="75" y="85">${(() => {
          const d = new Date(life.drawn_at);
          return String(d.getDate()).padStart(2,'0') + '.' + String(d.getMonth()+1).padStart(2,'0') + '.' + String(d.getFullYear()).slice(-2);
        })()}</text>
        <text class="sub" x="75" y="100">TRUKKET</text>
      </svg>
    </div>
    </div>

    <div class="dims">
      ${dimsHtml}
    </div>

    <div class="parents">
      <div class="parents-title">Foreldre</div>
      <div class="parent-grid">
        <div>
          <div><span class="parent-name">${escapeHtml(m.name)}</span><span class="parent-years">${motherYears}</span></div>
          <div class="parent-line">${escapeHtml(m.occupation)}</div>
          <div class="parent-line" style="color: var(--ink-3);">${escapeHtml(m.education)}</div>
        </div>
        <div>
          <div><span class="parent-name">${escapeHtml(f.name)}</span><span class="parent-years">${fatherYears}</span></div>
          <div class="parent-line">${escapeHtml(f.occupation)}</div>
          <div class="parent-line" style="color: var(--ink-3);">${escapeHtml(f.education)}</div>
        </div>
      </div>
    </div>

    <div class="mrz">
      <div>${mrz[0].split("<").join('<span class="chev">&lt;</span>')}</div>
      <div>${mrz[1].split("<").join('<span class="chev">&lt;</span>')}</div>
    </div>

  `;

  // Wire dim interactions only on the main card
  if (!targetId) {
    root.querySelectorAll(".dim").forEach(el => {
      const dim = el.dataset.dim;
      el.addEventListener("click", e => {
        if (e.target.closest(".dim-btn")) return;
        showDistribution(dim);
      });
      const lockBtn = el.querySelector('[data-act="lock"]');
      if (lockBtn) lockBtn.addEventListener("click", e => {
        e.stopPropagation();
        toggleLock(dim);
      });
      el.querySelector('[data-act="reroll"]').addEventListener("click", e => {
        e.stopPropagation();
        rerollExcept(dim);
      });
    });
  }
}
function escapeHtml(s) {
  if (s == null) return "";
  return String(s).replace(/[&<>"']/g, c => ({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"})[c]);
}

// ---- Lock / reroll ----
function lockKeyFor(dim) {
  return ({ name:"name", age:"age", municipality:"municipality", occupation:"occupation", education:"education", field:"field", income:"income_nok", household:"household", marital:"marital", religion:"religion", party:"party", wealth:"wealth" })[dim];
}
function toggleLock(dim) {
  const k = lockKeyFor(dim);
  if (!k) return;
  if (STATE.locks[k] != null) {
    delete STATE.locks[k];
  } else {
    const life = STATE.current;
    if (k === "income_nok") STATE.locks[k] = life.income_nok;
    else if (k === "wealth" || k === "field") {
      // these aren't directly lockable in sampler — skip gracefully
      return;
    }
    else STATE.locks[k] = life[k];
  }
  renderCard();
}
// App-dim → R-reroll-dim (kun dimensjoner R kan om-trekke betinget).
const R_REROLL_DIM = {
  name: 'name', age: 'age', municipality: 'municipality',
  occupation: 'occupation', education: 'education', income: 'income',
  household: 'household', marital: 'marital_status',
  religion: 'religion', party: 'party'
};
// Låse-nøkkel (STATE.locks) → intern R-pin-nøkkel, slik at brukerens harde
// låser også pinnes når de ligger nedstrøms av måldimensjonen.
const R_PIN_FOR_LOCK = {
  name: 'name', age: 'age', municipality: 'mun', occupation: 'occ',
  education: 'edu', field: 'nus', income_nok: 'inc', household: 'household',
  marital: 'ms', religion: 'rel', party: 'party', wealth: 'wealth'
};

async function rerollExcept(dim) {
  const life = STATE.current;
  // Foretrukket rute: betinget om-trekning i R-pakken. Oppstrøms beholdes
  // eksakt; målet + dets nedstrøms-avhengige trekkes på nytt betinget.
  const rDim = R_REROLL_DIM[dim];
  if (RBridge.ready && rDim && life && life._fromR && life._ctxId != null &&
      window.CFM_R && window.CFM_R.rerollLife) {
    try {
      const keep = Object.keys(STATE.locks || {})
        .map(k => R_PIN_FOR_LOCK[k]).filter(Boolean);
      const raw = await window.CFM_R.rerollLife(rDim, life._ctxId, keep);
      const newLife = window.translateRLife(raw);
      newLife._fromR = true;
      STATE.current = newLife;
      renderCard();
      renderProbability();
      renderWhy();
      prepareStoryForTypewriter();
      return;
    } catch (e) {
      console.warn('[CFM] R-reroll feilet — faller tilbake til JS-låsesti', e);
    }
  }
  // Fallback (R utilgjengelig / ukjent dim): lås alt unntatt `dim` i JS-sampleren.
  const newLocks = {};
  DIM_KEYS.forEach(d => {
    if (d.key === dim) return;
    const k = lockKeyFor(d.key);
    if (!k || k === "wealth" || k === "field") return;
    if (k === "income_nok") newLocks[k] = life.income_nok;
    else newLocks[k] = life[k];
  });
  // preserve user-set hard locks too
  const oldLocks = STATE.locks;
  STATE.locks = newLocks;
  await drawAndRender();
  STATE.locks = oldLocks;
  renderCard();
}

// ---- Probability ----
function renderProbability() {
  const sec = document.getElementById("prob-section");
  if (!STATE.showProbability) { sec.style.display = "none"; return; }
  sec.style.display = "";
  const matchCount = CFM.probabilityScore(STATE.current);
  const num = document.getElementById("prob-num");
  const note = document.getElementById("prob-note");
  function fmt(n) { return n.toLocaleString("nb-NO").replace(/,/g, " "); }
  let display, label;
  if (matchCount >= 100000) { display = `~${fmt(Math.round(matchCount/1000)*1000)}`; label = "nordmenn passer denne grovskissen"; }
  else if (matchCount >= 10000) { display = `~${fmt(Math.round(matchCount/100)*100)}`; label = "nordmenn passer denne grovskissen"; }
  else if (matchCount >= 1000) { display = `~${fmt(Math.round(matchCount/10)*10)}`; label = "nordmenn passer denne grovskissen"; }
  else if (matchCount >= 100) { display = `~${fmt(matchCount)}`; label = "nordmenn passer denne grovskissen"; }
  else if (matchCount >= 10) { display = `~${fmt(matchCount)}`; label = "personer — sjelden kombinasjon"; }
  else { display = `<10`; label = "personer — meget sjeldent"; }
  num.textContent = display;
  note.textContent = label;
}

// ---- Saved ----
// (renderSaved declared further down with filter support)
function persist() {
  try { localStorage.setItem("cfm-saved", JSON.stringify(STATE.saved)); } catch (e) {}
}
function restore() {
  try {
    const s = localStorage.getItem("cfm-saved");
    if (s) STATE.saved = JSON.parse(s) || [];
  } catch (e) {}
}

// ---- Distributions ----
function showDistribution(dim) {
  const overlay = document.getElementById("dist");
  const title = document.getElementById("dist-title");
  const sub = document.getElementById("dist-sub");
  const bars = document.getElementById("dist-bars");

  const life = STATE.current;
  let data = []; // { lbl, pct, hi }
  let titleText = "", subText = "";

  switch (dim) {
    case "education": {
      const w = []; // mimic conditional weights at this age
      const ws = (function(){
        const f = window.CFM_evalEdu;
        if (f) return f(life.age);
        return null;
      })();
      const tbl = CFM_DATA.educationNo;
      const rawW = (function(){
        // inline call — copy of eduBandWeights via simple re-derivation
        const a = life.age;
        const w = new Array(10).fill(0);
        if (a <= 5) { w[0]=1; }
        else if (a <= 12) { w[1]=0.95; w[0]=0.05; }
        else if (a <= 15) { w[2]=0.90; w[1]=0.08; w[0]=0.02; }
        else if (a <= 19) { w[2]=0.55; w[3]=0.30; w[4]=0.10; w[5]=0.02; w[6]=0.02; w[9]=0.01; }
        else if (a <= 24) { w[2]=0.10; w[3]=0.10; w[4]=0.45; w[5]=0.05; w[6]=0.20; w[7]=0.07; w[9]=0.03; }
        else if (a <= 29) { w[2]=0.10; w[4]=0.30; w[5]=0.06; w[6]=0.30; w[7]=0.20; w[8]=0.01; w[9]=0.03; }
        else if (a <= 39) { w[2]=0.13; w[3]=0.05; w[4]=0.30; w[5]=0.05; w[6]=0.25; w[7]=0.18; w[8]=0.02; w[9]=0.02; }
        else if (a <= 49) { w[2]=0.18; w[3]=0.07; w[4]=0.28; w[5]=0.05; w[6]=0.20; w[7]=0.18; w[8]=0.02; w[9]=0.02; }
        else if (a <= 59) { w[1]=0.02; w[2]=0.22; w[3]=0.10; w[4]=0.27; w[5]=0.05; w[6]=0.18; w[7]=0.12; w[8]=0.02; w[9]=0.02; }
        else if (a <= 66) { w[1]=0.04; w[2]=0.28; w[3]=0.12; w[4]=0.25; w[5]=0.04; w[6]=0.14; w[7]=0.10; w[8]=0.01; w[9]=0.02; }
        else { w[1]=0.10; w[2]=0.40; w[3]=0.10; w[4]=0.18; w[5]=0.03; w[6]=0.10; w[7]=0.06; w[8]=0.01; w[9]=0.02; }
        return w;
      })();
      const total = rawW.reduce((a,b) => a+b, 0) || 1;
      data = tbl.map(e => ({ lbl: e.label, pct: rawW[e.code]/total, hi: e.label === life.education }));
      titleText = "Utdanning";
      subText = `Fordeling for ${life.age}-åringer · SSB 08921`;
      break;
    }
    case "marital": {
      // crude: same w-array used in sampler, evaluated for life.age
      const a = life.age;
      let w;
      if (a < 18) w = [1,0,0,0,0,0,0];
      else if (a <= 19) w = [0.95,0.001,0,0,0,0,0.05];
      else if (a <= 22) w = [0.80,0.03,0,0.001,0.001,0.001,0.17];
      else if (a <= 24) w = [0.75,0.05,0.001,0.005,0.002,0.002,0.19];
      else if (a <= 34) w = [0.35,0.25,0.002,0.03,0.01,0.005,0.35];
      else if (a <= 49) w = [0.15,0.40,0.005,0.08,0.02,0.005,0.25];
      else if (a <= 64) w = [0.08,0.50,0.03,0.15,0.02,0.005,0.12];
      else if (a <= 74) w = [0.05,0.45,0.10,0.12,0.01,0.005,0.08];
      else w = [0.05,0.35,0.25,0.10,0.01,0.005,0.03];
      const total = w.reduce((a,b)=>a+b,0)||1;
      data = CFM_DATA.maritalStatus.map((m,i) => ({ lbl: m.label, pct: w[i]/total, hi: m.label === life.marital }));
      titleText = "Sivilstand";
      subText = `Fordeling for ${life.age}-åringer · SSB`;
      break;
    }
    case "household": {
      const tbl = CFM_DATA.households;
      data = tbl.map(h => ({ lbl: h.label, pct: h.share, hi: h.label === life.household }));
      titleText = "Husholdning"; subText = "Befolkningsandel · SSB";
      break;
    }
    case "religion": {
      const tbl = CFM_DATA.religions;
      data = tbl.map(r => ({ lbl: r.label, pct: r.share, hi: r.label === life.religion }));
      titleText = "Trosamfunn"; subText = "Befolkningsandel"; break;
    }
    case "party": {
      const tbl = CFM_DATA.parties;
      data = tbl.map(p => ({ lbl: p.label, pct: p.share, hi: p.label === life.party }));
      titleText = "Partipreferanse"; subText = "Nasjonal baseline 2025"; break;
    }
    case "municipality": {
      const top = [...CFM_DATA.municipalities].sort((a,b)=>b.pop-a.pop).slice(0, 12);
      const totalPop = 5627400;
      data = top.map(m => ({ lbl: `${m.name} (${m.county})`, pct: m.pop/totalPop, hi: m.name === life.municipality }));
      if (!data.find(d => d.hi)) {
        const cur = CFM_DATA.municipalities.find(m => m.name === life.municipality);
        if (cur) data.push({ lbl: `${cur.name} (${cur.county})`, pct: cur.pop/totalPop, hi: true });
      }
      titleText = "Bosted"; subText = "Topp 12 kommuner + ditt valg · SSB 07459"; break;
    }
    case "age": {
      data = CFM_DATA.ageBands.map(b => ({ lbl: `${b[0]}–${b[1]} år`, pct: b[2], hi: life.age >= b[0] && life.age <= b[1] }));
      titleText = "Alder"; subText = "Befolkningsandel · SSB 07459"; break;
    }
    case "income": {
      data = CFM_DATA.incomeDeciles.map(d => ({ lbl: d.label, pct: 0.10, hi: life.income_nok >= d.lo && life.income_nok < d.hi }));
      titleText = "Inntekt"; subText = "Inntekts­desiler · 10 % per desil"; break;
    }
    case "occupation": {
      const top = [...CFM_DATA.occupations].sort((a,b)=>b.hc-a.hc).slice(0, 12);
      const totalHc = CFM_DATA.occupations.reduce((s,o) => s+o.hc, 0);
      data = top.map(o => ({ lbl: o.label, pct: o.hc/totalHc, hi: o.label === life.occupation }));
      if (!data.find(d => d.hi)) {
        const cur = CFM_DATA.occupations.find(o => o.label === life.occupation);
        if (cur) data.push({ lbl: cur.label, pct: cur.hc/totalHc, hi: true });
      }
      titleText = "Yrke"; subText = "Topp 12 yrker (av kuratet utvalg) · SSB 11418"; break;
    }
    case "wealth": {
      const tbl = CFM_DATA.wealthClasses;
      data = tbl.map(c => ({ lbl: c.label, pct: c.share, hi: c.label === life.wealth_class }));
      titleText = "Nettoformue"; subText = "Klasse­fordeling · SSB 12558"; break;
    }
    case "origin": {
      // Grov nasjonal andel: ~80 % majoritet, ~14 % første-generasjon, ~6 % andre-generasjon
      const o = life.origin || {};
      const cur = o.kind === "norsk" ? "Norsk" : (o.generation === 2 ? "Andre­gen." : "Første­gen.");
      data = [
        { lbl: "Norsk (begge foreldre)", pct: 0.80, hi: cur === "Norsk" },
        { lbl: "Førstegenerasjons­innvandrer", pct: 0.14, hi: cur === "Første­gen." },
        { lbl: "Andregenerasjons­innvandrer", pct: 0.06, hi: cur === "Andre­gen." }
      ];
      titleText = "Bakgrunn"; subText = "Befolkningsandel · SSB 05182"; break;
    }
    case "housing": {
      // Eier/leier-andel etter alder — fra D.ownerByAge i datasettet
      const band = (CFM_DATA.ownerByAge || []).find(b => life.age >= b[0] && life.age <= b[1]);
      const pOwn = band ? band[2] : 0.75;
      const status = life.housing?.status || "—";
      data = [
        { lbl: `Eier · ${life.age <= 30 ? 'unge' : life.age <= 50 ? 'voksne' : life.age <= 70 ? 'eldre voksne' : 'eldre'}`,
          pct: pOwn, hi: status === "Eier" },
        { lbl: "Leier eller annet",
          pct: 1 - pOwn, hi: status === "Leier" || status === "Bor hjemme" }
      ];
      titleText = "Bolig"; subText = `Eier­andel for ${life.age}-åringer · SSB 06265`; break;
    }
    case "children": {
      const n = life.n_children;
      const total = [0.18, 0.22, 0.30, 0.18, 0.08, 0.04]; // 0,1,2,3,4,5+ ferdige fødselsforløp
      const labels = ["Ingen barn", "1 barn", "2 barn", "3 barn", "4 barn", "5+ barn"];
      data = labels.map((lbl, i) => ({
        lbl, pct: total[i],
        hi: (n == null && i === 0) || n === i || (i === 5 && n >= 5)
      }));
      titleText = "Antall barn"; subText = "Kvinner 45+, antall fødte · SSB 04231"; break;
    }
    case "siblings": {
      const c = life.siblings?.count ?? 0;
      const total = [0.16, 0.40, 0.30, 0.10, 0.04];
      const labels = ["Enebarn", "1 søsken", "2 søsken", "3 søsken", "4+ søsken"];
      data = labels.map((lbl, i) => ({
        lbl, pct: total[i],
        hi: c === i || (i === 4 && c >= 4)
      }));
      titleText = "Søsken"; subText = "Antall biologiske søsken · SSB"; break;
    }
    case "hytte": {
      const hasHytte = !!life.hytte;
      data = [
        { lbl: "Eier hytte / fritidsbolig", pct: 0.27, hi: hasHytte },
        { lbl: "Ingen hytte",               pct: 0.73, hi: !hasHytte }
      ];
      titleText = "Hytte"; subText = "Husholdninger med fritidsbolig · SSB 06205"; break;
    }
    case "health": {
      // Selvrapportert helse fra D.selfRatedHealth-bånd
      function bandLabel(a) {
        if (a < 16) return "0-15"; if (a < 25) return "16-24";
        if (a < 45) return "25-44"; if (a < 65) return "45-64";
        if (a < 80) return "65-79"; return "80+";
      }
      const row = (CFM_DATA.selfRatedHealth || []).find(r => r.band === bandLabel(life.age))
                || (CFM_DATA.selfRatedHealth || [])[0] || { probs: [0.5,0.35,0.10,0.04,0.01] };
      const labels = ["Meget god", "God", "Så som så", "Dårlig", "Meget dårlig"];
      const cur = life.health?.selfRated;
      data = labels.map((lbl, i) => ({
        lbl, pct: row.probs[i] || 0,
        hi: cur === lbl
      }));
      titleText = "Helse"; subText = `Selvrapportert helse for ${life.age}-åringer · FHI`; break;
    }
    case "bourdieu": {
      // Grov klasse­fordeling — speiler tabellen Hjellbrekke & Korsnes
      const cur = life.bourdieu?.klasse;
      data = [
        { lbl: "Etablert overklasse",        pct: 0.06, hi: cur === "Etablert overklasse" },
        { lbl: "Økonomisk elite",            pct: 0.05, hi: cur === "Økonomisk elite" },
        { lbl: "Kulturell elite",            pct: 0.05, hi: cur === "Kulturell elite" },
        { lbl: "Etablert middelklasse",      pct: 0.18, hi: cur === "Etablert middelklasse" },
        { lbl: "Kulturell middelklasse",     pct: 0.14, hi: cur === "Kulturell middelklasse" },
        { lbl: "Økonomisk middelklasse",     pct: 0.10, hi: cur === "Økonomisk middelklasse" },
        { lbl: "Faglært arbeiderklasse",     pct: 0.18, hi: cur === "Faglært arbeiderklasse" },
        { lbl: "Tradisjonell arbeiderklasse",pct: 0.16, hi: cur === "Tradisjonell arbeiderklasse" },
        { lbl: "Prekariat",                  pct: 0.08, hi: cur === "Prekariat" }
      ];
      titleText = "Klasseposisjon"; subText = "Norsk klassekart · Hjellbrekke & Korsnes (2012)"; break;
    }
    case "name": case "field": default: {
      titleText = ({name:"Navn", field:"Studieretning"})[dim] || "Fordeling";
      subText = "Trekkes betinget · ingen enkel marginalfordeling å vise";
      data = [{ lbl: life[dim] || "—", pct: 1, hi: true }];
    }
  }

  title.textContent = titleText;
  sub.textContent = subText;

  const max = Math.max(...data.map(d => d.pct), 0.01);
  bars.innerHTML = data.map(d => `
    <div class="dist-row ${d.hi ? 'hi' : ''}">
      <div>
        <div class="lbl">${escapeHtml(d.lbl)}</div>
        <div class="bar-wrap"><div class="bar" style="width: ${(d.pct/max*100).toFixed(1)}%"></div></div>
      </div>
      <div class="pct">${(d.pct*100).toFixed(d.pct < 0.01 ? 2 : 1)} %</div>
    </div>
  `).join("");

  overlay.classList.add("open");
}
document.getElementById("dist-close").onclick = () => document.getElementById("dist").classList.remove("open");
document.getElementById("dist").addEventListener("click", e => {
  if (e.target.id === "dist") e.currentTarget.classList.remove("open");
});

// ---- Compare ----
document.getElementById("btn-compare").onclick = () => {
  if (STATE.saved.length === 0) {
    alert("Lagre minst to liv først for å sammenligne.");
    return;
  }
  const wrap = document.getElementById("compare-cards");
  // cap to 6
  const lives = STATE.saved.slice(-6);
  wrap.innerHTML = lives.map((_, i) => `<article class="card" id="cmp-${i}"></article>`).join("");
  lives.forEach((life, i) => {
    const prev = STATE.current;
    STATE.current = life;
    renderCard(`cmp-${i}`);
    STATE.current = prev;
  });
  document.getElementById("compare").classList.add("open");
  const checked = document.getElementById('diff-toggle').checked;
  wrap.classList.toggle('diff', checked);
  applyDiffMarks();
};
document.getElementById("btn-compare-close").onclick = () => document.getElementById("compare").classList.remove("open");

// ---- Save ----
document.getElementById("btn-save").onclick = () => {
  if (!STATE.current) return;
  STATE.saved.push(JSON.parse(JSON.stringify(STATE.current)));
  if (STATE.saved.length > 60) STATE.saved.shift();
  persist();
  renderSaved();
  toast('Lagret');
};
document.getElementById("btn-share").onclick = () => {
  if (!STATE.current) return;
  const url = shareUrlFor(STATE.current);
  navigator.clipboard.writeText(url).then(
    () => toast('Lenke kopiert'),
    () => { prompt('Kopier lenken manuelt:', url); }
  );
};
document.getElementById("why-toggle").onclick = () => {
  const c = document.getElementById('why-card');
  c.classList.toggle('open');
  document.getElementById('why-toggle').textContent = c.classList.contains('open') ? 'Skjul' : 'Vis brytning';
};
document.getElementById("saved-search").addEventListener('input', e => {
  STATE.savedFilter = e.target.value.toLowerCase().trim();
  renderSaved();
});
document.getElementById("diff-toggle").addEventListener('change', e => {
  document.getElementById('compare-cards').classList.toggle('diff', e.target.checked);
  applyDiffMarks();
});
document.getElementById("btn-draw").onclick = () => {
  STATE.userHasDrawn = true;
  // Behold låser på tvers av trekninger — bruker må aktivt låse opp via ⌖.
  drawAndRender();
};

// ---- Tweaks panel ----
const tweakDefaults = /*EDITMODE-BEGIN*/{
  "showProbability": true,
  "showSources": true
}/*EDITMODE-END*/;

function TweaksApp() {
  const { useTweaks, TweaksPanel, TweakSection, TweakSelect, TweakToggle } = window;
  const [vals, setTweak] = useTweaks(tweakDefaults);

  React.useEffect(() => {
    STATE.showProbability = vals.showProbability;
    STATE.showSources = vals.showSources;
    document.getElementById("sources-section").style.display = vals.showSources ? "" : "none";
    drawAndRender();
  }, [vals.showProbability, vals.showSources]);

  return (
    <TweaksPanel title="Tweaks">
      <TweakSection title="Visning">
        <TweakToggle label="Vis sannsynlighet" value={vals.showProbability} onChange={v => setTweak("showProbability", v)} />
        <TweakToggle label="Vis SSB-kilder" value={vals.showSources} onChange={v => setTweak("showSources", v)} />
      </TweakSection>
    </TweaksPanel>
  );
}

const tweaksMount = document.createElement("div");
document.body.appendChild(tweaksMount);
ReactDOM.createRoot(tweaksMount).render(<TweaksApp />);

// ---- Why this life? ----
function renderWhy() {
  const life = STATE.current;
  if (!life) return;
  const D = CFM_DATA;
  const POP = 5627400;
  const rows = [];

  const ageBand = D.ageBands.find(b => life.age >= b[0] && life.age <= b[1]);
  const pAge = ageBand ? ageBand[2] : 0.05;
  rows.push({ lbl:'Alder', desc:`${life.age} år (${ageBand ? ageBand[0]+'–'+ageBand[1] : 'ukjent'} band)`, pct: pAge });

  const countyPop = D.municipalities.filter(m => m.county === life.county).reduce((s,m)=>s+m.pop, 0);
  const pCounty = countyPop > 0 ? countyPop/POP : 0.05;
  rows.push({ lbl:'Fylke', desc:`Bor i ${life.county}`, pct: pCounty });

  const eduObj = D.educationNo.find(e => e.label === life.education);
  const pEdu = eduObj && eduObj.share ? Math.max(0.04, eduObj.share) : 0.10;
  rows.push({ lbl:'Utdanning', desc: life.education, pct: pEdu });

  let pOcc = 0.15, occDesc = life.occupation;
  if (life.occKind === 'kid' || life.occKind === 'teen' || life.occKind === 'student') { pOcc = 0.5; occDesc = `${life.occupation} (skole/under utd.)`; }
  else if (life.occKind === 'pensioner') { pOcc = 0.7; occDesc = `${life.occupation} (pensjonist)`; }
  else if (life.occKind === 'disabled') { pOcc = 0.5; occDesc = `${life.occupation} (uføre)`; }
  else if (life.styrk1) {
    const sameStyrk = D.occupations.filter(o => o.styrk1 === life.styrk1).reduce((s,o)=>s+(o.hc||0), 0);
    pOcc = Math.max(0.04, sameStyrk/2700000);
  }
  rows.push({ lbl:'Yrke', desc: occDesc, pct: pOcc });

  // gender
  const pGender = life.gender === 'F' ? 0.495 : 0.505;
  rows.push({ lbl:'Kjønn', desc: life.gender === 'F' ? 'Kvinne' : 'Mann', pct: pGender });

  const body = document.getElementById('why-body');
  const joint = rows.reduce((acc, r) => acc * r.pct, 1);
  const matchCount = Math.max(1, Math.round(POP * joint));

  body.innerHTML = rows.map(r => `
    <div class="why-row">
      <div class="lbl">${escapeHtml(r.lbl)}</div>
      <div class="desc">${escapeHtml(r.desc)}</div>
      <div class="pct">${(r.pct*100).toFixed(r.pct < 0.01 ? 2 : 1)} %</div>
    </div>
  `).join('') + `
    <div class="why-joint">
      Antar du at disse fem var uavhengige, ville ca. <strong>${matchCount.toLocaleString('nb-NO').replace(/,/g,' ')}</strong> av Norges 5,6 millioner passe profilen. I virkeligheten korrelerer de — utdanning, yrke og formue henger sammen både i egen biografi og på tvers av generasjoner.
    </div>
  `;
}

// ---- Share URL ----
function encodeLife(life) {
  const json = JSON.stringify(life);
  // utf-8 safe base64
  const bytes = new TextEncoder().encode(json);
  let bin = '';
  for (const b of bytes) bin += String.fromCharCode(b);
  return btoa(bin).replace(/\+/g,'-').replace(/\//g,'_').replace(/=+$/,'');
}
function decodeLife(s) {
  try {
    let b = s.replace(/-/g,'+').replace(/_/g,'/');
    while (b.length % 4) b += '=';
    const bin = atob(b);
    const bytes = new Uint8Array(bin.length);
    for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
    return JSON.parse(new TextDecoder().decode(bytes));
  } catch (e) { return null; }
}
function shareUrlFor(life) {
  const enc = encodeLife(life);
  const url = new URL(window.location.href);
  url.hash = 'l=' + enc;
  return url.toString();
}
function loadFromHash() {
  const m = (location.hash || '').match(/[#&]l=([^&]+)/);
  if (!m) return false;
  const life = decodeLife(m[1]);
  if (!life || !life.name) return false;
  STATE.current = life;
  renderCard();
  renderProbability();
  renderWhy();
  renderStory();
  toast('Liv åpnet fra lenke');
  return true;
}

// ---- Story (template-based + optional AI) ----
function composeStoryTemplate(life) {
  const name = `<span class="accent">${escapeHtml(life.name)}</span>`;
  const age = life.age;
  const she = life.gender === 'F' ? 'Hun' : 'Han';
  const sheLow = life.gender === 'F' ? 'hun' : 'han';
  const place = escapeHtml(life.municipality);
  const county = escapeHtml(life.county);
  const occLow = (life.occupation || '').toLowerCase();

  // --- Sentence 1: who/where ---
  let s1;
  if (age <= 4) s1 = `${name} er ${age} år og bor med foreldrene sine i ${place}.`;
  else if (age <= 9) s1 = `${name} (${age}) går på barneskolen i ${place}.`;
  else if (age <= 12) s1 = `${name} er ${age} og bor i ${place}, der ${sheLow} går på barneskolen.`;
  else if (age <= 15) s1 = `${name} (${age}) går på ungdomsskolen i ${place}.`;
  else if (age <= 18) s1 = `${name} (${age}) går videregående i ${place}.`;
  else if (life.occKind === 'student') {
    const fld = life.field ? life.field.toLowerCase() : 'høyere utdanning';
    s1 = `${name} er ${age} år og studerer ${escapeHtml(fld)} i ${place}.`;
  }
  else if (life.occKind === 'pensioner') s1 = `${name}, ${age} år, er pensjonist i ${place}.`;
  else if (life.occKind === 'disabled') s1 = `${name} (${age}) er uføretrygdet og bor i ${place}.`;
  else if (life.occKind === 'worker') s1 = `${name} er ${age} og jobber som ${escapeHtml(occLow)} i ${place}.`;
  else s1 = `${name}, ${age} år, bor i ${place}.`;

  // --- Setning 1b: innvandrerbakgrunn (håndterer både R- og JS-form) ---
  let sOrigin = '';
  const og = life.origin;
  if (og && og.kind && og.kind !== 'norsk') {
    const cname = typeof og.country === 'string'
      ? og.country
      : (og.country && (og.country.name || og.country.label)) || null;
    const gen2 = og.generation === 2 || og.kind === 'second_gen' || og.kind === 'andregen';
    if (gen2) {
      sOrigin = cname
        ? `${she} er født i Norge, med foreldre fra ${escapeHtml(cname)}.`
        : `${she} er født i Norge av innvandrede foreldre.`;
    } else if (cname) {
      const yrs = og.years_in_norway;
      sOrigin = `${she} kom til Norge fra ${escapeHtml(cname)}${yrs ? ` for ${yrs} år siden` : ''}.`;
    }
  }

  // --- Sentence 2: household + marital ---
  // SSB household labels rewritten to fit naturally after "bor".
  const HH_AFTER_BOR = {
    'aleneboende': 'alene',
    'par uten barn': 'med partneren sin',
    'par med barn 0–5 år': 'med partner og småbarn',
    'par med barn 6–17 år': 'med partner og barn i skolealder',
    'par med voksne barn': 'med partner og voksne barn fortsatt hjemme',
    'enslig med barn 0–17 år': 'alene med barn',
    'enslig med voksne barn': 'alene med voksne barn',
    'flerfamiliehusholdning': 'i en flerfamiliehusholdning',
    'bofellesskap': 'i bofellesskap',
    'annen husholdning': 'i en sammensatt husholdning',
    'bor hos foreldre': 'hjemme hos foreldrene sine'
  };
  const hh = (life.household || '').toLowerCase();
  const mar = (life.marital || '').toLowerCase();
  const hhPhrase = HH_AFTER_BOR[hh] || (hh ? 'i ' + hh : '');
  let s2 = '';
  // Skip household sentence for children — it's always "bor hos foreldre".
  if (age < 18) {
    s2 = '';
  } else if (mar.startsWith('enke')) {
    s2 = `${she} ble ${life.gender === 'F' ? 'enke' : 'enkemann'} for noen år siden${hhPhrase ? ', og bor ' + hhPhrase : ''}.`;
  } else if (mar.startsWith('skilt') || mar.startsWith('separert')) {
    s2 = `${she} er ${escapeHtml(mar)}${hhPhrase ? ', og bor ' + hhPhrase : ''}.`;
  } else if (mar.startsWith('ugift') && age >= 35 && hhPhrase === 'alene') {
    s2 = `${she} er ugift og bor alene.`;
  } else if (hhPhrase) {
    s2 = `${she} bor ${hhPhrase}.`;
  }

  // --- Sentence 3: economy / housing / parents ---
  let s3 = '';
  if (age >= 19 && age <= 67 && life.occKind !== 'student') {
    const inc = life.income_nok || 0;
    const incLabel = inc < 200000 ? 'under fattigdomsgrensen' :
                     inc < 350000 ? 'i lavinntektsgruppen' :
                     inc < 480000 ? 'midt på inntektsstigen' :
                     inc < 660000 ? 'over medianen' :
                     inc < 1100000 ? 'godt over snittet' : 'i topp 10 %';
    const h = life.housing || {};
    if (h.status === 'Eier' && h.purchaseYear) {
      s3 = `Kjøpte bolig i ${h.purchaseYear} for ${(h.purchasePrice/1e6).toFixed(1)} mill., verdt ${(h.currentValue/1e6).toFixed(1)} mill. i dag — og tjener ${incLabel}.`;
    } else if (h.status === 'Leier') {
      const rent = (h.rent || 0).toLocaleString('nb-NO').replace(/,/g, ' ');
      s3 = `Leier ${h.sqm ? h.sqm + ' kvm ' : ''}i ${county} til ${rent} kr i måneden, og tjener ${incLabel}.`;
    } else {
      s3 = `Inntekten ligger ${incLabel}.`;
    }
  } else if (age <= 18 && life.parents) {
    const m = life.parents.mother, f = life.parents.father;
    s3 = `Mor er ${escapeHtml((m.occupation||'').toLowerCase())}, far er ${escapeHtml((f.occupation||'').toLowerCase())}.`;
  } else if (age >= 68) {
    const h = life.housing;
    if (h && h.status === 'Eier' && h.currentValue) {
      s3 = `Etter et langt arbeidsliv eier ${sheLow} en bolig verdt ${(h.currentValue/1e6).toFixed(1)} mill. i dag.`;
    } else {
      s3 = `Lever av pensjonen sin og holder seg nær${life.gender === 'F' ? '' : 'e'} ${place}.`;
    }
  }

  // --- Sentence 4: optional flavour from v0.8.4 dimensions ---
  let s4 = '';
  const flavour = [];
  if (life.hytte) {
    flavour.push(`Eier en ${life.hytte.typeLabel.toLowerCase()} verdt ${(life.hytte.value/1e6).toFixed(1)} mill.`);
  }
  if (life.n_children > 0 && age >= 22) {
    const n = life.n_children;
    flavour.push(`Har ${n === 1 ? 'ett barn' : n + ' barn'}.`);
  }
  if (age <= 21 && life.siblings && life.siblings.count > 0) {
    const n = life.siblings.count;
    const sib = (life.siblings.siblings || [])[0];
    if (n === 1 && sib && sib.name) {
      flavour.push(`Har ${sib.gender === 'F' ? 'en søster' : 'en bror'}, ${escapeHtml(sib.name)}.`);
    } else {
      flavour.push(`Har ${n} søsken.`);
    }
  }
  if (life.health && life.health.chronic && age >= 25) {
    flavour.push(`Lever med ${life.health.chronicType.toLowerCase()}.`);
  }
  if (flavour.length > 0) {
    // Pick at most two flavour notes
    const picks = flavour.slice(0, 2);
    s4 = picks.join(' ');
  }

  // --- Avsnitt 2: utdanning, livsstil, klasseprofil ---
  // Stabilt valg per liv (ikke per render) — enkel hash av navn+alder+inntekt.
  const storySeed = ((life.name || '').length * 31 + (age || 0) * 7 +
                     ((life.income_nok || 0) % 97)) >>> 0;

  // Utdanning (voksne, ikke studenter)
  let sEdu = '';
  if (age >= 23 && life.occKind !== 'student' && life.education) {
    const fld = life.field && life.field !== 'NA' ? String(life.field).toLowerCase() : null;
    sEdu = `Utdanningsløpet endte med ${escapeHtml(life.education.toLowerCase())}${fld ? ', innen ' + escapeHtml(fld) : ''}.`;
  }

  // --- Livsstil som flytende prosa ---
  // De rå SSB-verdiene er frekvenser/mønstre («Ukentlig», «Helger»). Vi
  // oversetter dem til naturlige verbfraser, slik at avsnittet leses som en
  // fortelling om hvordan personen lever — ikke som utfylte skjemafelt.
  const joinNo = (arr) => {
    if (arr.length === 0) return '';
    if (arr.length === 1) return arr[0];
    return arr.slice(0, -1).join(', ') + ' og ' + arr[arr.length - 1];
  };
  const lc = (v) => String(v).toLowerCase();
  const PODCAST = {
    'sjelden': 'hører en podkast i ny og ne',
    'av og til': 'hører på podkast av og til',
    'ukentlig': 'lytter til podkast hver uke',
    'daglig': 'har stort sett en podkast gående'
  };
  const SOCIAL = {
    'mest scroller': 'scroller gjerne på sosiale medier',
    'poster jevnlig': 'poster jevnlig på sosiale medier',
    'skapt innhold (vlog/blog)': 'lager eget innhold på nett'
  };
  const ALCOHOL = {
    'avholdsmann': 'er avholds',
    'sjelden (kun ved spesielle anledninger)': 'tar et glass bare ved spesielle anledninger',
    'helger': 'tar gjerne en øl eller to i helgene',
    'flere ganger i uka': 'tar et glass vin et par ganger i uka',
    'daglig (1-2 glass)': 'koser seg med et glass eller to om dagen'
  };
  const DIET = {
    'tradisjonell norsk (kjøtt-tungt)': 'spiser tradisjonell, kjøtt-tung husmannskost',
    'balansert': 'holder et balansert kosthold',
    'fleksitarianer (mest planter med litt kjøtt)': 'er fleksitarianer — mest grønt, litt kjøtt',
    'vegetarianer': 'lever vegetarisk',
    'vegan': 'spiser helt vegansk',
    'lavkarbo / keto': 'kjører lavkarbo',
    'glutenfri': 'holder seg glutenfri'
  };
  const paperPhrase = (v) => {
    const s = lc(v);
    if (/^ingen/.test(s)) return 'leser ingen avis';
    if (s === 'lokalavis') return 'holder lokalavisa';
    return `holder ${escapeHtml(v)}`;
  };

  let sLife = '';
  {
    const m = life.media || {};
    // 1) Fritid: hobbyer + ett mediefritids-element (podkast / sosiale medier / TV)
    const leisureMedia = [];
    if (m.podcast && PODCAST[lc(m.podcast)]) leisureMedia.push(PODCAST[lc(m.podcast)]);
    if (m.social && SOCIAL[lc(m.social)]) leisureMedia.push(SOCIAL[lc(m.social)]);
    if (m.tvHours != null && age >= 16 && m.tvHours >= 3.2) leisureMedia.push('ser mye på TV om kvelden');
    const leisurePick = leisureMedia.length ? leisureMedia[storySeed % leisureMedia.length] : null;

    let leisure = '';
    if (life.hobbies && life.hobbies.length > 0) {
      const hb = life.hobbies.slice(0, 2).map(h => escapeHtml(lc(h)));
      leisure = `På fritiden går det i ${joinNo(hb)}`;
      leisure += leisurePick ? `, og ${sheLow} ${leisurePick}.` : '.';
    } else if (leisurePick) {
      leisure = `${she} ${leisurePick}.`;
    }

    // 2) Hverdagsvaner: avis + kosthold + alkohol vevd til én setning
    const verbs = [];
    if (m.paper) verbs.push(paperPhrase(m.paper));
    if (life.diet && DIET[lc(life.diet)]) verbs.push(DIET[lc(life.diet)]);
    if (life.alcohol && age >= 18 && ALCOHOL[lc(life.alcohol)]) verbs.push(ALCOHOL[lc(life.alcohol)]);
    const habits = verbs.length ? `${she} ${joinNo(verbs)}.` : '';

    sLife = [leisure, habits].filter(Boolean).join(' ');
  }

  // Klasseprofil (Bourdieu) — som setning, ikke etikett
  let sClass = '';
  if (age >= 25 && life.bourdieu && life.bourdieu.klasse) {
    sClass = `På klassekartet plasserer ${sheLow} seg i ${escapeHtml(lc(life.bourdieu.klasse))}.`;
  }

  const p1 = [s1, sOrigin, s2, s3].filter(Boolean).join(' ');
  const p2 = [sEdu, s4, sLife, sClass].filter(Boolean).join(' ');
  return p2 ? `${p1}<br><br>${p2}` : p1;
}

function rand01() { return Math.random(); }

// ---- Typewriter ----
let _typewriterTimer = null;
function prepareStoryForTypewriter() {
  const el = document.getElementById('story-text');
  if (!el) return;
  if (_typewriterTimer) { clearTimeout(_typewriterTimer); _typewriterTimer = null; }
  el.classList.remove('loading');
  el.classList.add('typing');
  el.innerHTML = '<span class="tw-cursor"></span>';
}
function typewriterHTML(el, html, opts = {}) {
  const tickMs = opts.tickMs ?? 30;
  const charsPerTick = opts.charsPerTick ?? 1;
  const delMs = opts.delMs ?? Math.round((opts.tickMs ?? 30) * 0.55);
  if (_typewriterTimer) { clearTimeout(_typewriterTimer); _typewriterTimer = null; }
  // Build target DOM in a detached node, then walk text nodes.
  const tmp = document.createElement('div');
  tmp.innerHTML = html;
  // Capture text targets in order, paired with their parent nodes in the live tree.
  // We'll move tmp's children into el (empty its content first) and progressively
  // reveal text. To avoid layout jumping, we keep all elements in place and just
  // mask the text by replacing text-node values with their truncated versions.
  el.innerHTML = '';
  while (tmp.firstChild) el.appendChild(tmp.firstChild);
  // Append cursor
  const cursor = document.createElement('span');
  cursor.className = 'tw-cursor';
  el.appendChild(cursor);
  // Collect all text nodes that are NOT inside the cursor span, and expand each
  // into a small op-script. A node whose parent carries [data-alt] gets a false
  // start: the alternative is typed, held, deleted backwards, then the real
  // text is typed — the life that could have been, then the one that was.
  const ops = [];
  const walker = document.createTreeWalker(el, NodeFilter.SHOW_TEXT, {
    acceptNode(n) {
      return n.parentNode === cursor ? NodeFilter.FILTER_REJECT : NodeFilter.FILTER_ACCEPT;
    }
  });
  let n;
  while ((n = walker.nextNode())) {
    const full = n.nodeValue;
    n.nodeValue = '';
    const host = (n.parentNode && n.parentNode.nodeType === 1) ? n.parentNode : null;
    const alt = host && host.getAttribute ? host.getAttribute('data-alt') : null;
    if (alt) {
      ops.push({ node: n, kind: 'type', text: alt, ghost: host });
      ops.push({ node: n, kind: 'hold', ms: Math.max(500, tickMs * 18) });
      ops.push({ node: n, kind: 'del', ghost: host });
      ops.push({ node: n, kind: 'hold', ms: Math.max(200, tickMs * 6) });
      ops.push({ node: n, kind: 'type', text: full });
    } else {
      ops.push({ node: n, kind: 'type', text: full });
    }
  }
  let i = 0, j = 0;
  function tick() {
    if (i >= ops.length) {
      // Done — keep cursor blinking briefly then drop.
      _typewriterTimer = setTimeout(() => {
        el.classList.remove('typing');
        cursor.remove();
      }, 900);
      return;
    }
    const op = ops[i];
    if (op.kind === 'hold') {
      i++; j = 0;
      _typewriterTimer = setTimeout(tick, op.ms);
      return;
    }
    if (op.kind === 'del') {
      const cur = op.node.nodeValue;
      if (cur.length > 0) {
        op.node.nodeValue = cur.slice(0, -1);
        _typewriterTimer = setTimeout(tick, delMs);
      } else {
        if (op.ghost) op.ghost.classList.remove('tw-ghost');
        i++; j = 0;
        _typewriterTimer = setTimeout(tick, tickMs);
      }
      return;
    }
    const full = op.text;
    if (j === 0 && op.ghost) op.ghost.classList.add('tw-ghost');
    if (j < full.length) {
      const next = Math.min(full.length, j + charsPerTick);
      op.node.nodeValue = full.slice(0, next);
      const ch = full[next - 1];
      j = next;
      let delay = tickMs;
      if (/[.!?]/.test(ch)) delay = tickMs * 20;        // sentence end
      else if (/[,;:—–]/.test(ch)) delay = tickMs * 8;  // clause pause
      else if (ch === ' ') delay = tickMs * 1.5;
      _typewriterTimer = setTimeout(tick, delay);
    } else {
      i++; j = 0;
      _typewriterTimer = setTimeout(tick, tickMs);
    }
  }
  tick();
}

function renderStory() {
  const life = STATE.current;
  if (!life) return;
  const el = document.getElementById('story-text');
  if (!el) return;
  // Foretrukket: R-pakkens egen narrate_life()-biografi (0.9.16). Den er
  // skrevet som flytende prosa over flere avsnitt — bruk den direkte.
  // Fall tilbake til JS-malen kun når R ikke ga oss en narrasjon.
  let html;
  if (life._fromR && typeof life.narrative === 'string' && life.narrative.trim()) {
    html = formatRNarrative(life.narrative, life.name);
  } else {
    html = composeStoryTemplate(life);
  }
  html = injectFalseStarts(html, life);
  typewriterHTML(el, html, { tickMs: 26, charsPerTick: 1 });
}

// --- "Det kunne blitt sånn" ---------------------------------------------
// Velger 1–2 konkrete verdier i den ferdige narrasjonen og markerer dem med
// et alternativ fra samme fordeling. Skrivemaskinen skriver alternativet
// først, sletter det bakover, og skriver så det som faktisk ble.
function pickOther(list, notThis) {
  const pool = list.filter(v => v && v !== notThis);
  if (pool.length === 0) return null;
  return pool[Math.floor(Math.random() * pool.length)];
}
function falseStartCandidates(life) {
  const D = window.CFM_DATA || {};
  const out = [];
  const push = (value, alt) => {
    if (value && alt && String(value).length > 2) {
      out.push({ value: String(value), alt: String(alt) });
    }
  };
  if (life.occupation && Array.isArray(D.occupations)) {
    push(life.occupation, pickOther(D.occupations.map(o => o.label), life.occupation));
  }
  if (life.municipality && Array.isArray(D.municipalities)) {
    push(life.municipality, pickOther(D.municipalities.map(m => m.name), life.municipality));
  }
  if (life.education && Array.isArray(D.educationNo)) {
    push(life.education, pickOther(D.educationNo.map(e => e.label), life.education));
  }
  if (life.party) {
    push(life.party, altForLabelOrHumor(life.party, D.parties, D.partyHumor));
  }
  if (life.religion) {
    push(life.religion, altForLabelOrHumor(life.religion, D.religions, D.religionHumor));
  }
  return out;
}
// Parti/religion kan komme som en humor-etikett fra pakken ("Stemmer H — …")
// i stedet for et rent navn. Alternativet må være av samme slag, ellers skriver
// maskinen et bart fragment ("Rødt.") som en hel setning. Vi slår opp i de
// faktiske puljene i stedet for å gjette på lengde.
function flattenHumor(humorMap) {
  const pool = [];
  if (!humorMap) return pool;
  for (const k of Object.keys(humorMap)) {
    const arr = humorMap[k];
    if (Array.isArray(arr)) pool.push(...arr);
  }
  return pool;
}
function altForLabelOrHumor(value, labelList, humorMap) {
  const plain = Array.isArray(labelList) ? labelList.map(x => x.label) : [];
  const humor = flattenHumor(humorMap);
  if (humor.includes(value)) return pickOther(humor, value);
  if (plain.includes(value)) return pickOther(plain, value);
  return null; // verken kjent navn eller kjent humor-etikett — hopp over
}
function injectFalseStarts(html, life) {
  const cands = falseStartCandidates(life);
  if (cands.length === 0) return html;
  // Bland, og ta 1–2 — nok til å merkes, sjeldent nok til å ikke bli en tic.
  for (let k = cands.length - 1; k > 0; k--) {
    const m = Math.floor(Math.random() * (k + 1));
    [cands[k], cands[m]] = [cands[m], cands[k]];
  }
  // Hvor ofte maskinen ombestemmer seg: oftest ikke i det hele tatt. Da blir
  // slettingen et unntak man legger merke til, ikke en tic i hver biografi.
  const roll = Math.random();
  const want = roll < 0.55 ? 0 : (roll < 0.90 ? 1 : 2);
  if (want === 0) return html;
  let used = 0;
  for (const c of cands) {
    if (used >= want) break;
    // Bare treff utenfor eksisterende tagger, og bare første forekomst.
    const safeVal = escapeHtml(c.value);
    const idx = html.indexOf(safeVal);
    if (idx === -1) continue;
    const before = html.slice(0, idx);
    // hopp over treff inne i en tag eller et attributt
    if (before.lastIndexOf('<') > before.lastIndexOf('>')) continue;
    html = before +
      `<span class="tw-alt" data-alt="${escapeHtml(c.alt)}">${safeVal}</span>` +
      html.slice(idx + safeVal.length);
    used++;
  }
  return html;
}

// Formater R-narrasjonen: escape, marker navnet (første forekomst) med
// aksentfarge, og gjør doble linjeskift om til avsnitt.
function formatRNarrative(text, name) {
  const paras = String(text).split(/\n\s*\n/).map(p => p.trim()).filter(Boolean);
  let nameMarked = false;
  const htmlParas = paras.map(p => {
    let safe = escapeHtml(p);
    if (!nameMarked && name) {
      const safeName = escapeHtml(name);
      const idx = safe.indexOf(safeName);
      if (idx !== -1) {
        safe = safe.slice(0, idx) +
          `<span class="accent">${safeName}</span>` +
          safe.slice(idx + safeName.length);
        nameMarked = true;
      }
    }
    return safe;
  });
  return htmlParas.join('<br><br>');
}

// ---- Diff in compare ----
function applyDiffMarks() {
  const wrap = document.getElementById('compare-cards');
  if (!wrap.classList.contains('diff')) {
    wrap.querySelectorAll('.dim').forEach(d => { d.classList.remove('diff','same'); });
    return;
  }
  const cards = wrap.querySelectorAll('.card');
  if (cards.length < 2) return;
  // Use card 0 as the reference and match dims by their data-dim key
  // (so cards with different dim counts — kids vs. adults — still compare).
  const refDims = cards[0].querySelectorAll('.dim');
  refDims.forEach(refDim => {
    const key = refDim.dataset.dim;
    const refVal = refDim.querySelector('.dim-value')?.textContent;
    cards.forEach(card => {
      const d = card.querySelector(`.dim[data-dim="${key}"]`);
      if (!d) return;
      const v = d.querySelector('.dim-value')?.textContent;
      d.classList.toggle('diff', v !== refVal);
      d.classList.toggle('same', v === refVal);
    });
  });
}

// ---- Toast ----
let _toastTimer = null;
function toast(msg) {
  const el = document.getElementById('toast');
  el.textContent = msg;
  el.classList.add('show');
  if (_toastTimer) clearTimeout(_toastTimer);
  _toastTimer = setTimeout(() => el.classList.remove('show'), 1800);
}

// ---- Override saved render to filter + show count ----
STATE.savedFilter = '';
function renderSaved() {
  const list = document.getElementById('saved-list');
  const q = STATE.savedFilter || '';
  const matches = STATE.saved.map((l, idx) => ({l, idx})).filter(({l}) => {
    if (!q) return true;
    return [l.name, l.occupation, l.county, l.municipality, l.education, l.party, l.religion]
      .filter(Boolean).some(x => String(x).toLowerCase().includes(q));
  });
  if (STATE.saved.length === 0) {
    list.innerHTML = `<div class="mono" style="font-size:10px; color: var(--ink-3); padding: 8px 0; letter-spacing: 0.04em;">Ingen lagret enda. Trykk «Lagre» for å samle liv her.</div>`;
    return;
  }
  if (matches.length === 0) {
    list.innerHTML = `<div class="saved-empty-filter">Ingen treff for «${escapeHtml(q)}»</div>`;
    return;
  }
  list.innerHTML = matches.map(({l, idx}) => `
    <div class="saved-card" data-idx="${idx}">
      <button class="saved-rm" data-rm="${idx}" title="Fjern">×</button>
      <div class="saved-name">${escapeHtml(l.name)}</div>
      <div class="saved-meta">${l.age} år · ${escapeHtml(l.municipality)} · ${escapeHtml(l.frame)}</div>
    </div>
  `).join('');
  list.querySelectorAll('.saved-card').forEach(el => {
    el.addEventListener('click', e => {
      if (e.target.matches('[data-rm]')) return;
      const idx = +el.dataset.idx;
      STATE.current = STATE.saved[idx];
      renderCard();
      renderProbability();
      renderWhy();
      renderStory();
    });
    el.querySelector('[data-rm]').addEventListener('click', e => {
      e.stopPropagation();
      const idx = +el.querySelector('[data-rm]').dataset.rm;
      STATE.saved.splice(idx, 1);
      persist();
      renderSaved();
    });
  });
}

// ---- Override compare to re-apply diff after render ----
// (compare onclick now applies diff inline; no override needed)

// ---- Init ----
renderCounties();
wireFilters();
restore();
renderSaved();
if (!loadFromHash()) drawAndRender();
