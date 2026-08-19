// cfm-translate.js — oversetter R-pakkens utdata til det JS-skjemaet
// app.jsx forventer. R bruker snake_case + flate felter; JS bruker
// camelCase + nestede objekter (parents, housing, health, bourdieu osv.).
//
// Eksponerer en global: window.translateRLife(rLife) → JS-life

(function () {
  // R wealth_class koder → norske labels. R-pakken bruker bracket_code fra
  // wealth_by_age.csv ("00-","01"…"10+") + topp-haler "top5"/"top1"/"top01"
  // satt i .cond_wealth ved arvflyt.
  const WEALTH_CLASS_LABELS = {
    '00-':   'Negativ formue',
    '01':    '1 – 100 000 kr',
    '02':    '100 – 200 000 kr',
    '03':    '200 – 300 000 kr',
    '04':    '300 – 400 000 kr',
    '05':    '400 – 500 000 kr',
    '07':    '500 – 750 000 kr',
    '08':    '750 000 – 1 M',
    '09':    '1 – 2 M',
    '10+':   '2 M +',
    'top5':  'Topp 5 %',
    'top1':  'Topp 1 %',
    'top01': 'Topp 0,1 %'
  };

  // Inntekt-label fra NOK (matcher data.js incomeDeciles labels)
  function incomeLabel(nok) {
    if (nok == null) return '';
    if (nok < 200000)   return 'Under 200 000 kr';
    if (nok < 280000)   return '200 – 280 000 kr';
    if (nok < 340000)   return '280 – 340 000 kr';
    if (nok < 410000)   return '340 – 410 000 kr';
    if (nok < 480000)   return '410 – 480 000 kr';
    if (nok < 560000)   return '480 – 560 000 kr';
    if (nok < 660000)   return '560 – 660 000 kr';
    if (nok < 810000)   return '660 – 810 000 kr';
    if (nok < 1100000)  return '810 000 – 1,1 M';
    return 'Over 1,1 M';
  }

  // Utled occKind fra alder + edu_code + yrkesnavn — speiler hvordan
  // app.jsx bruker dette (vis "Student", "Pensjonist", "Trygd", "Lek", ...).
  function deriveOccKind(age, occ) {
    if (age == null) return 'worker';
    if (age <= 5) return 'kid';
    if (age <= 12) return 'kid';
    if (age <= 15) return 'teen';
    if (age <= 18) return 'teen';
    if (occ === 'Student' || /^Student/i.test(occ || '')) return 'student';
    if (/^Uføretrygdet/i.test(occ || '')) return 'disabled';
    if (/^Pensjonist|^Alderspensjonist/i.test(occ || '')) return 'pensioner';
    if (age >= 67) return 'pensioner';
    return 'worker';
  }

  // R 'background' ('majority' | 'first_gen' | 'second_gen') → JS origin shape
  function deriveOrigin(rLife) {
    const bg = rLife.background;
    if (!bg || bg === 'majority') return { kind: 'norsk' };
    const generation = bg === 'second_gen' ? 2 : 1;
    return {
      kind: bg,
      country: rLife.country_background || null,
      generation,
      region: null, // R eksponerer ikke name_region på topp-nivå
      years_in_norway: rLife.years_in_norway || null
    };
  }

  // Parent: R-shape → JS-shape (camelCase)
  function translateParent(p) {
    if (!p) return null;
    return {
      name: p.name,
      gender: p.gender,
      birthYear: p.birth_year,
      deathYear: p.death_year,
      education: p.education,
      educationCode: p.education_code,
      occupation: p.occupation,
      styrk1: p.styrk_code != null ? String(p.styrk_code).charAt(0) : null,
      origin_region: p.origin_region || null
    };
  }

  function translateSiblings(rLife) {
    const count = rLife.n_siblings ?? 0;
    let siblings = rLife.siblings;
    // R can return list-of-objects, single object, or null
    if (siblings && !Array.isArray(siblings)) siblings = [siblings];
    return {
      count,
      siblings: (siblings || []).map(s => ({
        name: s.name,
        gender: s.gender,
        birthYear: s.birth_year,
        ageDelta: s.age_delta
      }))
    };
  }

  function translateHousing(rLife) {
    const t = rLife.housing_tenure;
    if (!t || t === null) return null; // mindreårige
    if (t === 'Leier') {
      // R returnerer ikke leie-detaljer (sqm/rent) — approksimer
      return {
        status: 'Leier',
        sqm: null,
        rent: null,
        label: 'Leier'
      };
    }
    // Eier
    const value = rLife.housing_value_nok;
    const price = rLife.housing_purchase_price_nok;
    const year  = rLife.housing_purchase_year;
    const sqm   = rLife.housing_area_m2;
    return {
      status: 'Eier',
      sqm,
      purchaseYear: year,
      purchasePrice: price,
      currentValue: value,
      gain: value != null && price != null ? value - price : null,
      equity: rLife.housing_equity_nok,
      debt: rLife.housing_debt_nok,
      luxury: !!rLife.housing_luxury,
      type: rLife.housing_type,
      label: value != null
        ? `Eier ${sqm || '?'} m² · kjøpt ${year} for ${(price/1e6).toFixed(1)} M, verdt ${(value/1e6).toFixed(1)} M`
        : 'Eier'
    };
  }

  function translateHytte(rLife) {
    if (!rLife.has_hytte) return null;
    return {
      typeLabel: rLife.hytte_type || 'Hytte',
      value: rLife.hytte_value_nok || 0
    };
  }

  function translateHealth(rLife) {
    if (rLife.self_rated_health == null) return { selfRated: null, chronic: false };
    return {
      selfRated: rLife.self_rated_health,
      chronic: !!rLife.has_chronic,
      chronicType: rLife.chronic_type || null
    };
  }

  function translateBourdieu(rLife) {
    // R bruker norsk ø i nøkkelen — fall back på begge skrivemåter
    const o = rLife['bourdieu_økonomisk'] ?? rLife.bourdieu_okonomisk;
    const k = rLife.bourdieu_kulturell;
    const s = rLife.bourdieu_sosial;
    const kl = rLife.bourdieu_klasse;
    if (o == null && kl == null) return null;
    return { okonomisk: o, kulturell: k, sosial: s, klasse: kl };
  }

  function translateRLife(r) {
    if (!r) return null;
    const age = r.age;
    const wealthClassRaw = r.wealth_class;
    const wealthLabel = WEALTH_CLASS_LABELS[wealthClassRaw]
      || (wealthClassRaw ? wealthClassRaw : null);

    // For barn returnerer R ofte income_nok = 0; app-en sin "kid allowance"
    // tekst genereres lokalt. R-pakken har egne morsomme "ukepenger"-labels
    // for 0-15-åringer som ligger under income_bracket — flytt over.
    const isKid = age != null && age <= 15;
    let incomeLbl;
    if (isKid && r.income_bracket && /[a-zæøå]/i.test(r.income_bracket)) {
      // R legger ofte hele kid-flavor strengen i income_bracket
      incomeLbl = r.income_bracket;
    } else {
      incomeLbl = incomeLabel(r.income_nok);
    }

    return {
      // identitet
      name: r.name,
      age,
      gender: r.gender,
      drawn_at: r.drawn_at || Date.now(),
      _ctxId: r._ctx_id != null ? r._ctx_id : null,
      narrative: (typeof r.narrative === 'string' && r.narrative.trim()) ? r.narrative : null,

      // geografi
      municipality: r.municipality,
      county: r.county,
      munPop: r.mun_pop || 0,
      sentralitet: r.sentralitet,
      sentralitet_label: r.sentralitet_label,

      // bakgrunn
      origin: deriveOrigin(r),

      // utdanning
      education: r.education,
      eduCode: r.edu_code,
      field: r.field_of_study,
      field_detail: r.field_of_study_detail,

      // arbeid
      occupation: r.occupation,
      styrk1: null, // R eksponerer ikke ego-STYRK på topp-nivå
      occKind: deriveOccKind(age, r.occupation),
      neet: !!r.neet,

      // økonomi
      income_nok: r.income_nok || 0,
      income_label: incomeLbl,
      income_kid: isKid,
      net_wealth: r.net_wealth_nok || 0,
      wealth_class: wealthLabel,
      inheritance: r.inheritance_nok || 0,
      financial_assets: r.financial_assets_nok,
      business_equity: r.business_equity_nok,
      capital_income: r.capital_income_nok,

      // bolig
      housing: translateHousing(r),
      hytte: translateHytte(r),

      // hjem & familie
      marital: r.marital_status,
      marital_code: r.marital_code,
      household: r.household,

      // tro & politikk
      religion: r.religion,
      religion_code: null,
      religion_humor: null, // R legger humor inn i label allerede
      party: r.party,
      party_code: null,
      party_humor: null,    // R legger humor inn i label allerede
      orientation: r.orientation,

      // foreldre & slekt
      parents: {
        mother: translateParent(r.mother),
        father: translateParent(r.father)
      },
      siblings: translateSiblings(r),
      grandparents: {
        mormor: translateParent(r.mormor),
        morfar: translateParent(r.morfar),
        farmor: translateParent(r.farmor),
        farfar: translateParent(r.farfar)
      },
      n_children: r.n_children || 0,

      // klassifikasjon & velferd
      bourdieu: translateBourdieu(r),
      health: translateHealth(r),
      isolation: {
        loneliness: r.loneliness,
        trust: r.trust
      },
      deprivation: {
        count: r.deprivation_count || 0,
        label: r.deprivation_label || null
      },
      hobbies: Array.isArray(r.hobbies) ? r.hobbies : (r.hobbies ? [r.hobbies] : []),

      // livsstil (0.9.3/0.9.4)
      media: {
        paper: r.media_paper || null,
        tvHours: (typeof r.media_tv_hours === 'number') ? r.media_tv_hours : null,
        podcast: r.media_podcast || null,
        social: r.media_social || null
      },
      sleep_hours: (typeof r.sleep_hours === 'number') ? r.sleep_hours : null,
      diet: r.diet || null,
      alcohol: r.alcohol_pattern || null,

      // frame (R-modus = funksjonalisme baseline, ingen strukturell variant)
      frame: 'funksjonalisme',
      conditional: !!r.conditional
    };
  }

  window.translateRLife = translateRLife;
})();
