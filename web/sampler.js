// CounterfactMe sampler — JS approximation of R conditional logic
// Mirrors R-package v0.9.1 (.cond_party, .cond_health, .cond_bourdieu, ...).
// Frame: structural variants modulate parents→child correlations
// Frames: "ren_sjanse" | "meritokrati" | "bourdieu" | "rawls" | "funksjonalisme"

(function() {
  const D = window.CFM_DATA;
  const REF_YEAR = 2026;

  // --- RNG with optional seed ---
  let _seed = null;
  function seedRng(s) { _seed = s ? mulberry32(s) : null; }
  function rand() { return _seed ? _seed() : Math.random(); }
  function mulberry32(a) {
    return function() {
      a |= 0; a = a + 0x6D2B79F5 | 0;
      let t = a;
      t = Math.imul(t ^ t >>> 15, t | 1);
      t ^= t + Math.imul(t ^ t >>> 7, t | 61);
      return ((t ^ t >>> 14) >>> 0) / 4294967296;
    };
  }

  function pickWeighted(items, weightFn) {
    const ws = items.map(weightFn);
    const total = ws.reduce((a,b) => a+b, 0);
    if (total <= 0) return items[Math.floor(rand() * items.length)];
    let r = rand() * total;
    for (let i = 0; i < items.length; i++) {
      r -= ws[i];
      if (r <= 0) return items[i];
    }
    return items[items.length-1];
  }
  function pickUniform(arr) { return arr[Math.floor(rand() * arr.length)]; }
  function rnorm(mu=0, sd=1) {
    const u1 = Math.max(rand(), 1e-12), u2 = rand();
    return mu + sd * Math.sqrt(-2*Math.log(u1)) * Math.cos(2*Math.PI*u2);
  }
  function runif(a, b) { return a + rand() * (b - a); }
  function rint(a, b) { return Math.floor(runif(a, b+1)); }

  // --- Age ---
  function sampleAge(minAge=0, maxAge=99) {
    const bands = D.ageBands.filter(b => b[1] >= minAge && b[0] <= maxAge);
    const band = pickWeighted(bands, b => b[2]);
    let a = rint(Math.max(band[0], minAge), Math.min(band[1], maxAge));
    return a;
  }

  // --- Name (cohort-weighted) ---
  function sampleName(gender, age) {
    const birthYear = REF_YEAR - age;
    const pool = gender === "M" ? D.namesM : D.namesF;
    const sigma = 12;
    const pick = pickWeighted(pool, p => {
      const dist = Math.abs(birthYear - p.peak);
      return p.f * Math.exp(-(dist*dist)/(2*sigma*sigma));
    });
    return pick.n;
  }

  // --- Education conditional on age ---
  function eduBandWeights(age) {
    const w = new Array(10).fill(0);
    if (age <= 5) { w[0] = 1; return w; }
    if (age <= 12) { w[1] = 0.95; w[0] = 0.05; return w; }
    if (age <= 15) { w[2] = 0.90; w[1] = 0.08; w[0] = 0.02; return w; }
    if (age <= 19) { w[2] = 0.55; w[3] = 0.30; w[4] = 0.10; w[5] = 0.02; w[6] = 0.02; w[9] = 0.01; return w; }
    if (age <= 24) { w[2] = 0.10; w[3] = 0.10; w[4] = 0.45; w[5] = 0.05; w[6] = 0.20; w[7] = 0.07; w[9] = 0.03; return w; }
    if (age <= 29) { w[2] = 0.10; w[4] = 0.30; w[5] = 0.06; w[6] = 0.30; w[7] = 0.20; w[8] = 0.01; w[9] = 0.03; return w; }
    if (age <= 39) { w[2] = 0.13; w[3] = 0.05; w[4] = 0.30; w[5] = 0.05; w[6] = 0.25; w[7] = 0.18; w[8] = 0.02; w[9] = 0.02; return w; }
    if (age <= 49) { w[2] = 0.18; w[3] = 0.07; w[4] = 0.28; w[5] = 0.05; w[6] = 0.20; w[7] = 0.18; w[8] = 0.02; w[9] = 0.02; return w; }
    if (age <= 59) { w[1] = 0.02; w[2] = 0.22; w[3] = 0.10; w[4] = 0.27; w[5] = 0.05; w[6] = 0.18; w[7] = 0.12; w[8] = 0.02; w[9] = 0.02; return w; }
    if (age <= 66) { w[1] = 0.04; w[2] = 0.28; w[3] = 0.12; w[4] = 0.25; w[5] = 0.04; w[6] = 0.14; w[7] = 0.10; w[8] = 0.01; w[9] = 0.02; return w; }
    w[1] = 0.10; w[2] = 0.40; w[3] = 0.10; w[4] = 0.18; w[5] = 0.03; w[6] = 0.10; w[7] = 0.06; w[8] = 0.01; w[9] = 0.02; return w;
  }
  function sampleEducation(age) {
    const w = eduBandWeights(age);
    const pick = pickWeighted(D.educationNo, e => w[e.code]);
    return pick;
  }

  // --- STYRK major weights by edu ---
  function eduToStyrk(edu) {
    if (edu <= 2) return [0.5,0.1,0.01,0.2,0.8,1.0,1.0,1.0,1.0,1.0];
    if (edu <= 4) return [0.8,0.3,0.05,0.6,1.0,1.0,0.9,1.0,0.9,0.6];
    if (edu === 5) return [0.6,0.5,0.2,1.0,0.8,0.6,0.4,1.0,0.9,0.2];
    if (edu === 6) return [0.3,1.0,0.6,1.0,0.6,0.3,0.1,0.2,0.2,0.05];
    if (edu === 7) return [0.2,1.0,1.0,0.5,0.3,0.1,0.03,0.05,0.05,0.02];
    return [0.05,0.6,1.0,0.2,0.1,0.02,0.02,0.02,0.02,0.01];
  }
  function bostedMult(pop) {
    if (pop < 5000) return [1.5,0.4,0.5,0.7,0.3,0.8,5.0,2.0,2.0,0.6];
    if (pop < 20000) return [1.2,0.7,0.8,0.9,0.7,1.0,2.0,1.5,1.5,0.8];
    if (pop < 50000) return [1.0,1.0,1.0,1.0,1.0,1.0,0.5,1.0,1.0,1.0];
    return [0.5,1.5,1.5,1.3,1.5,1.0,0.05,0.5,0.4,1.2];
  }

  // --- Occupation ---
  function sampleOccupation(age, eduCode, munPop, gender) {
    if (age <= 5) return {label: pickUniform(D.toddler), kind:"kid"};
    if (age <= 12) return {label: pickUniform(D.school), kind:"kid"};
    if (age <= 15) return {label: pickUniform(D.teen), kind:"teen"};
    if (age <= 18) return {label: pickUniform(D.uppersec), kind:"teen"};
    if (age >= 19 && age <= 24 && eduCode >= 5 && rand() < 0.6) {
      return {label:"Student", kind:"student", styrk1:null, med:null};
    }
    if (age >= 18 && age <= 67) {
      const dis = (age<=24?0.018: age<=34?0.045: age<=44?0.069: age<=54?0.123: age<=61?0.20: 0.268);
      if (rand() < dis) return {label:"Uføretrygdet", kind:"disabled"};
    }
    if (age >= 72 && rand() < 0.90) return {label: pickUniform(D.pensjonist), kind:"pensioner"};
    if (age >= 67 && age < 72 && rand() < 0.55) return {label: pickUniform(D.pensjonist), kind:"pensioner"};

    const eduW = eduToStyrk(eduCode);
    const bostW = bostedMult(munPop);
    const o = pickWeighted(D.occupations, occ => {
      let w = occ.hc * eduW[occ.styrk1] * bostW[occ.styrk1];
      if (gender === "F") {
        // crude female-share by STYRK major
        const fs = [0.05,0.45,0.55,0.78,0.62,0.78,0.30,0.07,0.10,0.55][occ.styrk1];
        w *= Math.max(fs, 0.02);
      } else {
        const fs = [0.05,0.45,0.55,0.78,0.62,0.78,0.30,0.07,0.10,0.55][occ.styrk1];
        w *= Math.max(1 - fs, 0.02);
      }
      return w;
    });
    return {label: o.label, kind:"worker", styrk1: o.styrk1, med: o.med};
  }

  // --- NUS field ---
  function sampleField(eduCode, styrk1) {
    if (eduCode == null || eduCode <= 2) return null;
    if (eduCode === 9) return null;
    // crude STYRK→broad NUS mapping
    const map = {
      1: [4,3,4,4,3,4,1,1,4,4],   // Ledere → økonomi/jus/admin
      2: [6,3,2,4,5,5,1,2,3,2],   // Akademiske: helse for legar, teknisk for ingeniør
      3: [6,2,5,5,3,5,4,5,5,5],   // Høyskole: helse, lærer, tekniske
      4: [4,4,4,3,4,4,4,4,4,4],   // Kontor: økonomisk-admin
      5: [6,5,8,6,8,5,5,8,8,5],   // Service/helse
      6: [7,7,7,7,7,7,7,7,7,7],   // Primær
      7: [5,5,5,5,5,5,5,5,5,5],   // Håndverk
      8: [5,5,5,5,5,8,5,5,5,8],   // Prosess/transport
      9: [8,5,5,5,5,5,5,5,5,5]
    };
    const choices = map[styrk1 || 4];
    const pick = choices[Math.floor(rand() * choices.length)];
    const f = D.nusFields.find(x => x.code === pick);
    return f ? f.label : null;
  }

  // --- Income ---
  function sampleIncome(age, eduCode, occ) {
    if (age <= 15) {
      return {nok: 0, label: kidsAllowance(age), kid: true};
    }
    if (age <= 18) {
      if (rand() < 0.7) return {nok: 0, label:"Ikke yrkesaktiv"};
      const nok = Math.round(Math.exp(rnorm(Math.log(40000), 0.6))/1000)*1000;
      return {nok: Math.max(5000, Math.min(nok, 120000)), label:"Helgejobb"};
    }
    if (occ.kind === "student") {
      const w = [3,4,2,0.5,0.1,0,0,0,0,0];
      const dec = pickWeighted(D.incomeDeciles.map((x,i)=>({...x,w:w[i]})), x=>x.w);
      const upper = Math.min(dec.hi, 300000);
      return {nok: Math.round(runif(dec.lo, upper)/1000)*1000, label: dec.label};
    }
    if (occ.kind === "disabled") {
      const w = [1,2.5,3,2,1,0.4,0.1,0.05,0.02,0.01];
      const dec = pickWeighted(D.incomeDeciles.map((x,i)=>({...x,w:w[i]})), x=>x.w);
      return {nok: Math.round(runif(dec.lo, dec.hi)/1000)*1000, label: dec.label};
    }
    if (occ.kind === "pensioner") {
      const w = [0.5,1.5,2,2.5,2,1.5,0.5,0.3,0.1,0.05];
      const dec = pickWeighted(D.incomeDeciles.map((x,i)=>({...x,w:w[i]})), x=>x.w);
      return {nok: Math.round(runif(dec.lo, dec.hi)/1000)*1000, label: dec.label};
    }
    if (occ.kind === "worker" && occ.med) {
      const annualMed = occ.med * 12;
      let ageMult;
      if (age <= 24) ageMult = runif(0.55, 0.85);
      else if (age <= 29) ageMult = runif(0.80, 1.00);
      else if (age <= 39) ageMult = runif(0.95, 1.15);
      else if (age <= 54) ageMult = runif(1.00, 1.20);
      else if (age <= 66) ageMult = runif(0.95, 1.15);
      else ageMult = runif(0.70, 1.00);
      const noise = Math.exp(rnorm(0, 0.25));
      const nok = Math.max(0, Math.round(annualMed * ageMult * noise / 1000) * 1000);
      const dec = D.incomeDeciles.find(d => nok >= d.lo && nok < d.hi) || D.incomeDeciles[D.incomeDeciles.length-1];
      return {nok, label: dec.label};
    }
    return {nok: 350000, label:"340 000 – 400 000 kr"};
  }
  function kidsAllowance(age) {
    if (age <= 2) return "Har null peiling på penger";
    if (age <= 5) return pickUniform(["Får 10 kr i uka fra bestefar","Tror en tikrone er en formue","Eier tre mynter og en blank knapp"]);
    if (age <= 9) return pickUniform(["50 kr/uke fra mormor","Klipper plenen for 200 kr","Negativ inntekt – skylder pappa 80 kr"]);
    if (age <= 12) return pickUniform(["100 kr/uke for å rydde rommet","Driver ulovlig godteributikk på skolen","Har 2 847 kr i sparegris"]);
    return pickUniform(["Sommerjobb: 3 500 kr","Avisrute: 800 kr/mnd","Babysitter: 100 kr/kveld","Skylder kompisen 200 kr"]);
  }

  // --- Marital ---
  function sampleMarital(age) {
    if (age < 18) return {code:1, label:"Ugift"};
    let w;
    if (age <= 19) w = [0.95,0.001,0,0,0,0,0.05];
    else if (age <= 22) w = [0.80,0.03,0,0.001,0.001,0.001,0.17];
    else if (age <= 24) w = [0.75,0.05,0.001,0.005,0.002,0.002,0.19];
    else if (age <= 34) w = [0.35,0.25,0.002,0.03,0.01,0.005,0.35];
    else if (age <= 49) w = [0.15,0.40,0.005,0.08,0.02,0.005,0.25];
    else if (age <= 64) w = [0.08,0.50,0.03,0.15,0.02,0.005,0.12];
    else if (age <= 74) w = [0.05,0.45,0.10,0.12,0.01,0.005,0.08];
    else w = [0.05,0.35,0.25,0.10,0.01,0.005,0.03];
    const pick = pickWeighted(D.maritalStatus.map((m,i)=>({...m,w:w[i]})), x=>x.w);
    let label = pick.label;
    // joker
    if (age >= 19 && rand() < 0.015) {
      label = age >= 67 ? pickUniform(D.jokesElder) : pickUniform(D.jokesAdult);
    }
    return {code: pick.code, label};
  }

  // --- Household ---
  function sampleHousehold(age, maritalCode) {
    if (age < 18) return "Bor hos foreldre";
    const hh = D.households;
    let w = hh.map(h => 0.01);
    function set(code, val) { w[hh.findIndex(h=>h.code===code)] = val; }
    if (age <= 24) { set(11,0.35); set(1,0.25); set(9,0.15); set(2,0.10); set(3,0.05); set(6,0.02); }
    else if (age <= 39) { set(3,0.25); set(4,0.20); set(2,0.18); set(1,0.15); set(6,0.08); set(9,0.03); }
    else if (age <= 59) { set(4,0.25); set(2,0.22); set(5,0.15); set(1,0.15); set(7,0.06); set(6,0.04); }
    else if (age <= 69) { set(2,0.35); set(1,0.25); set(5,0.15); set(7,0.05); }
    else { set(1,0.40); set(2,0.35); set(5,0.08); set(10,0.05); }
    // age caps
    hh.forEach((h,i) => {
      if (h.code===3 && (age<18||age>50)) w[i]=0;
      if (h.code===4 && (age<22||age>65)) w[i]=0;
      if (h.code===5 && age<36) w[i]=0;
      if (h.code===6 && (age<18||age>60)) w[i]=0;
      if (h.code===7 && age<36) w[i]=0;
      if (h.code===11 && age>45) w[i]=0;
    });
    if (age >= 35) { const i = hh.findIndex(h=>h.code===9); w[i]=0; }
    if (maritalCode === 2 || maritalCode === 6) {
      hh.forEach((h,i)=>{
        if (h.code===1) w[i]*=0.02;
        if (h.code===9||h.code===11||h.code===6||h.code===7) w[i]=0;
        if ([2,3,4,5].includes(h.code)) w[i]*=3;
      });
    } else if (maritalCode === 7) {
      hh.forEach((h,i)=>{
        if (h.code===1) w[i]*=0.05;
        if (h.code===9||h.code===6||h.code===7) w[i]=0;
        if ([2,3,4].includes(h.code)) w[i]*=2.5;
      });
    } else if ([3,4,5].includes(maritalCode)) {
      hh.forEach((h,i)=>{
        if (h.code===1) w[i]*=3;
        if (h.code===6||h.code===7) w[i]*=2;
        if ([2,3,4,5].includes(h.code)) w[i]*=0.02;
        if (h.code===9||h.code===11) w[i]=0;
      });
    } else if (maritalCode === 1) {
      hh.forEach((h,i)=>{
        if (h.code===1) w[i]*=2.5;
        if (h.code===9) w[i]*=2;
        if ([2,3,4,5].includes(h.code)) w[i]*=0.15;
      });
    }
    const pick = pickWeighted(hh.map((h,i)=>({...h,_w:w[i]})), x=>x._w);
    return pick.label;
  }

  // --- Municipality with optional county filter ---
  function sampleMunicipality(filterCounty) {
    let pool = D.municipalities;
    if (filterCounty && filterCounty !== "all") pool = pool.filter(m => m.county === filterCounty);
    if (pool.length === 0) pool = D.municipalities;
    return pickWeighted(pool, m => m.pop);
  }

  // --- Religion / Party (with simple frame modulation already mixed in) ---
  function sampleReligion(age, county, originRegion) {
    // If the person has immigrant background from a non-norden region,
    // draw religion conditional on origin region (CounterfactMe religion_by_region.csv)
    if (originRegion && originRegion !== "norden" && D.religionByRegion && D.religionByRegion[originRegion]) {
      const probs = D.religionByRegion[originRegion];
      const pick = pickWeighted(D.religions, r => probs[r.code] || 0.001);
      return { code: pick.code, label: pick.label };
    }
    let w = D.religions.map(r => r.share);
    if (age >= 60) { w[0] *= 1.3; w[1] *= 0.6; }
    if (age <= 35) { w[0] *= 0.7; w[1] *= 1.5; }
    if (county === "Oslo") { w[0] *= 0.7; w[3] *= 2.0; w[1] *= 1.3; }
    const pick = pickWeighted(D.religions.map((r,i)=>({...r,w:w[i]})), x=>x.w);
    return { code: pick.code, label: pick.label };
  }
  function sampleParty(age, eduCode, styrk1, county, income) {
    // R-pakken (.cond_party) returnerer NA for under 18 — speil det her.
    if (age < 18) return { code: null, label: null };
    let w = D.parties.map(p => p.share);
    // crude tilts (indices: 0=AP 1=H 2=FRP 3=SP 4=SV 5=R 6=V 7=KRF 8=MDG 9=ANDRE 10=STEMTE_IKKE)
    if (eduCode >= 6) { w[1] *= 0.9; w[6] *= 1.5; w[8] *= 1.6; w[5] *= 1.4; }
    if (eduCode <= 3) { w[2] *= 1.6; w[0] *= 1.2; w[6] *= 0.6; w[8] *= 0.5; w[10] *= 1.4; }
    if (income > 800000) { w[1] *= 1.6; w[2] *= 1.2; w[5] *= 0.5; }
    if (income < 350000) { w[5] *= 1.5; w[0] *= 1.2; w[10] *= 1.5; }
    if (county === "Oslo" || county === "Akershus") { w[1] *= 1.3; w[3] *= 0.4; }
    if (county === "Innlandet" || county === "Trøndelag" || county === "Nordland") { w[3] *= 2.5; w[0] *= 1.2; }
    if (age >= 65) { w[7] *= 1.6; w[1] *= 1.2; w[10] *= 0.6; }
    if (age <= 25) { w[10] *= 1.6; }
    const pick = pickWeighted(D.parties.map((p,i)=>({...p,w:w[i]})), x=>x.w);
    return { code: pick.code, label: pick.label };
  }

  // --- Net wealth — age + income + parents_capital aware (mirrors .cond_wealth) ---
  function sampleWealth(age, incomeNok, frame, parentsCapital) {
    if (age < 18) return {nok: 0, classLabel: null};
    const pc = parentsCapital || 0;
    const ageFactor = age <= 25 ? 0.05 : age <= 35 ? 0.4 : age <= 50 ? 1.2 : age <= 70 ? 2.5 : 1.8;
    let incFactor = Math.max(0.3, incomeNok / 500000);
    // parents_capital lifts the whole distribution (Bourdieu transfers + boligarv)
    if (pc > 25e6) incFactor *= 2.6;
    else if (pc > 10e6) incFactor *= 1.8;
    else if (pc > 5e6) incFactor *= 1.4;
    else if (pc > 2e6) incFactor *= 1.15;
    let mu = Math.log(Math.max(50000, 700000 * ageFactor * incFactor));
    let sd = 1.4;
    let nok = Math.round(Math.exp(rnorm(mu, sd)) / 1000) * 1000;
    // small chance of negative (gjeld > eiendeler) — reduced for rich-parent egos
    const negP = pc > 5e6 ? 0.05 : pc > 2e6 ? 0.10 : 0.18;
    if (rand() < negP) nok = -Math.round(rand() * 1500000 / 1000) * 1000;
    // top-tier tail (top 0.1 % ~3 % when parents_capital > 25M).
    // KREVER kapital-anker: uten parents_capital eller veldig høy inntekt
    // skal denne tail-bumpen ikke kunne dumpe titalls millioner på lavinntekt.
    let tailP = 0;
    if (pc > 25e6) tailP = 0.06;
    else if (pc > 10e6) tailP = 0.025;
    else if (pc > 5e6) tailP = 0.008;
    else if (incomeNok > 1.2e6) tailP = 0.005;  // selfmade-tail kun ved høy inntekt
    if (rand() < tailP) nok = Math.round(Math.abs(nok) * runif(10, pc > 25e6 ? 200 : 60));
    let cls = null;
    for (const c of D.wealthClasses) {
      if (nok >= c.lo && nok < c.hi) { cls = c.label; break; }
    }
    return {nok, classLabel: cls};
  }

  // --- Parents (cohort-aware, kohortbetinget yrke) ---
  function sampleParents(egoAge, egoEduCode, egoStyrk1, frame) {
    // FRAME: how strongly parent education and class are inherited from ego?
    // We work backwards: given ego's edu+occ, what are plausible parents under each frame?
    const frameInheritance = {
      ren_sjanse: 0.0,         // independent
      meritokrati: 0.15,       // weak link
      funksjonalisme: 0.30,    // moderate
      bourdieu: 0.65,          // strong reproduction
      rawls: 0.0               // no inheritance: re-roll fresh
    }[frame] || 0.30;

    const motherBirthAge = Math.round(rnorm(28, 4));
    const fatherBirthAge = Math.round(rnorm(31, 5));
    const egoBirthYear = REF_YEAR - egoAge;
    const motherBirthYear = egoBirthYear - Math.max(18, Math.min(45, motherBirthAge));
    const fatherBirthYear = egoBirthYear - Math.max(18, Math.min(48, fatherBirthAge));
    const motherAge = REF_YEAR - motherBirthYear;
    const fatherAge = REF_YEAR - fatherBirthYear;
    // Death prob
    function maybeDeath(by, gender) {
      const age = REF_YEAR - by;
      const deathProb = age < 65 ? 0.02 : age < 75 ? 0.18 : age < 85 ? 0.45 : 0.75;
      if (rand() < deathProb) {
        const minD = by + 50;
        const maxD = Math.min(REF_YEAR - 1, by + (gender==="F"? 86: 81));
        if (maxD <= minD) return null;
        return rint(minD, maxD);
      }
      return null;
    }
    function parentEdu(parentAge) {
      // kohortbetinget — older cohorts had less edu
      let baseW = eduBandWeights(parentAge);
      // inheritance: bias toward ego's edu code
      if (egoEduCode != null && frameInheritance > 0) {
        for (let c = 0; c <= 9; c++) {
          if (Math.abs(c - egoEduCode) <= 1) baseW[c] *= (1 + frameInheritance * 4);
        }
      }
      return pickWeighted(D.educationNo, e => baseW[e.code]);
    }
    function parentOccupation(parentAge, parentEdu, gender, deathYear) {
      if (parentAge >= 67 && !deathYear) {
        // pensjonist; vis siste yrke?  use a worker-ish
      }
      // women born before 1960: chance of "Hjemmeværende"
      if (gender === "F" && motherBirthYear < 1965 && rand() < 0.25) {
        return {label:"Hjemmeværende", styrk1:null, med:null};
      }
      // historical occupations bias: older men → industri / sjøfart
      let eduCode = parentEdu.code;
      const eduW = eduToStyrk(eduCode);
      // inherit STYRK
      const o = pickWeighted(D.occupations, occ => {
        let w = occ.hc * eduW[occ.styrk1];
        if (gender === "F") {
          const fs = [0.05,0.45,0.55,0.78,0.62,0.78,0.30,0.07,0.10,0.55][occ.styrk1];
          w *= Math.max(fs, 0.02);
        } else {
          const fs = [0.05,0.45,0.55,0.78,0.62,0.78,0.30,0.07,0.10,0.55][occ.styrk1];
          w *= Math.max(1-fs, 0.02);
        }
        // inheritance bias
        if (egoStyrk1 != null && frameInheritance > 0) {
          if (occ.styrk1 === egoStyrk1) w *= (1 + frameInheritance * 6);
        }
        return w;
      });
      return o;
    }
    const motherEdu = parentEdu(motherAge);
    const fatherEdu = parentEdu(fatherAge);
    const motherDeath = maybeDeath(motherBirthYear, "F");
    const fatherDeath = maybeDeath(fatherBirthYear, "M");
    const motherOcc = parentOccupation(motherAge, motherEdu, "F", motherDeath);
    const fatherOcc = parentOccupation(fatherAge, fatherEdu, "M", fatherDeath);
    return {
      mother: {
        name: sampleName("F", motherAge),
        birthYear: motherBirthYear,
        deathYear: motherDeath,
        education: motherEdu.label,
        occupation: motherOcc.label
      },
      father: {
        name: sampleName("M", fatherAge),
        birthYear: fatherBirthYear,
        deathYear: fatherDeath,
        education: fatherEdu.label,
        occupation: fatherOcc.label
      }
    };
  }

  // --- Probability score: rough "how rare is this combo" ---
  function probabilityScore(life) {
    // Simplified: estimate how many Norwegians (~5.6M) share this rough profile.
    // Uses just 4 marginals — alder × fylke × utdanning × yrkesgruppe — so the number stays grokable.
    const POP = 5627400;
    const ageBand = D.ageBands.find(b => life.age >= b[0] && life.age <= b[1]);
    const pAge = ageBand ? ageBand[2] : 0.05;
    const countyPop = D.municipalities.filter(m => m.county === life.county).reduce((s,m) => s + m.pop, 0);
    const pCounty = countyPop > 0 ? countyPop / POP : 0.05;
    const eduObj = D.educationNo.find(e => e.label === life.education);
    const pEdu = eduObj ? Math.max(0.04, eduObj.share) : 0.1;
    // occupation as broad styrk1 group, not the specific job — keeps numbers reasonable
    let pOcc = 0.15;
    if (life.occKind === "kid" || life.occKind === "teen" || life.occKind === "student") pOcc = 0.5;
    else if (life.occKind === "pensioner") pOcc = 0.7;
    else if (life.occKind === "disabled") pOcc = 0.5;
    else if (life.styrk1) {
      const sameStyrk = D.occupations.filter(o => o.styrk1 === life.styrk1).reduce((s,o) => s + (o.hc || 0), 0);
      pOcc = Math.max(0.04, sameStyrk / 2700000);
    }
    const matchCount = Math.max(1, Math.round(POP * pAge * pCounty * pEdu * pOcc));
    return matchCount;
  }

  // --- Origin (innvandrerbakgrunn: 1./2.gen + opphavsland) ---
  function sampleOrigin(age) {
    const r = rand();
    const immigrantP = D.immigrantShare || 0.20;
    if (r >= immigrantP) return { kind: "norsk", region: "norden", country: null, generation: 0, label: "Norskfødt med to norskfødte foreldre" };
    const isSecondGen = rand() < (D.secondGenShare || 0.20);
    const country = pickWeighted(D.origins, o => o.w);
    return {
      kind: isSecondGen ? "andregen" : "førstegen",
      region: country.region,
      country: country.label,
      generation: isSecondGen ? 2 : 1,
      label: isSecondGen
        ? `Andregenerasjon (${country.label})`
        : `Førstegenerasjon (${country.label})`
    };
  }

  // --- Housing: eier/leier + kjøpsår + nåverdi --------------------------------
  // Speiler .cond_housing i R-pakken v0.9.1: areal og verdi er betinget av
  // alder, inntekt OG parents_capital. Lavinntekt + lav-SES-foreldre kan altså
  // ikke lenger tilfeldigvis havne i et palass.
  function sampleHousing(age, county, incomeNok, parentsCapital) {
    if (age < 18) return { status: "Bor hjemme", label: "Bor hos foreldre" };
    const inc = incomeNok || 0;
    const pc  = parentsCapital || 0;
    // P(eier) — base × income × parents_capital  (mirrors .housing_own_prob)
    const band = (D.ownerByAge || []).find(b => age >= b[0] && age <= b[1]);
    let pOwn = band ? band[2] : 0.75;
    const incMult = inc < 150000 ? 0.40
                  : inc < 300000 ? 0.80
                  : inc < 500000 ? 1.00
                  : inc < 800000 ? 1.10 : 1.15;
    const pcMult  = pc > 10e6 ? 1.25
                  : pc >  5e6 ? 1.15
                  : pc >  2e6 ? 1.05 : 1.00;
    pOwn = pOwn * incMult * pcMult;
    if ((county === "Oslo" || county === "Akershus") && age < 35) pOwn -= 0.10;
    pOwn = Math.max(0.02, Math.min(0.97, pOwn));

    if (rand() >= pOwn) {
      const sqm = Math.max(20, Math.round(rnorm(55, 12)));
      const pricePerSqm = D.housingPricePerSqm[county] || 38000;
      const monthlyRent = Math.max(6000, Math.round((pricePerSqm * sqm * 0.0035) / 100) * 100);
      return {
        status: "Leier",
        sqm,
        rent: monthlyRent,
        label: `Leier ~${sqm} m² · ${monthlyRent.toLocaleString("nb-NO").replace(/,/g," ")} kr/mnd`
      };
    }

    // --- Eier-grenen ---
    const ageAtPurchase = Math.max(22, Math.round(rnorm(30, 6)));
    const yearsOwned = Math.max(0, Math.min(age - 22, age - ageAtPurchase));
    const purchaseYear = Math.max(1995, REF_YEAR - yearsOwned);

    // Areal: base 70 m², skaleres med inntekt + parents_capital (R: .draw_housing_area)
    let sqmMu = 70
              + (inc > 800000 ? 35 : inc > 500000 ? 20 : inc > 300000 ? 10 : inc > 150000 ? 0 : -10)
              + (pc > 25e6 ? 50 : pc > 10e6 ? 30 : pc > 5e6 ? 18 : pc > 2e6 ? 8 : 0);
    if (county === "Oslo") sqmMu *= 0.85;       // Oslo: tettere
    else if (county === "Akershus") sqmMu *= 0.95;
    let sqm = Math.max(28, Math.round(rnorm(sqmMu, 18)));

    // Luksus-trekning (1.5 % nasjonalt, høyere i Oslo/Akershus med høy pc/inntekt).
    // Speiler .luxury_draw — KAN IKKE skje uten kapital-anker.
    let luxP = 0;
    if (pc > 25e6 || inc > 1.2e6) luxP = 0.08;
    else if (pc > 10e6 || inc > 800000) luxP = 0.025;
    else if (pc > 5e6) luxP = 0.008;
    if (county === "Oslo" || county === "Akershus") luxP *= 2.5;
    const luxury = rand() < luxP;

    let pricePerSqm = D.housingPricePerSqm[county] || 38000;
    pricePerSqm *= runif(0.85, 1.15); // location-within-county støy
    if (luxury) {
      pricePerSqm *= runif(3.0, 6.0);
      sqm = Math.round(sqm * runif(1.3, 1.9));
    }

    const valueNow = Math.round(pricePerSqm * sqm / 1000) * 1000;

    // Historiske indekser
    const indices = D.housingIndex || {};
    const years = Object.keys(indices).map(Number).sort((a,b)=>a-b);
    function nearestIdx(y) {
      let best = years[0];
      for (const yy of years) { if (Math.abs(yy - y) < Math.abs(best - y)) best = yy; }
      return indices[best];
    }
    const idxThen = nearestIdx(purchaseYear);
    const idxNow  = nearestIdx(REF_YEAR);
    const valueThen = Math.round(valueNow * (idxThen / idxNow) / 1000) * 1000;
    const gain = valueNow - valueThen;

    // Hard sanity-cap: bolig > 30× årsinntekt er urealistisk uten parents_capital.
    // (R-pakkens verify.R sjekker akkurat dette forholdstallet.)
    // Hvis vi bryter mot capet, downgrad sqm — bevarer pricePerSqm-realisme.
    const maxValueByIncome = inc > 0 ? inc * (pc > 10e6 ? 80 : pc > 5e6 ? 50 : pc > 2e6 ? 35 : 25) : 4e6;
    if (valueNow > maxValueByIncome && !luxury) {
      const scale = maxValueByIncome / valueNow;
      const sqmCapped = Math.max(28, Math.round(sqm * scale));
      const valueCapped = Math.round(pricePerSqm * sqmCapped / 1000) * 1000;
      const valueThenCapped = Math.round(valueCapped * (idxThen / idxNow) / 1000) * 1000;
      return {
        status: "Eier",
        sqm: sqmCapped,
        purchaseYear,
        purchasePrice: valueThenCapped,
        currentValue: valueCapped,
        gain: valueCapped - valueThenCapped,
        equity: Math.round(estimateEquity(valueCapped, age - yearsOwned, pc, luxury)),
        label: `Eier ${sqmCapped} m² · kjøpt ${purchaseYear} for ${(valueThenCapped/1000000).toFixed(1)} M, verdt ${(valueCapped/1000000).toFixed(1)} M`
      };
    }

    return {
      status: "Eier",
      sqm,
      purchaseYear,
      purchasePrice: valueThen,
      currentValue: valueNow,
      gain,
      equity: Math.round(estimateEquity(valueNow, age - yearsOwned, pc, luxury)),
      luxury,
      label: `Eier ${sqm} m² · kjøpt ${purchaseYear} for ${(valueThen/1000000).toFixed(1)} M, verdt ${(valueNow/1000000).toFixed(1)} M`
    };
  }

  // Mirror av R-pakkens .initial_ltv + lineær amortisering over 25 år.
  function estimateEquity(valueNow, ageAtPurchase, pc, luxury) {
    let ltv = ageAtPurchase < 25 ? 0.90
            : ageAtPurchase < 30 ? 0.85
            : ageAtPurchase < 40 ? 0.78 : 0.70;
    if (pc > 10e6) ltv -= 0.20;
    else if (pc > 5e6) ltv -= 0.12;
    else if (pc > 2e6) ltv -= 0.05;
    if (luxury) ltv -= 0.10;
    ltv = Math.max(0.10, Math.min(0.95, ltv));
    return valueNow * (1 - ltv); // forenklet: ignorerer amortisering for nå
  }

  // --- Humor labels (CounterfactMe humor tables) ---
  function partyHumorLabel(code) {
    const list = (D.partyHumor && D.partyHumor[code]) || null;
    return list ? pickUniform(list) : null;
  }
  function religionHumorLabel(code) {
    const list = (D.religionHumor && D.religionHumor[code]) || null;
    return list ? pickUniform(list) : null;
  }

  // --- v0.8.4: Health, social isolation, material deprivation, n_children,
  // siblings, grandparents, hytte, Bourdieu, inheritance flow. ---

  function ageBandLabel6(age) {
    if (age < 16) return "0-15";
    if (age < 25) return "16-24";
    if (age < 45) return "25-44";
    if (age < 65) return "45-64";
    if (age < 80) return "65-79";
    return "80+";
  }

  function sampleHealth(age, eduCode) {
    if (!D.selfRatedHealth) return { selfRated: null, chronic: false };
    const band = ageBandLabel6(age);
    const srhRow = D.selfRatedHealth.find(r => r.band === band) || D.selfRatedHealth[0];
    let probs = srhRow.probs.slice();
    // For svært små barn: ingen "Dårlig"/"Meget dårlig"-selvrapportering — gir ikke mening.
    if (age < 12) {
      probs = [probs[0] + probs[3] + probs[4], probs[1], probs[2], 0, 0];
    }
    if (eduCode >= 6) probs = probs.map((p,i) => p * [1.3,1.1,0.8,0.6,0.5][i]);
    else if (eduCode <= 1) probs = probs.map((p,i) => p * [0.7,0.95,1.2,1.4,1.5][i]);
    const total = probs.reduce((a,b)=>a+b,0) || 1;
    probs = probs.map(p => p/total);
    let r = rand(), acc = 0, idx = 0;
    for (let i = 0; i < probs.length; i++) { acc += probs[i]; if (r < acc) { idx = i; break; } }
    const selfRated = D.srhLabels[idx];

    const cipRow = (D.chronicIllness || []).find(r => r.band === band) || (D.chronicIllness || [])[0];
    let pChr = cipRow ? cipRow.p : 0.2;
    if (eduCode <= 2) pChr *= 1.3;
    else if (eduCode >= 6) pChr *= 0.7;
    const chronic = rand() < Math.min(1, pChr);
    let chronicType = null;
    if (chronic) {
      // chronicTypes order: Hjerte/kar, Diabetes, Astma/KOLS, Revmatisme, Psykisk, Kreft, Annen
      // Age-appropriate weights + relabeling — speiler R-pakken v0.9.1 (.cond_health)
      // men fjerner labels som er meningsløse for små barn (KOLS, hjerte/kar, kreft-historie).
      let w, types;
      if (age < 12) {
        // Astma + allergi/eksem + atferd dominerer; ingen hjerte/kar, ingen kreft-historie.
        types = ["—","Diabetes type 1","Astma","Allergi/eksem","Atferd/oppmerksomhet","—","Annen kronisk lidelse"];
        w     = [0,    0.06,           0.55,    0.18,            0.10,                   0,   0.11];
      } else if (age < 18) {
        types = ["—","Diabetes type 1","Astma","Allergi/eksem","Psykisk lidelse","—","Annen kronisk lidelse"];
        w     = [0,    0.08,           0.40,    0.10,           0.30,             0,   0.12];
      } else if (age < 30) {
        types = D.chronicTypes;
        w     = [0.02, 0.05, 0.30, 0.10, 0.45, 0.02, 0.06];
      } else if (age < 50) {
        types = D.chronicTypes;
        w     = [0.15, 0.10, 0.20, 0.20, 0.20, 0.05, 0.10];
      } else if (age < 70) {
        types = D.chronicTypes;
        w     = [0.30, 0.15, 0.10, 0.20, 0.10, 0.10, 0.05];
      } else {
        types = D.chronicTypes;
        w     = [0.40, 0.15, 0.10, 0.15, 0.05, 0.10, 0.05];
      }
      const totW = w.reduce((a,b)=>a+b,0);
      let rr = rand() * totW, sum = 0;
      for (let i = 0; i < w.length; i++) { sum += w[i]; if (rr < sum) { chronicType = types[i]; break; } }
    }
    return { selfRated, chronic, chronicType };
  }

  function sampleSocialIsolation(age, household) {
    if (!D.socialIsolation) return { loneliness: null, trust: null };
    const band = ageBandLabel6(age);
    const row = D.socialIsolation.find(r => r.band === band) || D.socialIsolation[0];
    let probs = row.probs.slice();
    const hh = (household || "").toLowerCase();
    if (/enslig|aleneboende/.test(hh)) probs = probs.map((p,i)=>p*[1.5,1.3,0.85,0.7][i]);
    else if (/med barn/.test(hh)) probs = probs.map((p,i)=>p*[0.15,0.50,1.20,1.50][i]);
    else if (/par/.test(hh)) probs = probs.map((p,i)=>p*[0.30,0.70,1.20,1.40][i]);
    const total = probs.reduce((a,b)=>a+b,0) || 1;
    probs = probs.map(p => p/total);
    let r = rand(), acc = 0, idx = 0;
    for (let i = 0; i < probs.length; i++) { acc += probs[i]; if (r < acc) { idx = i; break; } }
    const loneliness = D.lonelinessLabels[idx];
    let trustMean = 6.7;
    if (idx === 0) trustMean -= 1.5;
    else if (idx === 1) trustMean -= 0.5;
    const trust = Math.max(0, Math.min(10, Math.round(rnorm(trustMean, 1.5))));
    return { loneliness, trust };
  }

  function sampleMaterialDeprivation(incomeNok, age, nChildren) {
    if (!D.materialDeprivation || age < 16) return { count: 0, label: null };
    // approximate income decile
    const decile = incomeNok < 200000 ? 1
                 : incomeNok < 280000 ? 2
                 : incomeNok < 340000 ? 3
                 : incomeNok < 410000 ? 4
                 : incomeNok < 480000 ? 5
                 : incomeNok < 560000 ? 6
                 : incomeNok < 660000 ? 7
                 : incomeNok < 810000 ? 8
                 : incomeNok < 1100000 ? 9 : 10;
    const row = D.materialDeprivation.find(r => r.decile === decile) || D.materialDeprivation[4];
    let probs = row.probs.slice();
    // boost deprivation count for families with many kids
    if (nChildren >= 3) probs = probs.map((p,i) => p * [0.85, 1.05, 1.20, 1.35, 1.50][i]);
    const total = probs.reduce((a,b)=>a+b,0) || 1;
    probs = probs.map(p => p/total);
    let r = rand(), acc = 0, idx = 0;
    for (let i = 0; i < probs.length; i++) { acc += probs[i]; if (r < acc) { idx = i; break; } }
    const counts = [0, 1, 2, 3, 4];
    const labels = ["Ingen mangler", "1 mangel", "2 mangler", "3 mangler", "4+ mangler"];
    return { count: counts[idx], label: labels[idx] };
  }

  function sampleNChildren(age, gender, nameRegion, background) {
    if (!D.nChildrenByCohort || age < 16) return 0;
    const birthYear = REF_YEAR - age;
    let row = D.nChildrenByCohort.find(r => birthYear >= r.min && birthYear < r.max) || D.nChildrenByCohort[D.nChildrenByCohort.length-1];
    let probs = row.probs.slice();
    // background adjustment
    if (background !== "majority" && nameRegion) {
      if (nameRegion === "mena_sor_asia" || nameRegion === "afrika_sub") {
        probs = probs.map((p,i) => p * [0.6, 0.7, 1.2, 1.4, 1.5][i]);
      } else if (nameRegion === "ost_europa") {
        probs = probs.map((p,i) => p * [0.9, 1.0, 1.1, 1.0, 0.9][i]);
      }
    }
    // age dampening
    if (age < 22) probs = probs.map((p,i) => p * [3.0, 1.2, 0.5, 0.2, 0.05][i]);
    else if (age < 26) probs = probs.map((p,i) => p * [1.5, 1.3, 0.9, 0.4, 0.2][i]);
    if (gender === "M" && age < 28) probs = probs.map((p,i) => p * [2.5, 1.0, 0.6, 0.3, 0.1][i]);
    const total = probs.reduce((a,b)=>a+b,0) || 1;
    probs = probs.map(p => p/total);
    let r = rand(), acc = 0, idx = 0;
    for (let i = 0; i < probs.length; i++) { acc += probs[i]; if (r < acc) { idx = i; break; } }
    let pick = [0,1,2,3,4][idx];
    if (pick === 4) {
      const w = [0.55, 0.25, 0.13, 0.07];
      let rr = rand(), aa = 0;
      for (let i = 0; i < w.length; i++) { aa += w[i]; if (rr < aa) { pick = 4 + i; break; } }
    }
    if (age < 17) pick = 0;
    else if (age < 19) pick = Math.min(pick, 1);
    else if (age < 22) pick = Math.min(pick, 2);
    else if (age < 25) pick = Math.min(pick, 3);
    return pick;
  }

  function sampleSiblings(motherBirthYear, nameRegion, egoAge, egoGender) {
    if (motherBirthYear == null) return { count: 0, siblings: [] };
    const mcFactor = motherBirthYear < 1940 ? 1.4
                  : motherBirthYear < 1960 ? 1.0
                  : motherBirthYear < 1980 ? 0.7 : 0.5;
    const regionFactor = {
      "mena_sor_asia": 1.4, "afrika_sub": 1.6,
      "ost_europa": 0.9, "latam_filippin": 1.2
    }[nameRegion] || 1.0;
    const baseP = [0.25, 0.45, 0.20, 0.07, 0.02, 0.01]; // 1,2,3,4,5,6+ total kids
    const ks = [1, 2, 3, 4, 5, 6.5];
    const factor = mcFactor * regionFactor;
    let w = baseP.map((p, i) => p * Math.pow(ks[i], (factor - 1) * 1.2));
    const tot = w.reduce((a,b)=>a+b,0);
    w = w.map(x => x / tot);
    let r = rand(), acc = 0, totalKids = 1;
    for (let i = 0; i < w.length; i++) { acc += w[i]; if (r < acc) { totalKids = i + 1; break; } }
    const nSiblings = Math.max(0, totalKids - 1);
    if (nSiblings === 0) return { count: 0, siblings: [] };
    const siblings = [];
    for (let i = 0; i < nSiblings; i++) {
      const rawDelta = Math.round(rnorm(0, 4));
      const delta = Math.max(-15, Math.min(egoAge, rawDelta));
      const sibAge = Math.max(0, Math.min(95, egoAge - delta));
      const sibBirthYear = REF_YEAR - sibAge;
      const sibGender = rand() < 0.5 ? "M" : "F";
      let sibName = sampleName(sibGender, sibAge);
      if (nameRegion && nameRegion !== "norden" && D.namesByRegion && D.namesByRegion[nameRegion]) {
        const pool = D.namesByRegion[nameRegion][sibGender] || D.namesByRegion[nameRegion].M;
        if (pool && pool.length) sibName = pool[Math.floor(rand() * pool.length)];
      }
      siblings.push({ name: sibName, gender: sibGender, birthYear: sibBirthYear, ageDelta: delta });
    }
    // sort eldest first
    siblings.sort((a,b) => a.birthYear - b.birthYear);
    return { count: nSiblings, siblings };
  }

  function sampleGrandparents(motherBirthYear, fatherBirthYear, motherRegion, fatherRegion) {
    function drawGp(parentBirthYear, gpGender, region) {
      const birthAge = Math.max(16, Math.min(50, Math.round(gpGender === "F" ? rnorm(27, 5) : rnorm(30, 6))));
      const gpBirthYear = parentBirthYear - birthAge;
      const impliedAge = REF_YEAR - gpBirthYear;
      const pDead = impliedAge >= 105 ? 1.0
                  : impliedAge >= 95  ? 0.97
                  : impliedAge >= 90  ? 0.85
                  : impliedAge >= 85  ? 0.65
                  : impliedAge >= 80  ? 0.45
                  : impliedAge >= 75  ? 0.25
                  : impliedAge >= 70  ? 0.15
                  : impliedAge >= 65  ? 0.05
                  : 0;
      let deathYear = null;
      if (rand() < pDead) {
        const deathAgeMax = Math.min(105, impliedAge);
        if (deathAgeMax >= 60) {
          const deathAge = Math.round(Math.max(60, Math.min(deathAgeMax, rnorm(80, 8))));
          deathYear = gpBirthYear + deathAge;
        }
      }
      let name = sampleName(gpGender, impliedAge);
      if (region && region !== "norden" && D.namesByRegion && D.namesByRegion[region]) {
        const pool = D.namesByRegion[region][gpGender] || D.namesByRegion[region].M;
        if (pool && pool.length) name = pool[Math.floor(rand() * pool.length)];
      }
      return { name, gender: gpGender, birthYear: gpBirthYear, deathYear };
    }
    if (motherBirthYear == null || fatherBirthYear == null) {
      return { mormor: null, morfar: null, farmor: null, farfar: null };
    }
    return {
      mormor: drawGp(motherBirthYear, "F", motherRegion || "norden"),
      morfar: drawGp(motherBirthYear, "M", motherRegion || "norden"),
      farmor: drawGp(fatherBirthYear, "F", fatherRegion || "norden"),
      farfar: drawGp(fatherBirthYear, "M", fatherRegion || "norden")
    };
  }

  function sampleHytte(age, incomeNok, county, parentsCapital, ownsPrimary) {
    if (age < 25) return null;
    const inc = incomeNok || 400000;
    const pc = parentsCapital || 0;
    let base = 0.17;
    if (!ownsPrimary) base *= 0.4;
    const incMult = inc < 250000 ? 0.20 : inc < 450000 ? 0.55 : inc < 700000 ? 1.05
                  : inc < 1.0e6 ? 1.85 : inc < 1.5e6 ? 2.80 : 3.60;
    const ageMult = age < 35 ? 0.35 : age < 45 ? 0.75 : age < 55 ? 1.10 : age < 70 ? 1.30 : 1.05;
    const geoMult = (county === "Oslo" || county === "Akershus") ? 1.20
                  : ["Innlandet","Buskerud","Vestfold","Telemark","Trøndelag"].includes(county) ? 1.10 : 0.95;
    const pcMult = pc > 10e6 ? 1.40 : pc > 5e6 ? 1.20 : pc > 2e6 ? 1.05 : 1.0;
    const p = Math.min(0.92, base * incMult * ageMult * geoMult * pcMult);
    if (rand() >= p) return null;
    // Type by county
    let typeProbs;
    if (county === "Oslo" || county === "Akershus") typeProbs = {fjell:0.45,innland:0.10,kyst:0.30,kyst_luksus:0.15};
    else if (["Buskerud","Innlandet","Telemark"].includes(county)) typeProbs = {fjell:0.55,innland:0.30,kyst:0.13,kyst_luksus:0.02};
    else if (["Vestfold","Agder"].includes(county)) typeProbs = {fjell:0.15,innland:0.10,kyst:0.55,kyst_luksus:0.20};
    else if (["Vestland","Rogaland","Møre og Romsdal"].includes(county)) typeProbs = {fjell:0.30,innland:0.10,kyst:0.50,kyst_luksus:0.10};
    else if (["Nordland","Troms","Finnmark"].includes(county)) typeProbs = {fjell:0.20,innland:0.30,kyst:0.45,kyst_luksus:0.05};
    else if (county === "Trøndelag") typeProbs = {fjell:0.40,innland:0.25,kyst:0.30,kyst_luksus:0.05};
    else typeProbs = {fjell:0.30,innland:0.30,kyst:0.35,kyst_luksus:0.05};
    if (inc < 700000 && pc < 5e6) typeProbs.kyst_luksus = 0;
    const entries = Object.entries(typeProbs);
    const totT = entries.reduce((s,[,v]) => s+v, 0);
    let r = rand() * totT, acc = 0, type = "fjell";
    for (const [k,v] of entries) { acc += v; if (r < acc) { type = k; break; } }
    // value
    const muMap = {fjell:Math.log(2.8e6), innland:Math.log(1.4e6), kyst:Math.log(3.5e6), kyst_luksus:Math.log(15e6)};
    const sdMap = {fjell:0.55, innland:0.55, kyst:0.65, kyst_luksus:0.85};
    let value = Math.exp(rnorm(muMap[type], sdMap[type]));
    if (inc > 1.5e6) value *= runif(1.3, 2.0);
    if (inc < 500000 && pc < 5e6) value *= runif(0.40, 0.70);
    else if (inc < 700000 && pc < 3e6) value *= runif(0.65, 0.90);
    const cap = type === "kyst_luksus" ? 150e6 : type === "kyst" ? 25e6 : 12e6;
    value = Math.min(cap, Math.round(value));
    const typeLabel = {fjell:"Fjellhytte", innland:"Innlandshytte", kyst:"Kysthytte", kyst_luksus:"Strandeiendom"}[type];
    return { type, typeLabel, value: Math.round(value) };
  }

  function sampleBourdieu(ctx) {
    // Økonomisk
    const nw = ctx.netWealth || 0;
    const ci = ctx.capitalIncome || 0;
    const he = ctx.housingEquity || 0;
    const totalEcon = nw + ci * 5 + he * 0.5;
    const econScore = totalEcon <= 0 ? 0 : Math.min(100, Math.max(0, 50 + 15 * (Math.log10(Math.max(1, totalEcon)) - 6.5)));

    // Kulturell
    const edu = ctx.eduCode != null ? ctx.eduCode : 4;
    const eduPts = Math.min(50, edu * 6);
    const parEdu = ((ctx.motherEdu ?? 3) + (ctx.fatherEdu ?? 3)) / 2;
    const parPts = Math.min(25, parEdu * 3);
    const s1 = ctx.styrk1;
    const styrkPts = s1 == null ? 8
                   : (s1 === 2 || s1 === 3) ? 25
                   : (s1 === 1 || s1 === 4) ? 18
                   : (s1 === 5) ? 10
                   : (s1 === 6 || s1 === 7 || s1 === 8) ? 8 : 12;
    const cultScore = Math.min(100, eduPts + parPts + styrkPts);

    // Sosial
    const hh = (ctx.household || "").toLowerCase();
    const hhPts = /par med barn/.test(hh) ? 30
                : /par uten/.test(hh) ? 25
                : /bofellesskap/.test(hh) ? 20
                : /enslig/.test(hh) ? 12 : 15;
    const sibPts = ctx.nSiblings == null ? 5
                 : ctx.nSiblings === 0 ? 5
                 : ctx.nSiblings <= 2 ? 12 : 18;
    const partyPts = ctx.partyCode == null ? 8
                   : ctx.partyCode === "STEMTE_IKKE" ? 3 : 20;
    const socScore = Math.min(100, hhPts + sibPts + partyPts);

    // Klasseposisjon
    let klasse;
    if (econScore >= 70 && cultScore >= 70) klasse = "Etablert overklasse";
    else if (econScore >= 75 && cultScore < 65) klasse = "Økonomisk elite";
    else if (cultScore >= 75 && econScore < 65) klasse = "Kulturell elite";
    else if (econScore >= 50 && cultScore >= 50) klasse = "Etablert middelklasse";
    else if (cultScore >= 55 && econScore >= 30) klasse = "Kulturell middelklasse";
    else if (econScore >= 55 && cultScore >= 30) klasse = "Økonomisk middelklasse";
    else if (econScore >= 30 && cultScore >= 25) klasse = "Tradisjonell arbeiderklasse";
    else if (cultScore >= 25) klasse = "Ny arbeiderklasse";
    else klasse = "Prekariat";

    return {
      okonomisk: Math.round(econScore * 10) / 10,
      kulturell: Math.round(cultScore * 10) / 10,
      sosial: Math.round(socScore * 10) / 10,
      klasse
    };
  }

  function styrk1ToCodeStr(s) {
    // map our coarse 0-9 styrk1 to a fake 2-digit string for sibling pool
    return s != null ? String(s) : null;
  }

  // --- Top-level draw ---
  function drawLife(opts = {}, locks = {}) {
    const filters = opts.filters || {};
    const frame = opts.frame || "funksjonalisme";

    let age = locks.age != null ? locks.age : sampleAge(filters.minAge ?? 0, filters.maxAge ?? 99);
    let gender = locks.gender || (filters.gender && filters.gender !== "any" ? filters.gender : (rand() < 0.5 ? "M" : "F"));
    let mun = locks.municipality ? D.municipalities.find(m => m.name === locks.municipality) || sampleMunicipality(filters.county) : sampleMunicipality(filters.county);
    let name = locks.name || sampleName(gender, age);
    let eduObj = locks.education ? D.educationNo.find(e => e.label === locks.education) || sampleEducation(age) : sampleEducation(age);
    let occ = locks.occupation
      ? (D.occupations.find(o => o.label === locks.occupation)
          ? {label: locks.occupation, kind:"worker", styrk1: D.occupations.find(o => o.label === locks.occupation).styrk1, med: D.occupations.find(o => o.label === locks.occupation).med}
          : {label: locks.occupation, kind:"worker"})
      : sampleOccupation(age, eduObj.code, mun.pop, gender);
    let field = sampleField(eduObj.code, occ.styrk1);
    let inc = sampleIncome(age, eduObj.code, occ);
    if (locks.income_nok != null) inc = {nok: locks.income_nok, label: D.incomeDeciles.find(d => locks.income_nok >= d.lo && locks.income_nok < d.hi)?.label || ""};
    let mar = locks.marital ? {code: D.maritalStatus.find(m => m.label === locks.marital)?.code || 1, label: locks.marital} : sampleMarital(age);
    let hh = locks.household || sampleHousehold(age, mar.code);
    let origin = sampleOrigin(age);
    // override name for first-gen immigrants — draw from region pool
    if (origin.generation === 1 && D.namesByRegion && D.namesByRegion[origin.region]) {
      const pool = D.namesByRegion[origin.region][gender] || D.namesByRegion[origin.region].M;
      if (pool && pool.length) name = pool[Math.floor(rand() * pool.length)];
    }
    let relObj = sampleReligion(age, mun.county, origin.region);
    if (locks.religion) relObj = { code: relObj.code, label: locks.religion };
    let partyObj = sampleParty(age, eduObj.code, occ.styrk1, mun.county, inc.nok);
    if (locks.party) partyObj = { code: partyObj.code, label: locks.party };
    // Parents drawn BEFORE wealth so parents_capital can feed into wealth distribution
    let parents = sampleParents(age, eduObj.code, occ.styrk1, frame);
    const parentsCapital = (() => {
      const mEdu = (D.educationNo.find(e => e.label === parents.mother.education) || {}).code || 3;
      const fEdu = (D.educationNo.find(e => e.label === parents.father.education) || {}).code || 3;
      const avgEdu = (mEdu + fEdu) / 2;
      let cap = 500000 + avgEdu * 600000;
      const avgBirth = (parents.mother.birthYear + parents.father.birthYear) / 2;
      const parentAvgAge = REF_YEAR - avgBirth;
      if (parentAvgAge > 70) cap *= 1.6;
      else if (parentAvgAge > 55) cap *= 1.2;
      if (["Oslo","Akershus"].includes(mun.county)) cap *= 1.3;
      cap *= Math.exp(rnorm(0, 0.35));
      return Math.round(Math.max(0, cap));
    })();
    let wealth = sampleWealth(age, inc.nok, frame, parentsCapital);
    let housing = sampleHousing(age, mun.county, inc.nok, parentsCapital);
    const partyHumor = age >= 18 ? partyHumorLabel(partyObj.code) : null;
    const religionHumor = age >= 14 ? religionHumorLabel(relObj.code) : null;

    // --- v0.8.4 dimensions ---
    // Health
    const health = sampleHealth(age, eduObj.code);
    // Social isolation
    const isolation = sampleSocialIsolation(age, hh);
    // Number of children ego has
    const nChildren = sampleNChildren(age, gender, origin.region, origin.kind === "norsk" ? "majority" : "immigrant");
    // Material deprivation
    const deprivation = sampleMaterialDeprivation(inc.nok, age, nChildren);
    // Siblings — based on mother birth year + region
    const motherRegion = origin.region; // simplification
    const siblings = sampleSiblings(parents.mother.birthYear, motherRegion, age, gender);
    // Grandparents — derived from parents' birth years
    const grandparents = sampleGrandparents(parents.mother.birthYear, parents.father.birthYear, motherRegion, motherRegion);
    // Hytte (cabin) — depends on housing/income/county/parents_capital (already computed)
    const ownsPrimary = housing && housing.status === "Eier";
    const hytte = sampleHytte(age, inc.nok, mun.county, parentsCapital, ownsPrimary);
    // Inheritance flow: if BOTH parents dead, parents_capital flows as arv
    let netWealth = wealth.nok;
    let wealthClass = wealth.classLabel;
    let inheritance = null;
    if (parents.mother.deathYear && parents.father.deathYear) {
      const sibCount = siblings.count || 0;
      const arvShare = Math.round(parentsCapital / (sibCount + 1));
      if (arvShare > 100000) {
        inheritance = arvShare;
        netWealth = (netWealth || 0) + arvShare;
        for (const c of (D.wealthClasses || [])) {
          if (netWealth >= c.lo && netWealth < c.hi) { wealthClass = c.label; break; }
        }
      }
    }
    // Bourdieu kapitalprofil
    const bourdieu = sampleBourdieu({
      netWealth,
      capitalIncome: netWealth > 500000 ? Math.round(netWealth * 0.02) : 0,
      housingEquity: ownsPrimary && housing.currentValue ? housing.currentValue * 0.6 : 0,
      eduCode: eduObj.code,
      motherEdu: (D.educationNo.find(e => e.label === parents.mother.education) || {}).code,
      fatherEdu: (D.educationNo.find(e => e.label === parents.father.education) || {}).code,
      styrk1: occ.styrk1,
      household: hh,
      nSiblings: siblings.count,
      partyCode: partyObj.code
    });

    return {
      age, gender, name,
      municipality: mun.name, county: mun.county, munPop: mun.pop,
      education: eduObj.label, eduCode: eduObj.code,
      occupation: occ.label, occKind: occ.kind, styrk1: occ.styrk1,
      field,
      income_nok: inc.nok, income_label: inc.label, income_kid: inc.kid || false,
      marital: mar.label, marital_code: mar.code,
      household: hh,
      religion: relObj.label, religion_code: relObj.code, religion_humor: religionHumor,
      party: partyObj.label, party_code: partyObj.code, party_humor: partyHumor,
      origin,
      housing,
      net_wealth: netWealth, wealth_class: wealthClass,
      inheritance,
      parents,
      // v0.8.4 dimensions
      health,
      isolation,
      n_children: nChildren,
      deprivation,
      siblings,
      grandparents,
      hytte,
      bourdieu,
      frame,
      drawn_at: Date.now()
    };
  }

  window.CFM = {
    drawLife,
    probabilityScore,
    seedRng,
    formatNok(n) {
      if (n == null) return "—";
      const sign = n < 0 ? "−" : "";
      return sign + Math.abs(n).toLocaleString("nb-NO").replace(/,/g, " ") + " kr";
    }
  };
})();
