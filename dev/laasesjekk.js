/* Lim inn i nettleserkonsollen paa http://localhost:4321 naar topbaren
   sier "Kilde · R-pakke". Laaser hvert felt etter tur, trekker paa nytt,
   og sjekker to ting: holdt laasen, og kom livet fra R? */
(async () => {
  const wait = ms => new Promise(r => setTimeout(r, ms));
  while (!RBridge.ready) { console.log('venter paa R...'); await wait(1000); }

  const rows = [];
  for (const d of DIM_KEYS) {
    const k = lockKeyFor(d.key);
    if (!isLockable(k)) continue;

    STATE.locks = {};
    await drawAndRender();
    if (!STATE.current || !dimAppliesAt(d.key, STATE.current.age)) continue;

    toggleLock(d.key);
    const want = STATE.locks[k];
    if (want == null) { rows.push({felt: d.label, status: 'ikke laasbar naa'}); continue; }

    let held = true, fromR = true;
    for (let i = 0; i < 3; i++) {
      await drawAndRender();
      const got = STATE.current[LOCK_VALUE_FIELD[k] || k];
      if (String(got) !== String(want)) held = false;
      if (!STATE.current._fromR) fromR = false;
    }
    rows.push({
      felt: d.label,
      laast: String(want).slice(0, 28),
      holdt: held ? 'ja' : 'NEI',
      kilde: fromR ? 'R' : 'JS-FALLBACK'
    });
    STATE.locks = {};
  }

  console.table(rows);
  const d1 = rows.filter(r => r.holdt === 'NEI').map(r => r.felt);
  const d2 = rows.filter(r => r.kilde === 'JS-FALLBACK').map(r => r.felt);
  console.log(d1.length ? 'LAAS BRAST: ' + d1.join(', ') : 'alle laaser holdt');
  console.log(d2.length ? 'FALT TIL JS: ' + d2.join(', ') : 'alle trekk kom fra R');
})();
