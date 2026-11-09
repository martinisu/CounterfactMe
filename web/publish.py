#!/usr/bin/env python3
"""Publiser AlternaLiv til docs/ for GitHub Pages.

Hvorfor dette skriptet finnes
-----------------------------
docs/index.html var fram til 2026-10-15 en BUNDLET enkeltfil: hver lokal
ressurs omskrevet til en UUID, payloaden gzippet og base64-kodet inn i
__bundler/manifest-blokker. Den fila ble laget av et verktoy som ikke finnes
i repoet -- den kom tilbake som en zip fra en egen designsesjon hver gang noe
skulle oppdateres. Derfor laa sida elleve pakkeversjoner bak.

Maalingen som avgjorde saken: web/ veier 1.5 MB som den er, den bundlede
enkeltfila 2.4 MB. Bundlingen gjorde fila STOERRE -- base64 blaaser opp med en
tredjedel, og gzip tar det ikke igjen. Det eneste enkeltfila ga var at den
kunne sendes som ett vedlegg, og GitHub Pages trenger ikke det.

Saa: ingen bundling. docs/ serverer filene slik de ligger.

Hva som skjer
-------------
1. cfm-assets.js bygges paa nytt fra R/ og inst/extdata/ (build-assets.py
   globber begge, saa nye filer kommer med av seg selv).
2. Runtime-filene kopieres til docs/.
3. .nojekyll beholdes -- uten den ignorerer GitHub Pages filer med understrek.
4. Verifisering: versjon, antall R-filer og CSV-er mot pakka.

Merk at cfm-webr.js har hardkodede R_FILES- og CSV_FILES-lister. De brukes
BARE i fallback-stien som henter fra r-pkg/; naar cfm-assets.js finnes
utledes listene fra Object.keys(). Den publiserte sida treffer derfor aldri
de listene, men de er utdaterte og en felle for den som en gang skrur av
bundelen.

Kjor:  python3 web/publish.py
"""

import json
import os
import pathlib
import re
import shutil
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
WEB = ROOT / "web"
DOCS = ROOT / "docs"

# Filene nettleseren faktisk trenger. build-assets.py og publish.py er
# byggeverktoy og skal IKKE ut.
RUNTIME = [
    "index.html", "app.jsx", "tweaks-panel.jsx",
    "data.js", "sampler.js", "cfm-translate.js", "cfm-webr.js",
    "cfm-assets.js", "cfm-bootstrap.R", "cfm-reroll.R",
    # Synlighet: ikon, delingsbilde, sitemap. Uten disse i lista ville
    # metadataen i index.html pekt paa filer som ikke finnes paa sida.
    "favicon.svg", "apple-touch-icon.png", "og-image.png", "sitemap.xml",
    # IndexNow-noekkel (Bing, Yandex m.fl.). Fila maa ligge paa sida for at
    # varsler om nye/endrede sider skal godtas. Ikke hemmelig.
    "099f8c1cfe9f8b58d653db5eaf062b82.txt",
    # Google Search Console-bekreftelse. Maa bli liggende, ellers mister
    # eiendommen bekreftelsen.
    "google2eab145b52a95185.html",
]


def main():
    print("1. Bygger cfm-assets.js fra R/ og inst/extdata/")
    r = subprocess.run([sys.executable, str(WEB / "build-assets.py")],
                       capture_output=True, text=True, cwd=str(ROOT))
    if r.returncode != 0:
        print(r.stdout); print(r.stderr); sys.exit(1)
    print("   " + (r.stdout.strip().splitlines() or ["ok"])[-1])

    # sitemap.xml med lastmod = i dag. Bare en URL: alt annet er hash-lenker
    # (#l=...) som soekemotorer uansett behandler som samme side.
    import datetime
    (WEB / "sitemap.xml").write_text(
        '<?xml version="1.0" encoding="UTF-8"?>\n'
        '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n'
        '  <url>\n'
        '    <loc>https://martinisu.github.io/CounterfactMe/</loc>\n'
        f'    <lastmod>{datetime.date.today().isoformat()}</lastmod>\n'
        '  </url>\n'
        '</urlset>\n', encoding="utf-8")

    print("\n2. Kopierer runtime-filer til docs/")
    DOCS.mkdir(exist_ok=True)
    # Rydd bort den gamle bundlede fila hvis den ligger der.
    for f in RUNTIME:
        src = WEB / f
        if not src.exists():
            print(f"   MANGLER {f}"); sys.exit(1)
        shutil.copy2(src, DOCS / f)
        print(f"   {f:22} {src.stat().st_size:>9,} bytes")

    nojekyll = DOCS / ".nojekyll"
    if not nojekyll.exists():
        nojekyll.touch()
    print("   .nojekyll              ok")

    print("\n3. Verifisering")
    txt = (DOCS / "cfm-assets.js").read_text(encoding="utf-8")
    assets = json.loads(txt[txt.index("{"):txt.rindex("}") + 1])
    desc = (ROOT / "DESCRIPTION").read_text(encoding="utf-8")
    pkg_version = re.search(r"^Version:\s*(\S+)", desc, re.M).group(1)

    n_r_pkg = len([f for f in os.listdir(ROOT / "R") if f.endswith(".R")])
    n_csv_pkg = len([f for f in os.listdir(ROOT / "inst/extdata")
                     if f.endswith(".csv")])
    ok = True
    if assets["version"] != pkg_version:
        print(f"   AVVIK versjon: bundle {assets['version']} mot pakke {pkg_version}")
        ok = False
    else:
        print(f"   versjon   {pkg_version}")
    # build-assets.py hopper over 2 R-filer (llm.R, CounterfactMe-package.R)
    if len(assets["r"]) != n_r_pkg - 2:
        print(f"   AVVIK R-filer: {len(assets['r'])} i bundle mot {n_r_pkg - 2} ventet")
        ok = False
    else:
        print(f"   R-filer   {len(assets['r'])}")
    # build-assets.py hopper over SKIP_CSV (politikk-katalogen).
    n_skip_csv = 1
    if len(assets["csv"]) != n_csv_pkg - n_skip_csv:
        print(f"   AVVIK CSV: {len(assets['csv'])} i bundle mot "
              f"{n_csv_pkg - n_skip_csv} ventet")
        ok = False
    else:
        print(f"   CSV       {len(assets['csv'])}")

    # Struktur: de tre kolonnene maa ligge INNE i .stage. En overfloedig
    # </div> lukket .stage foer tid (nov. 2026): venstre kolonne sto alene,
    # og kortet havnet under. Nettleseren sier ingenting om slikt.
    from html.parser import HTMLParser
    class _Stage(HTMLParser):
        def __init__(self):
            super().__init__(); self.stack = []; self.found = {}
        def handle_starttag(self, tag, attrs):
            if tag in ("div", "aside", "main", "section"):
                cls = dict(attrs).get("class", "") or ""
                self.stack.append((tag, cls))
                for want in ("left", "center", "right"):
                    if want in cls.split() and tag in ("aside", "main"):
                        self.found[want] = any("stage" in c.split() for _, c in self.stack[:-1])
        def handle_endtag(self, tag):
            for i in range(len(self.stack) - 1, -1, -1):
                if self.stack[i][0] == tag:
                    del self.stack[i:]; break
    html_src = (DOCS / "index.html").read_text(encoding="utf-8")
    body = re.sub(r"<script\b.*?</script>|<style\b.*?</style>|<template\b.*?</template>",
                  "", html_src, flags=re.S)
    st = _Stage(); st.feed(body)
    bad_cols = [k for k in ("left", "center", "right") if not st.found.get(k)]
    if bad_cols:
        print(f"   AVVIK: kolonner utenfor .stage: {bad_cols} (ubalansert </div>?)")
        ok = False
    else:
        print("   struktur  venstre, midten og hoeyre ligger i .stage")

    # Alle filer index.html refererer til lokalt skal finnes i docs/.
    html = (DOCS / "index.html").read_text(encoding="utf-8")
    refs = set(re.findall(r'(?:src|href)="([^":#?]+\.(?:js|jsx|R|svg|png|xml))"', html))
    refs |= {u.rsplit("/", 1)[-1] for u in re.findall(
        r'content="https://martinisu\.github\.io/CounterfactMe/([^"]+)"', html)}
    missing = sorted(r for r in refs if not (DOCS / r).exists())
    if missing:
        print(f"   AVVIK: index.html peker paa filer som mangler: {missing}")
        ok = False
    else:
        print(f"   lenker   alle {len(refs)} lokale referanser finnes")

    total = sum((DOCS / f).stat().st_size for f in RUNTIME)
    print(f"   docs/ totalt {total/1048576:.1f} MB")
    print("\n" + ("Publisert." if ok else "FEIL -- se avvikene over."))
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
