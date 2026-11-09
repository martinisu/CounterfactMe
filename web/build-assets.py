#!/usr/bin/env python3
"""Regenerate web/cfm-assets.js from the R package in this repository.

The bundle that GitHub Pages serves is built elsewhere, but this file --
the R sources and data the browser actually runs -- comes straight from
the package, so it can be regenerated here and stays in step with
DESCRIPTION.

Run from the repository root:  python3 web/build-assets.py
"""
import json, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT  = os.path.join(ROOT, "web", "cfm-assets.js")

# llm.R is deliberately excluded: it needs a local Ollama server and
# nothing in the app calls it. cfm-bootstrap.R documents the same choice.
SKIP_R = {"llm.R", "CounterfactMe-package.R"}

# Datafiler som IKKE skal ut paa den publiserte sida. policy_parameters.csv
# er politikk-katalogen i testfase -- ingen kode leser den, men bundleren
# globber alle CSV-er og ville ellers publisert den.
SKIP_CSV = {"policy_parameters.csv"}

desc = open(os.path.join(ROOT, "DESCRIPTION"), encoding="utf-8").read()
version = re.search(r"^Version:\s*(\S+)", desc, re.M).group(1)

r_files = {}
rdir = os.path.join(ROOT, "R")
for f in sorted(os.listdir(rdir)):
    if not f.endswith(".R") or f in SKIP_R:
        continue
    r_files[f] = open(os.path.join(rdir, f), encoding="utf-8").read()

csv_files = {}
cdir = os.path.join(ROOT, "inst", "extdata")
for f in sorted(os.listdir(cdir)):
    if f.endswith(".csv") and f not in SKIP_CSV:
        csv_files[f] = open(os.path.join(cdir, f), encoding="utf-8").read()

# Manifestet maa miste de samme radene. Ellers star policy_parameters.csv
# beskrevet i SOURCES.csv paa den publiserte sida selv om fila ikke er med,
# og data_sources() i nettleseren lister en fil som ikke finnes.
if "SOURCES.csv" in csv_files:
    csv_files["SOURCES.csv"] = "".join(
        ln for ln in csv_files["SOURCES.csv"].splitlines(keepends=True)
        if ln.split(",", 1)[0] not in SKIP_CSV)

# The two glue files live in web/ and are not part of the R package.
bootstrap = open(os.path.join(ROOT, "web", "cfm-bootstrap.R"), encoding="utf-8").read()
reroll    = open(os.path.join(ROOT, "web", "cfm-reroll.R"), encoding="utf-8").read()

assets = {"version": version, "r": r_files, "csv": csv_files,
          "bootstrap": bootstrap, "reroll": reroll}

header = (f"// cfm-assets.js — GENERERT fra r-pkg/ (CounterfactMe {version}, "
          f"martinisu/CounterfactMe@main).\n")
with open(OUT, "w", encoding="utf-8") as fh:
    fh.write(header)
    fh.write("window.CFM_ASSETS = ")
    json.dump(assets, fh, ensure_ascii=False, separators=(",", ":"))
    fh.write(";\n")

print(f"wrote {OUT}")
print(f"  version {version}  |  {len(r_files)} R files  |  {len(csv_files)} CSVs")
