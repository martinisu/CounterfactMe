#!/usr/bin/env python3
"""Regenerate inst/extdata/occupation_wage_spread.csv from SSB 11418:
lower quartile, median and upper quartile of monthly wage per 4-digit
STYRK-08 occupation (full-time employees, all sectors, both sexes, 2025).

Why
---
.cond_income() drew annual income as the occupation median times a
log-normal factor with sd 0.25 for EVERY occupation. 11418's quartiles put
the actual within-occupation spread between about 0.10 (grunnskolelaerere,
renholdere) and 0.51 (toppledere); the median across occupations is 0.21.
With 0.25 everywhere, low-spread jobs got far too long a right tail: a
52-year-old "laerervikar" on 820 000 and a 25-year-old omsorgsarbeider on
888 000 were both on the live site.

    sd_log = (ln Q3 - ln Q1) / 1.349

1.349 is the interquartile range of a standard normal, so sd_log is the
log-normal sd implied by the published quartiles. Occupations where SSB
suppresses a quartile get no row; the sampler falls back to the median
sd across occupations.
"""
import csv, json, math, os, urllib.request

here = os.path.dirname(os.path.abspath(__file__))
out = os.path.join(here, "..", "inst", "extdata", "occupation_wage_spread.csv")
URL = "https://data.ssb.no/api/v0/no/table/11418"
meta = json.load(urllib.request.urlopen(URL))
y = [v for v in meta["variables"] if v["code"] == "Yrke"][0]
codes = [c for c in y["values"] if len(c) == 4 and c.isdigit()]
q = {"query": [
    {"code": "MaaleMetode", "selection": {"filter": "item", "values": ["051", "01", "061"]}},
    {"code": "Yrke", "selection": {"filter": "item", "values": codes}},
    {"code": "Sektor", "selection": {"filter": "item", "values": ["ALLE"]}},
    {"code": "Kjonn", "selection": {"filter": "item", "values": ["0"]}},
    {"code": "AvtaltVanlig", "selection": {"filter": "item", "values": ["5"]}},
    {"code": "ContentsCode", "selection": {"filter": "item", "values": ["Manedslonn"]}},
    {"code": "Tid", "selection": {"filter": "item", "values": ["2025"]}}],
    "response": {"format": "json-stat2"}}
req = urllib.request.Request(URL, data=json.dumps(q).encode(),
                             headers={"Content-Type": "application/json"})
d = json.load(urllib.request.urlopen(req))
def order(dim):
    ix = d["dimension"][dim]["category"]["index"]
    return sorted(ix, key=ix.get) if isinstance(ix, dict) else ix
M, Y = order("MaaleMetode"), order("Yrke")
v = d["value"]
rows = []
for yi, yc in enumerate(Y):
    q1, md, q3 = (v[M.index(m) * len(Y) + yi] for m in ("051", "01", "061"))
    if q1 and md and q3 and q3 > q1:
        rows.append([yc, int(q1), int(md), int(q3),
                     round((math.log(q3) - math.log(q1)) / 1.349, 4), 2025])
with open(out, "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh)
    w.writerow(["code", "q1_nok", "median_nok", "q3_nok", "sd_log", "year"])
    w.writerows(rows)
print(f"wrote {out}: {len(rows)} occupations")
