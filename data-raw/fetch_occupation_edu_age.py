#!/usr/bin/env python3
"""Regenerate occupation_by_edu.csv (SSB 12849) and occupation_by_age.csv
(SSB 09793): employed persons by 2-digit STYRK-08 occupation, crossed with
education level and with age band.

Why
---
.occupation_weights() conditioned occupation on education through
.edu_to_styrk_majors(): hand-set weights per 1-DIGIT major group, e.g. 0.2
for grunnskole x "3 Hoyskoleyrker". Major 3 contains both shop supervisors
and ship's engineers, so the weight was far too coarse, and nothing at all
conditioned on age. On the live site that produced a 19-year-old with only
grunnskole working as "annenmaskinist" on 626 000 a year, and a bachelor
in medicine working as "overlege".

What the sampler now does
-------------------------
For each 2-digit occupation y:
    R_edu(y, e) = P(y | education e) / P(y)      from 12849, age 20-66
    R_age(y, a) = P(y | age band a)  / P(y)      from 09793
and multiplies the 4-digit headcount by R_edu * R_age. Within a 2-digit
group the 4-digit split stays proportional to headcount (11418).

Assumption, stated plainly: education and age act independently on the
2-digit occupation. 12849 has no age bands below "20-66" and 09793 has
no education, so the joint is not available in open data.

Not covered here: licensed professions (leger, tannleger, psykologer,
advokater ...) sit inside 2-digit groups dominated by people with shorter
degrees (22 is mostly nurses). Those get an explicit rule in R, not data.
"""
import csv, json, os, urllib.request

here = os.path.dirname(os.path.abspath(__file__))
ext = os.path.join(here, "..", "inst", "extdata")

def post(table, query):
    req = urllib.request.Request(f"https://data.ssb.no/api/v0/no/table/{table}",
        data=json.dumps({"query": query, "response": {"format": "json-stat2"}}).encode(),
        headers={"Content-Type": "application/json"})
    return json.load(urllib.request.urlopen(req))

def order(d, dim):
    ix = d["dimension"][dim]["category"]["index"]
    return sorted(ix, key=ix.get) if isinstance(ix, dict) else ix

def codes2(table):
    meta = json.load(urllib.request.urlopen(f"https://data.ssb.no/api/v0/no/table/{table}"))
    y = [v for v in meta["variables"] if v["code"] == "Yrke"][0]
    return [c for c in y["values"] if len(c) == 2 and c.isdigit()]

# ---- 12849: education level x 2-digit occupation, age 20-66, 2025 -------
EDU = ["1", "2", "3", "4", "0+9"]
d = post("12849", [
    {"code": "UtdNivaa", "selection": {"filter": "item", "values": EDU}},
    {"code": "Fagfelt", "selection": {"filter": "item", "values": [str(i) for i in range(10)]}},
    {"code": "Yrke", "selection": {"filter": "item", "values": codes2("12849")}},
    {"code": "Alder", "selection": {"filter": "item", "values": ["20-66"]}},
    {"code": "Tid", "selection": {"filter": "item", "values": ["2025"]}}])
U, F, Y = order(d, "UtdNivaa"), order(d, "Fagfelt"), order(d, "Yrke")
acc = {}
for ui, u in enumerate(U):
    for fi in range(len(F)):          # sum over the ten top-level fields
        for yi, y in enumerate(Y):
            v = d["value"][(ui * len(F) + fi) * len(Y) + yi] or 0
            acc[(y, u)] = acc.get((y, u), 0) + v
with open(os.path.join(ext, "occupation_by_edu.csv"), "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh); w.writerow(["styrk2", "edu_level", "count", "year"])
    for (y, u), v in sorted(acc.items()):
        w.writerow([y, u, int(v), 2025])
print("occupation_by_edu.csv:", len(acc), "rows")

# ---- 09793: age band x 2-digit occupation, both sexes, 2024 -------------
AGE = ["15-24", "25-39", "40-54", "55-74"]
d = post("09793", [
    {"code": "Kjonn", "selection": {"filter": "item", "values": ["0"]}},
    {"code": "Alder", "selection": {"filter": "item", "values": AGE}},
    {"code": "Yrke", "selection": {"filter": "item", "values": codes2("09793")}},
    {"code": "Tid", "selection": {"filter": "item", "values": ["2024"]}}])
A, Y = order(d, "Alder"), order(d, "Yrke")
rows = []
for ai, a in enumerate(A):
    for yi, y in enumerate(Y):
        v = d["value"][ai * len(Y) + yi]
        rows.append([y, a, "" if v is None else v, 2024])
with open(os.path.join(ext, "occupation_by_age.csv"), "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh); w.writerow(["styrk2", "age_band", "thousands", "year"])
    w.writerows(rows)
print("occupation_by_age.csv:", len(rows), "rows;",
      sum(1 for r in rows if r[2] == ""), "suppressed cells (treated as no information)")
