#!/usr/bin/env python3
"""Regenerate inst/extdata/part_time_by_occupation.csv from SSB 12542.

Why
---
.cond_income() turned a part-time worker's FTE wage into an annual income
by multiplying with a stillingsandel drawn uniformly between 0.45 and 0.80,
capped at 85 pct. Those three numbers were set by hand and were the same
for every occupation. Nobody below 45 pct existed, and the cap was a patch
on top of a guess.

12542 gives, per 4-digit STYRK-08 occupation and sex, how employees are
spread over agreed position sizes: 0-19, 20-39, 40-59, 60-79, 80-99 and
100 pct or more. The part-time groups are what the sampler now draws from,
so a part-time grunnskolelaerer gets the stillingsandel distribution of
part-time grunnskolelaerere (2025: 17 pct under 20, 16 pct 20-39, 24 pct
40-59, 19 pct 60-79, 23 pct 80-99), not one shared across all jobs.

What it does not give
---------------------
Nothing below the 4-digit code. A "laerervikar" and a part-time "lektor"
share the code 2341 and therefore the distribution, although vikarer are
likely to sit lower. Within a group the sampler draws uniformly.

Whether someone is part-time at all still comes from 11418 (headcounts by
schedule), as before. 12542 only decides HOW part-time.

Output columns
--------------
code    4-digit STYRK-08, or "0-9" for all occupations (fallback)
gender  A (both), M, F
p00_19 .. p80_99   counts of part-time employees per position-size group
p100    count at 100 pct or more
deltid  total part-time (= sum of the five part-time groups)
total   all employees
year    reference year (4th quarter)
"""
import csv, json, os, urllib.request

TABLE = "12542"
YEAR = "2025"
URL = f"https://data.ssb.no/api/v0/no/table/{TABLE}"
GROUPS = ["P000-019", "P020-039", "P040-059", "P060-079", "P080-099", "P100", "D", "P000-100"]
COLS = ["p00_19", "p20_39", "p40_59", "p60_79", "p80_99", "p100", "deltid", "total"]
GENDER = {"0": "A", "1": "M", "2": "F"}

here = os.path.dirname(os.path.abspath(__file__))
out = os.path.join(here, "..", "inst", "extdata", "part_time_by_occupation.csv")

meta = json.load(urllib.request.urlopen(URL))
yrke = [v for v in meta["variables"] if v["code"] == "Yrke"][0]
codes = [c for c in yrke["values"] if len(c) == 4 and c.isdigit()] + ["0-9"]

q = {"query": [
        {"code": "Yrke", "selection": {"filter": "item", "values": codes}},
        {"code": "Kjonn", "selection": {"filter": "item", "values": list(GENDER)}},
        {"code": "ArbeidsTidRen", "selection": {"filter": "item", "values": GROUPS}},
        {"code": "Tid", "selection": {"filter": "item", "values": [YEAR]}}],
     "response": {"format": "json-stat2"}}
req = urllib.request.Request(URL, data=json.dumps(q).encode(),
                             headers={"Content-Type": "application/json"})
d = json.load(urllib.request.urlopen(req))

def order(dim):
    ix = d["dimension"][dim]["category"]["index"]
    return sorted(ix, key=ix.get) if isinstance(ix, dict) else ix

Y, K, A = order("Yrke"), order("Kjonn"), order("ArbeidsTidRen")
val = d["value"]
def get(y, k, a):
    return val[(Y.index(y) * len(K) + K.index(k)) * len(A) + A.index(a)]

rows = []
mismatched = 0
for y in Y:
    for k in K:
        vals = [get(y, k, a) for a in GROUPS]
        if vals[-1] in (None, 0):
            continue
        vals = [0 if v is None else int(v) for v in vals]
        # The five part-time groups should add up to "Deltid", but SSB
        # rounds small cells for confidentiality (7543, both sexes: groups
        # 3+3+3+0+3 against Deltid 6). Not an error in the data, but it
        # means small occupations carry noise. The sampler therefore falls
        # back to the all-occupation distribution when Deltid < 30.
        if abs(sum(vals[:5]) - vals[6]) > max(5, 0.005 * vals[6]):
            mismatched += 1
        rows.append([y, GENDER[k]] + vals + [YEAR])

with open(out, "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh)
    w.writerow(["code", "gender"] + COLS + ["year"])
    w.writerows(rows)
print(f"wrote {out}: {len(rows)} rows; {mismatched} with rounding gaps (small cells)")
