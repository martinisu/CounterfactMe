#!/usr/bin/env python3
"""Regenerate inst/extdata/life_expectancy.csv from SSB 14569.

Why
---
.death_year() inside .cond_parents() conditioned on AGE ALONE:

    p_dead <- if (implied_age >= 105) 1.00
              else if (implied_age >= 95) 0.95 ...

No sex, no geography. Mothers and fathers died on an identical schedule,
which is the largest single error here: SSB 14569 puts the sex gap in life
expectancy at birth at 3.2 years (women 84.8, men 81.6, 2025).

What this table gives, and what it does not
-------------------------------------------
14569 is "Forventet gjenstaende levetid" -- REMAINING life expectancy from a
given age, which is the right quantity for a person already alive, rather
than life expectancy at birth. By single year of age 0-99, by sex, and by
the six-level sentralitet index the package already carries in
kommune_sentralitet.csv.

Measured spreads (2025, at birth):
    sex                       3.2 years (women longer)
    sentralitet, men          2.2 years (most central 82.7, least 80.4)
    sentralitet, women        1.2 years (85.8 vs 84.5)

Note the sentralitet gradient is NOT monotone in the middle for men: Se04
and Se05 both sit above Se03. Read off, not smoothed.

What is deliberately absent: education and income. SSB publishes no statbank
table of life expectancy by education level -- this was checked on
2026-10-15, and the design note that assumed one was wrong. The nearest
source is an SSB article giving an 11-12 year gap between the highest and
lowest OCCUPATIONAL groups for men, but it rests on 1996-2000 and 2009-2013
data and is about occupation, not education.

Stacking such a gradient on top of sex and sentralitet would double-count:
the three overlap heavily, and marginal associations are not additive. So
this file carries the three axes that are published and current, and nothing
more. Any education gradient would be an assumption and must be labelled as
one.

Run:  python3 data-raw/fetch_life_expectancy.py
"""

import csv
import json
import pathlib
import urllib.request

URL = "https://data.ssb.no/api/v0/no/table/14569"
YEAR = "2025"
REGIONS = ["0", "Se01", "Se02", "Se03", "Se04", "Se05", "Se06"]
# SSB's sentralitet index band -> the package's sentralitet code (1-6).
REGION_TO_SENTRALITET = {
    "0": "", "Se01": "1", "Se02": "2", "Se03": "3",
    "Se04": "4", "Se05": "5", "Se06": "6",
}
SEXES = {"1": "M", "2": "F"}
AGES = [f"{a:03d}" for a in range(0, 100)]

OUT = pathlib.Path(__file__).resolve().parents[1] / "inst/extdata/life_expectancy.csv"


def fetch():
    query = {
        "query": [
            {"code": "Region", "selection": {"filter": "item", "values": REGIONS}},
            {"code": "Kjonn", "selection": {"filter": "item", "values": list(SEXES)}},
            {"code": "Alder", "selection": {"filter": "item", "values": AGES}},
            {"code": "ContentsCode", "selection": {"filter": "item", "values": ["Levetid"]}},
            {"code": "Tid", "selection": {"filter": "item", "values": [YEAR]}},
        ],
        "response": {"format": "json-stat2"},
    }
    req = urllib.request.Request(URL, data=json.dumps(query).encode(),
                                headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=90) as fh:
        return json.loads(fh.read().decode())


def main():
    d = fetch()
    ids, size, values = d["id"], d["size"], d["value"]
    index = {k: d["dimension"][k]["category"]["index"] for k in ids}
    labels = d["dimension"]["Region"]["category"]["label"]

    def cell(**sel):
        off = 0
        for key, dim in zip(ids, size):
            off = off * dim + index[key][sel[key]]
        return values[off]

    rows = []
    for region in REGIONS:
        for sex_code, sex in SEXES.items():
            for age_code in AGES:
                v = cell(Region=region, Kjonn=sex_code, Alder=age_code,
                         ContentsCode="Levetid", Tid=YEAR)
                if v is None:
                    continue
                rows.append({
                    "region_code": region,
                    "region_label": labels[region],
                    "sentralitet": REGION_TO_SENTRALITET[region],
                    "gender": sex,
                    "age": int(age_code),
                    "remaining_years": round(float(v), 2),
                })

    with OUT.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f"Wrote {OUT.name}  ({len(rows)} rows)")

    def at(region, sex, age):
        return next(r["remaining_years"] for r in rows
                    if r["region_code"] == region and r["gender"] == sex
                    and r["age"] == age)
    print(f"  sex gap at birth:        {at('0','F',0) - at('0','M',0):+.1f} years")
    print(f"  sentralitet gap, men:    {at('Se01','M',0) - at('Se06','M',0):+.1f}")
    print(f"  sentralitet gap, women:  {at('Se01','F',0) - at('Se06','F',0):+.1f}")


if __name__ == "__main__":
    main()
