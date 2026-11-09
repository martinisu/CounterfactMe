#!/usr/bin/env python3
"""Regenerate the person-level income files from SSB 06655.

Why this replaced income_deciles.csv
------------------------------------
income_deciles.csv carried status "untraced" in SOURCES.csv -- ten decile
boundaries with no recorded origin -- and it was the backbone of the whole
income dimension: .cfm_env$income, read by both sample_income() and
.cond_income().

Checking it against SSB 09935 (mean gross income per decile, persons 17+)
traced it: the midpoints match the **2022** distribution to within 1-2 pct in
D5, D7 and D9. It was SSB's own figures, two years stale, while the rest of
the package runs on 2024 (wealth from 08589, wealth deciles from 10318).

Three separate problems, not one:

  1. LEVEL. 13 pct low through D3-D9 against 2024. Pure ageing of the file.

  2. THE BOTTOM HAD THE WRONG SHAPE. The package drew uniformly on
     [0, 150 000], giving a mean of 75 000; SSB's D1 mean is 50 400. The
     bottom decile is concentrated near zero, not spread evenly. Worse, it
     conflated "no income" with "low income": 06655 counts 4 468 820 persons
     17+ WITH gross income against a population of 4 541 587, so 72 767
     (1.6 pct) have none at all. That share runs from 3.39 pct at 17-24 down
     to 0.62 pct at 67+, and it is now carried explicitly.

  3. THE TOP HAD THE WRONG SHAPE -- the same error just fixed for wealth.
     sample_income() drew uniformly between 900 000 and a 2 MNOK cap. An
     income top is a Pareto tail, not a rectangle.

The fix, mirroring data-raw/fetch_wealth_tail.py
------------------------------------------------
06655 publishes person counts AND sums per income interval, by the same six
age bands the wealth table uses. Count and sum give each interval's mean, so
every interval can be anchored rather than assumed flat. The open top
interval ("2 000 000 kr og over") gets a Pareto with alpha solved so its
truncated mean equals SSB's.

Income alpha lands near 2.1, against 1.09-1.55 for wealth -- income is less
concentrated, as expected. Note that alpha > 2 means finite variance, so
unlike the wealth tail, the income tail mean CAN be tested empirically.

The cap (5 bn NOK) is a guard, not a measurement; it sits far above any
realistic Norwegian personal gross income and barely moves alpha.

Run:  python3 data-raw/fetch_income.py
"""

import csv
import json
import math
import pathlib
import urllib.request

URL = "https://data.ssb.no/api/v0/no/table/06655"
YEAR = "2024"
BANDS = ["17-24", "25-34", "35-44", "45-54", "55-66", "67+"]

# code -> (label, lower_nok, upper_nok). None upper = open-ended Pareto tail.
BRACKETS = [
    ("60", "Under 100 000 kr",          0,       99_999),
    ("61", "100 000 - 199 999 kr",      100_000, 199_999),
    ("62", "200 000 - 299 999 kr",      200_000, 299_999),
    ("08", "300 000 - 399 999 kr",      300_000, 399_999),
    ("09", "400 000 - 499 999 kr",      400_000, 499_999),
    ("70", "500 000 - 749 999 kr",      500_000, 749_999),
    ("71", "750 000 - 999 999 kr",      750_000, 999_999),
    ("72", "1 000 000 - 1 999 999 kr",  1_000_000, 1_999_999),
    ("73", "2 000 000 kr og over",      2_000_000, None),
]
TAIL_CODE = "73"
XM = 2e6
CAP = 5e9

ROOT = pathlib.Path(__file__).resolve().parents[1]
OUT_BY_AGE = ROOT / "inst/extdata/income_by_age.csv"
OUT_TAIL = ROOT / "inst/extdata/income_tail_person.csv"
OUT_NONE = ROOT / "inst/extdata/income_none_share.csv"


def truncated_pareto_mean(alpha, xm=XM, cap=CAP):
    if abs(alpha - 1.0) < 1e-9:
        return xm * math.log(cap / xm) / (1 - xm / cap)
    upper = alpha * xm / (alpha - 1) * (1 - (xm / cap) ** (alpha - 1))
    return upper / (1 - (xm / cap) ** alpha)


def solve_alpha(target_mean):
    lo, hi = 1.001, 12.0
    for _ in range(300):
        mid = (lo + hi) / 2
        if truncated_pareto_mean(mid) > target_mean:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def fetch():
    query = {
        "query": [
            {"code": "BruttoInn", "selection": {"filter": "item",
             "values": ["01"] + [c for c, *_ in BRACKETS]}},
            {"code": "Alder", "selection": {"filter": "item", "values": BANDS}},
            {"code": "Kjonn", "selection": {"filter": "item", "values": ["0"]}},
            {"code": "ContentsCode", "selection": {"filter": "item",
             "values": ["Personar", "BruttoInntekt"]}},
            {"code": "Tid", "selection": {"filter": "item", "values": [YEAR]}},
        ],
        "response": {"format": "json-stat2"},
    }
    req = urllib.request.Request(URL, data=json.dumps(query).encode(),
                                headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=60) as fh:
        return json.loads(fh.read().decode())


def population_by_band():
    """Persons 17+ per age band, from the 08589 extract already in extdata."""
    pop = {}
    with (ROOT / "inst/extdata/wealth_by_age.csv").open(encoding="utf-8") as fh:
        for row in csv.DictReader(fh):
            pop[row["age_band"]] = pop.get(row["age_band"], 0) + int(row["count"])
    return pop


def main():
    d = fetch()
    ids, size, values = d["id"], d["size"], d["value"]
    index = {k: d["dimension"][k]["category"]["index"] for k in ids}

    def cell(**sel):
        off = 0
        for key, dim in zip(ids, size):
            off = off * dim + index[key][sel[key]]
        return values[off]

    def get(code, band, what):
        return cell(BruttoInn=code, Alder=band, Kjonn="0",
                    ContentsCode=what, Tid=YEAR)

    by_age, tail, none = [], [], []
    pop = population_by_band()

    for band in BANDS:
        for code, label, lo, hi in BRACKETS:
            n = get(code, band, "Personar")
            total_mill = get(code, band, "BruttoInntekt")
            by_age.append({
                "age_band": band, "bracket_code": code, "bracket_label": label,
                "lower_nok": lo, "upper_nok": "" if hi is None else hi,
                "count": int(n), "sum_mill": int(total_mill),
                "mean_nok": round(total_mill * 1e6 / n),
            })
            if code == TAIL_CODE:
                mean = total_mill * 1e6 / n
                alpha = solve_alpha(mean)
                assert abs(truncated_pareto_mean(alpha) - mean) < 1.0
                tail.append({
                    "age_band": band, "n_persons": int(n),
                    "sum_mill": int(total_mill), "mean_nok": round(mean),
                    "pareto_xm": int(XM), "pareto_alpha": round(alpha, 4),
                    "pareto_cap": int(CAP),
                })
                print(f"  {band:8} tail n={int(n):>7,}  mean={mean:>12,.0f}  alpha={alpha:.4f}")

        with_income = get("01", band, "Personar")
        none.append({
            "age_band": band, "population": pop[band],
            "with_income": int(with_income),
            "none": int(pop[band] - with_income),
            "share_none": round((pop[band] - with_income) / pop[band], 5),
        })

    for path, rows in ((OUT_BY_AGE, by_age), (OUT_TAIL, tail), (OUT_NONE, none)):
        with path.open("w", newline="", encoding="utf-8") as fh:
            w = csv.DictWriter(fh, fieldnames=list(rows[0]))
            w.writeheader()
            w.writerows(rows)
        print(f"Wrote {path.name}  ({len(rows)} rows)")


if __name__ == "__main__":
    main()
