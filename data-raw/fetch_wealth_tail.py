#!/usr/bin/env python3
"""Regenerate inst/extdata/wealth_tail_person.csv from SSB 08589.

Why this file exists
--------------------
.cond_wealth() draws the body of the net-wealth distribution from SSB 08589,
which is PER PERSON (17+, by age band). Until 2026-09-18 it then escalated the
top bracket using thresholds taken from SSB 10318 -- "Desilfordelt berekna
nettoformue for HUSHALD". Households pool wealth, so a household threshold sits
higher than a person threshold: the sampler was measuring persons with a
household yardstick. The tier probabilities it used (0.68/0.25/0.05/0.007) were
marked "stylized" in the code, i.e. not derived from anything.

That mattered because the entire wealth tax lives in that tail.

The fix, person-level throughout
--------------------------------
08589's top interval really is "2 000 000 kr og over" -- there is no finer
person-level bracket to fetch. But the table also publishes SumNetto, the total
taxable net wealth per interval. Count and sum together give the MEAN above
2 MNOK, per age band, per person. That pins the tail.

We draw the tail from a Pareto truncated at `cap`, with alpha solved so the
truncated mean equals SSB's figure exactly. One parameter derived from data,
replacing four stylised ones.

The cap is a judgement call, not a measurement: 50 bn NOK, above any Norwegian
individual's net wealth. It moves alpha only in the third decimal (checked
against caps of 10, 50 and 200 bn), so it does no real work -- it only stops an
alpha near 1.1 from occasionally emitting a physically absurd draw.

Run:  python3 data-raw/fetch_wealth_tail.py
"""

import csv
import json
import math
import pathlib
import urllib.request

TABLE = "08589"
URL = f"https://data.ssb.no/api/v0/no/table/{TABLE}"
YEAR = "2024"
BANDS = ["17-24", "25-34", "35-44", "45-54", "55-66", "67+"]

XM = 2e6          # SSB's top interval starts here
CAP = 5e10        # see docstring: a guard, not a measurement
ROOT = pathlib.Path(__file__).resolve().parents[1]
OUT = ROOT / "inst/extdata/wealth_tail_person.csv"
OUT_CLASS = ROOT / "inst/extdata/wealth_class_person.csv"

# Total persons 17+ and the share at or above XM, from the same 08589 extract
# already in inst/extdata/wealth_by_age.csv. Needed to convert "P(X > x | tail)"
# into an unconditional person percentile.
def national_share():
    import csv as _csv
    total = tail = 0
    with (ROOT / "inst/extdata/wealth_by_age.csv").open(encoding="utf-8") as fh:
        for row in _csv.DictReader(fh):
            total += int(row["count"])
            if row["bracket_code"] == "10+":
                tail += int(row["count"])
    return tail / total


def truncated_pareto_mean(alpha, xm=XM, cap=CAP):
    """E[X] for Pareto(alpha, xm) truncated at cap."""
    if abs(alpha - 1.0) < 1e-9:
        return xm * math.log(cap / xm) / (1 - xm / cap)
    upper = alpha * xm / (alpha - 1) * (1 - (xm / cap) ** (alpha - 1))
    return upper / (1 - (xm / cap) ** alpha)


def solve_alpha(target_mean):
    """Bisect for the alpha whose truncated mean is target_mean."""
    lo, hi = 1.001, 8.0
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
            {"code": "FormueGjeld", "selection": {"filter": "item", "values": ["10+"]}},
            {"code": "Kjonn", "selection": {"filter": "item", "values": ["0"]}},
            {"code": "Alder", "selection": {"filter": "item", "values": BANDS}},
            {"code": "ContentsCode",
             "selection": {"filter": "item", "values": ["Nettoformue", "SumNetto"]}},
            {"code": "Tid", "selection": {"filter": "item", "values": [YEAR]}},
        ],
        "response": {"format": "json-stat2"},
    }
    req = urllib.request.Request(
        URL, data=json.dumps(query).encode(),
        headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=60) as fh:
        return json.loads(fh.read().decode())


def main():
    d = fetch()
    ids, size, values = d["id"], d["size"], d["value"]
    index = {k: d["dimension"][k]["category"]["index"] for k in ids}

    def cell(**sel):
        off = 0
        for key, dim in zip(ids, size):
            off = off * dim + index[key][sel[key]]
        return values[off]

    rows = []
    for band in BANDS:
        n = cell(FormueGjeld="10+", Kjonn="0", Alder=band,
                 ContentsCode="Nettoformue", Tid=YEAR)
        total_mill = cell(FormueGjeld="10+", Kjonn="0", Alder=band,
                          ContentsCode="SumNetto", Tid=YEAR)
        mean = total_mill * 1e6 / n
        alpha = solve_alpha(mean)
        realised = truncated_pareto_mean(alpha)
        assert abs(realised - mean) < 1.0, f"{band}: alpha did not converge"
        rows.append({
            "age_band": band,
            "n_persons": int(n),
            "sum_netto_mill": int(total_mill),
            "mean_nok": round(mean),
            "pareto_xm": int(XM),
            "pareto_alpha": round(alpha, 4),
            "pareto_cap": int(CAP),
        })
        print(f"  {band:8} n={int(n):>9,}  mean={mean:>13,.0f}  alpha={alpha:.4f}")

    with OUT.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    print(f"\nWrote {OUT}")

    # --- person-level class cut-points ---------------------------------
    # The old code labelled top5/top1/top01 with thresholds from SSB 10318,
    # a HOUSEHOLD table. Households pool wealth, so those cut-points are far
    # too high for a person: they demanded 2-3x too much wealth before a
    # person counted as top-tier. Derive person cut-points instead.
    #
    # P(X > x) = share_tail * (xm/x)^alpha, so x = xm / (p/share_tail)^(1/alpha),
    # using the national mean above xm to fix alpha.
    share = national_share()
    national_mean = sum(r["sum_netto_mill"] for r in rows) * 1e6 \
        / sum(r["n_persons"] for r in rows)
    national_alpha = solve_alpha(national_mean)
    class_rows = []
    for label, p in (("top5", 0.05), ("top1", 0.01), ("top01", 0.001)):
        cut = XM / ((p / share) ** (1 / national_alpha))
        class_rows.append({
            "class": label,
            "percentile": p,
            "cut_nok": round(cut),
            "share_at_or_above_xm": round(share, 6),
            "national_mean_nok": round(national_mean),
            "national_alpha": round(national_alpha, 4),
        })
        print(f"  {label:6} top {p:>6.1%} of persons -> {cut:>14,.0f} kr")
    with OUT_CLASS.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=list(class_rows[0]))
        writer.writeheader()
        writer.writerows(class_rows)
    print(f"Wrote {OUT_CLASS}")


if __name__ == "__main__":
    main()
