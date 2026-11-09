#!/usr/bin/env python3
"""Build inst/extdata/mortality_income_gradient.csv.

Hva dette er, og hva det ikke er
---------------------------------
En RELATIV gradient: hvor mange aar en person paa en gitt inntektspersentil
ventes aa leve over eller under gjennomsnittet for sitt kjonn. Nivaaet kommer
fra SSB 14569 (alder x kjonn x sentralitet, 2025); denne fila legger bare til
et avvik rundt det. Konstruksjonen er null-sum over en uniform
persentilfordeling, saa populasjonens snitt forblir SSBs tall.

Kilde for spennet: Kinge et al., "Association of Household Income With Life
Expectancy and Cause-Specific Mortality in Norway, 2005-2015", JAMA 2019;
321(19):1916-1925. Registerdata, hele Norge, 40 aar og over.

    menn:    p1 = 70.6 aar, p99 = 84.5  -> spenn 13.9
    kvinner: p1 = 78.0 aar, p99 = 86.4  -> spenn  8.4
    kvartilspenn (rikeste 25 pst mot fattigste 25 pst): menn 8, kvinner 6

Kurveformen er loest numerisk som en blanding av lineaer og probit slik at
forholdet mellom ytterpunktspenn og kvartilspenn treffer Kinges tall:
w = 0.696 for menn, 0.185 for kvinner. Mennenes kurve har langt tyngre haler,
som er hele grunnen til at deres gap er 14 aar og kvinnenes 8.

TRE FORBEHOLD, og det forste er alvorlig
-----------------------------------------
1. OMVENDT KAUSALITET. Kinges egne forfattere peker paa at kronisk sykdom
   forklarer mye av gapet. De fattigste 1 pst inkluderer mennesker som har
   lav inntekt FORDI de er doende. Aa bygge inntekt -> doedelighet som en
   enveispil framstiller et toveisforhold som aarsak. Dette er verst i
   ytterpunktene, altsaa nettopp der tallet er mest slaaende. Det maa staa
   i output, ikke bare her.

2. INNTEKT ER EN PROXY FOR HELE BUNTEN, ikke en paastand om at penger i seg
   selv forlenger liv. Den baerer utdanning, yrke, formue og helseatferd.
   Derfor er det NOEYAKTIG EN gradient her, ikke ett ledd per variabel:
   .cond_health() betinger paa utdanning, saa helse og inntekt er soesken i
   kjeden. Aa legge paa begge ville telt samme utdanning to ganger.
   Alkohol er heller ikke med: .cond_alcohol() trekker selvrapportert
   drikkemonster, ikke alkoholbrukslidelse, og virkningen ligger uansett
   inne i inntektsgradienten.

3. VINTAGE. Tallene er 2005-2015, og gapet VOKSTE i perioden. De er altsaa
   et drojt tiaar gamle og sannsynligvis for smaa i dag.

Run:  python3 data-raw/build_income_mortality.py
"""

import csv
import pathlib
import statistics

ND = statistics.NormalDist()
ROOT = pathlib.Path(__file__).resolve().parents[1]
OUT = ROOT / "inst/extdata/mortality_income_gradient.csv"

# Kinge et al. 2019
ANCHORS = {
    "M": {"p1": 70.6, "p99": 84.5, "quartile_span": 8.0},
    "F": {"p1": 78.0, "p99": 86.4, "quartile_span": 6.0},
}


def shape(r, w):
    """Blanding av lineaer og probit, begge skalert til [-1, 1] over p1-p99."""
    return (1 - w) * (r - 0.5) * 2 + w * ND.inv_cdf(r) / ND.inv_cdf(0.99)


def solve_w(target_ratio):
    lo, hi = 0.0, 1.0
    for _ in range(300):
        mid = (lo + hi) / 2
        ratio = ((shape(0.99, mid) - shape(0.01, mid)) /
                 (shape(0.875, mid) - shape(0.125, mid)))
        if ratio < target_ratio:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def main():
    rows = []
    for sex, a in ANCHORS.items():
        total = a["p99"] - a["p1"]
        w = solve_w(total / a["quartile_span"])
        scale = total / (shape(0.99, w) - shape(0.01, w))

        # Null-sum over uniforme persentiler: trekk fra gjennomsnittet, slik
        # at populasjonens snitt forblir SSBs nivaa.
        raw = {p: shape(p / 100 if p not in (0, 100) else (0.5 if p == 0 else 99.5) / 100, w) * scale
               for p in range(1, 100)}
        mean_raw = sum(raw.values()) / len(raw)

        for p in range(1, 100):
            rows.append({
                "gender": sex,
                "income_percentile": p,
                "years_offset": round(raw[p] - mean_raw, 3),
                "curve_w": round(w, 4),
            })

        off = {p: raw[p] - mean_raw for p in raw}
        print(f"  {sex}: w={w:.3f}  p1={off[1]:+.1f}  p25={off[25]:+.1f}  "
              f"p50={off[50]:+.1f}  p75={off[75]:+.1f}  p99={off[99]:+.1f}  "
              f"spenn={off[99]-off[1]:.1f}")
        q_lo = sum(off[p] for p in range(1, 26)) / 25
        q_hi = sum(off[p] for p in range(75, 100)) / 25
        print(f"      kvartilspenn {q_hi - q_lo:.1f} (Kinge: {a['quartile_span']})")
        print(f"      snitt over persentiler {sum(off.values())/len(off):+.3f} (skal vaere 0)")

    with OUT.open("w", newline="", encoding="utf-8") as fh:
        wtr = csv.DictWriter(fh, fieldnames=list(rows[0]))
        wtr.writeheader()
        wtr.writerows(rows)
    print(f"\nWrote {OUT.name}  ({len(rows)} rows)")


if __name__ == "__main__":
    main()
