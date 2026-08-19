#!/usr/bin/env python3
"""Build inst/extdata/housing_index_prewar.csv.

Two components, deliberately kept apart:

  cpi_index   Consumer price index, SSB table 08981, 1920-. Fetched here,
              so re-running this script reproduces it exactly.

  real_factor Real house price level relative to 1992. This is an
              ESTIMATE. Norway has no official house price index before
              1992 -- SSB table 07230 starts there -- and the standard
              long series (Eitrheim & Erlandsen, Norges Bank) is not
              fetched here. The anchors below reproduce the documented
              shape: flat-to-falling real prices through the war, slow
              growth through the postwar decades, the deregulation boom
              peaking around 1987 and the banking-crisis trough in 1992.
              They are not transcribed from a source.

The product of the two, normalised so 1992 = 100, gives a nominal index
that splices onto 07230 at 1992.
"""
import json, urllib.request, csv, os

OUT = os.path.join("inst", "extdata", "housing_index_prewar.csv")
URL = "https://data.ssb.no/api/v0/no/table/08981"
QUERY = {"query": [{"code": "ContentsCode",
                    "selection": {"filter": "item", "values": ["KpiIndMnd"]}}],
         "response": {"format": "json-stat2"}}

# year -> real house price level, 1992 = 100. Estimate; see docstring.
REAL_ANCHORS = {
    1920: 60, 1935: 65, 1945: 55, 1955: 60,
    1965: 70, 1975: 85, 1985: 115, 1988: 125, 1992: 100,
}

def interp(year, anchors):
    ks = sorted(anchors)
    if year <= ks[0]:  return anchors[ks[0]]
    if year >= ks[-1]: return anchors[ks[-1]]
    for a, b in zip(ks, ks[1:]):
        if a <= year <= b:
            f = (year - a) / (b - a)
            return anchors[a] * (1 - f) + anchors[b] * f

req = urllib.request.Request(URL, data=json.dumps(QUERY).encode(),
                             headers={"Content-Type": "application/json"})
d = json.load(urllib.request.urlopen(req))
idx = d["dimension"]["Tid"]["category"]["index"]
val = d["value"]
cpi = {int(y): val[i] for y, i in idx.items() if val[i] is not None}

base_cpi = cpi[1992]
rows = []
for y in sorted(cpi):
    if y > 1992:
        continue
    real = interp(y, REAL_ANCHORS)
    nominal = (cpi[y] / base_cpi) * (real / 100.0) * 100.0
    rows.append({"year": y,
                 "cpi_index": round(cpi[y], 2),
                 "real_factor": round(real, 1),
                 "nominal_index_1992_100": round(nominal, 2)})

with open(OUT, "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=["year", "cpi_index", "real_factor",
                                      "nominal_index_1992_100"])
    w.writeheader()
    w.writerows(rows)
print(f"wrote {OUT}: {len(rows)} years, {rows[0]['year']}-{rows[-1]['year']}")
