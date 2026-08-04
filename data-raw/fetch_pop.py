#!/usr/bin/env python3
"""Fetch current population by age + region from SSB table 07459.

Outputs:
  inst/extdata/age_distribution.csv   (5-year bands, national shares)
  inst/extdata/municipalities.csv     (code, name, county, population)
  inst/extdata/counties.csv           (code, name, population)

All figures are as of 1 January of the latest available year (typically t for t=today).
"""
import json, time, urllib.request, csv, os, collections, sys

BASE = "https://data.ssb.no/api/v0/no/table/07459"
PKG  = "/sessions/peaceful-stoic-franklin/mnt/9_counterfactme/CounterfactMe/inst/extdata"

def http_json(req, timeout=90):
    for attempt in range(4):
        try:
            with urllib.request.urlopen(req, timeout=timeout) as r:
                return json.loads(r.read().decode("utf-8"))
        except Exception as e:
            print(f"  retry {attempt+1}: {e}", file=sys.stderr); time.sleep(2**attempt)
    raise SystemExit("giving up")

meta = http_json(urllib.request.Request(BASE))
vars_ = {v["code"]: v for v in meta["variables"]}
all_regions = vars_["Region"]["values"]
region_labels = vars_["Region"]["valueTexts"]
all_ages    = vars_["Alder"]["values"]
all_years   = vars_["Tid"]["values"]
latest_year = all_years[-1]
print(f"[meta] latest year = {latest_year}, {len(all_regions)} region codes, {len(all_ages)} ages", file=sys.stderr)

# Separate regions by code length
country_code = "0"
county_codes = [r for r in all_regions if len(r) == 2 and r.isdigit()]
mun_codes    = [r for r in all_regions if len(r) == 4 and r.isdigit()]
print(f"[meta] country=1, counties={len(county_codes)}, municipalities={len(mun_codes)}", file=sys.stderr)

def fetch(regions, ages, year):
    body = {"query":[
        {"code":"Region","selection":{"filter":"item","values": regions}},
        {"code":"Kjonn","selection":{"filter":"item","values":["1","2"]}},
        {"code":"Alder","selection":{"filter":"item","values": ages}},
        {"code":"Tid","selection":{"filter":"item","values":[year]}},
    ], "response":{"format":"json-stat2"}}
    req = urllib.request.Request(BASE, data=json.dumps(body).encode(),
        headers={"Content-Type":"application/json","Accept":"application/json"})
    return http_json(req)

def parse(js):
    """Yield (region_code, age_code, value) tuples."""
    dim_ids = js["id"]; sizes = js["size"]
    cats = {}
    for d in dim_ids:
        idx = js["dimension"][d]["category"]["index"]
        if isinstance(idx, dict):
            cats[d] = sorted(idx.keys(), key=lambda k: idx[k])
        else:
            cats[d] = list(idx)
    values = js["value"]
    total = 1
    for sz in sizes: total *= sz
    if isinstance(values, dict):
        dense = [0]*total
        for k,v in values.items():
            dense[int(k)] = v if v is not None else 0
        values = dense
    else:
        values = [0 if v is None else v for v in values]
    strides = [1]*len(sizes)
    for i in range(len(sizes)-2,-1,-1):
        strides[i] = strides[i+1]*sizes[i+1]
    i_r = dim_ids.index("Region"); i_a = dim_ids.index("Alder")
    for cell, val in enumerate(values):
        if not val: continue
        rem = cell; coord = [0]*len(sizes)
        for d,st in enumerate(strides):
            coord[d] = rem // st; rem = rem % st
        yield cats["Region"][coord[i_r]], cats["Alder"][coord[i_a]], val

# --- 1. National age distribution (country_code + all ages) ---
print(f"[fetch] national ages for {latest_year}…", file=sys.stderr)
js = fetch([country_code], all_ages, latest_year)
age_totals = collections.Counter()
for reg, age, v in parse(js):
    age_totals[age] += v
total_pop = sum(age_totals.values())
print(f"[nat] total population {total_pop:,}", file=sys.stderr)

# --- 2. All municipalities (chunked if needed) ---
# Chunk to keep cells under ~200k per request. 400 regions * 106 ages * 2 sex = 84,800 fine.
print(f"[fetch] regions for {latest_year}…", file=sys.stderr)
reg_totals = collections.Counter()
CHUNK = 400
all_reg = county_codes + mun_codes
for i in range(0, len(all_reg), CHUNK):
    batch = all_reg[i:i+CHUNK]
    print(f"  chunk {i}-{i+len(batch)}", file=sys.stderr)
    js = fetch(batch, all_ages, latest_year)
    for reg, age, v in parse(js):
        reg_totals[reg] += v
    time.sleep(0.3)

# --- 3. Region names (strip bilingual Sami suffixes like "Trondheim - Tråante") ---
def clean(s):
    return s.split(" - ")[0].strip()
region_name = {c: clean(n) for c, n in zip(all_regions, region_labels)}

# --- 4. Write age_distribution.csv (5-year bands 0-4, 5-9, …, 95+) ---
def age_int(code):
    if code.endswith("+"): return int(code[:-1])
    return int(code)

bands = []
for lo in range(0, 100, 5):
    hi = lo + 4
    bands.append((lo, hi))
bands.append((100, 105))  # cap

band_share = []
for lo, hi in bands:
    pop = sum(v for a,v in age_totals.items() if lo <= age_int(a) <= hi)
    band_share.append((lo, hi, pop/total_pop))

out = os.path.join(PKG, "age_distribution.csv")
with open(out,"w",newline="",encoding="utf-8") as fh:
    w = csv.writer(fh); w.writerow(["age_lower","age_upper","population_share"])
    for lo,hi,sh in band_share:
        w.writerow([lo, hi, f"{sh:.5f}"])
print(f"[write] {out} ({len(band_share)} bands, shares sum to {sum(s for _,_,s in band_share):.4f})")

# --- 5. Write counties.csv ---
counties_rows = []
for code in county_codes:
    pop = reg_totals.get(code, 0)
    if pop == 0: continue
    counties_rows.append((code, region_name[code], pop))
counties_rows.sort(key=lambda r: -r[2])
out = os.path.join(PKG, "counties.csv")
with open(out,"w",newline="",encoding="utf-8") as fh:
    w = csv.writer(fh); w.writerow(["code","name","population"])
    w.writerows(counties_rows)
print(f"[write] {out} ({len(counties_rows)} counties)")

# --- 6. Write municipalities.csv (code, name, county, population) ---
# Infer county by first 2 digits of municipality code.
county_name_by_code = {c: n for c, n, _ in counties_rows}
mun_rows = []
for code in mun_codes:
    pop = reg_totals.get(code, 0)
    if pop == 0: continue
    cty_code = code[:2]
    cty_name = county_name_by_code.get(cty_code, "")
    mun_rows.append((code, region_name[code], cty_name, pop))
mun_rows.sort(key=lambda r: r[0])
out = os.path.join(PKG, "municipalities.csv")
with open(out,"w",newline="",encoding="utf-8") as fh:
    w = csv.writer(fh); w.writerow(["code","name","county","population"])
    w.writerows(mun_rows)
print(f"[write] {out} ({len(mun_rows)} municipalities, total {sum(r[3] for r in mun_rows):,})")

print(f"\n[done] source = SSB 07459, reference year = {latest_year}")
