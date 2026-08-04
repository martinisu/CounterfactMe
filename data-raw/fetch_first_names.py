#!/usr/bin/env python3
"""Fetch cohort-weighted Norwegian first names from SSB table 10467."""
import json, time, urllib.request, csv, sys, os, collections

BASE = "https://data.ssb.no/api/v0/no/table/10467"
OUT  = "/sessions/peaceful-stoic-franklin/mnt/9_counterfactme/CounterfactMe/inst/extdata/first_names_cohort.csv"

def http(req):
    with urllib.request.urlopen(req, timeout=60) as r:
        return json.loads(r.read().decode("utf-8"))

# --- metadata ---
meta = http(urllib.request.Request(BASE))
vars_ = {v["code"]: v for v in meta["variables"]}
name_codes = vars_["Fornavn"]["values"]
year_codes = vars_["Tid"]["values"]
print(f"[meta] {len(name_codes)} names, {len(year_codes)} years "
      f"({year_codes[0]}–{year_codes[-1]})", file=sys.stderr)

# --- decode Fornavn code -> (name, gender) ---
def decode(code):
    gender = "F" if code[0] == "1" else "M" if code[0] == "2" else None
    body = code[1:].replace("Z1", "Æ").replace("Z2", "Ø").replace("Z3", "Å").replace("_", "-")
    pretty = "-".join(p.capitalize() for p in body.split("-"))
    return pretty, gender

# --- fetch in decade chunks ---
buckets = collections.defaultdict(list)
for y in year_codes:
    buckets[(int(y)//10)*10].append(y)

# key = (name, gender, cohort_decade) -> sum of births
agg = collections.Counter()

for dec in sorted(buckets):
    yrs = buckets[dec]
    body = {
        "query": [
            {"code":"Fornavn","selection":{"filter":"item","values": name_codes}},
            {"code":"ContentsCode","selection":{"filter":"item","values":["Personer"]}},
            {"code":"Tid","selection":{"filter":"item","values": yrs}},
        ],
        "response": {"format": "json-stat2"},
    }
    data = json.dumps(body).encode("utf-8")
    req = urllib.request.Request(BASE, data=data,
        headers={"Content-Type":"application/json","Accept":"application/json"})
    print(f"[fetch] decade {dec} ({len(yrs)} yrs, {len(name_codes)*len(yrs)} cells)…", file=sys.stderr)

    for attempt in range(4):
        try:
            js = http(req)
            break
        except Exception as e:
            print(f"  retry {attempt+1}: {e}", file=sys.stderr)
            time.sleep(2 ** attempt)
    else:
        raise SystemExit(f"failed decade {dec}")

    # json-stat2 row-major: dims in js['id'], sizes in js['size']
    dim_ids = js["id"]            # e.g. ['Fornavn','ContentsCode','Tid']
    sizes   = js["size"]
    # category codes in index order for each dim
    cat_codes = {}
    for d in dim_ids:
        idx = js["dimension"][d]["category"]["index"]
        if isinstance(idx, dict):
            cat_codes[d] = sorted(idx.keys(), key=lambda k: idx[k])
        else:  # list
            cat_codes[d] = list(idx)

    values = js["value"]
    # sparse encoding (dict with str keys) vs dense list — handle both
    total = 1
    for s in sizes: total *= s
    if isinstance(values, dict):
        dense = [0]*total
        for k,v in values.items():
            dense[int(k)] = v if v is not None else 0
        values = dense
    else:
        values = [0 if v is None else v for v in values]

    # Assume order [Fornavn, ContentsCode, Tid] — verify and use fast indexing.
    assert dim_ids == ["Fornavn", "ContentsCode", "Tid"], dim_ids
    N = sizes[0]; Y = sizes[2]
    name_cats = cat_codes["Fornavn"]
    year_cats = [int(y) for y in cat_codes["Tid"]]
    decoded_cache = [decode(c) for c in name_cats]
    cohort_of = [(y // 10) * 10 for y in year_cats]

    for cell, v in enumerate(values):
        if not v:
            continue
        ni = cell // Y
        yi = cell % Y
        pretty, gender = decoded_cache[ni]
        if gender is None:
            continue
        agg[(pretty, gender, cohort_of[yi])] += v

    time.sleep(0.3)  # be polite; 40 req/min limit

# --- write CSV ---
os.makedirs(os.path.dirname(OUT), exist_ok=True)
rows = [(n, g, c, f) for (n,g,c), f in agg.items() if f > 0]
rows.sort(key=lambda r: (r[2], r[1], -r[3], r[0]))

with open(OUT, "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh)
    w.writerow(["name","gender","cohort","frequency"])
    w.writerows(rows)

uniq_names = len({r[0] for r in rows})
cohorts = sorted({r[2] for r in rows})
print(f"[done] wrote {len(rows)} rows -> {OUT}")
print(f"       {uniq_names} unique names, cohorts {cohorts[0]}–{cohorts[-1]}")
