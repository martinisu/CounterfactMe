#!/usr/bin/env python3
"""Fetch three SSB tables that unlock education-by-age, occupation salary + part-time,
and disability share by age. Writes three CSVs to CounterfactMe/inst/extdata/.

Tables:
  08921 – Personer 16+ etter utdanningsnivå × alder (Hele landet)
  11418 – Månedslønn etter yrke (STYRK-08 4-digit) × heltid/deltid
  11715 – Uføretrygdedes andel av befolkningen, etter alder
"""
import json, time, urllib.request, csv, os, sys, collections

PKG = "/sessions/peaceful-stoic-franklin/mnt/9_counterfactme/CounterfactMe/inst/extdata"
BASE = "https://data.ssb.no/api/v0/no/table/"

def http(req, timeout=90):
    for attempt in range(4):
        try:
            with urllib.request.urlopen(req, timeout=timeout) as r:
                return json.loads(r.read().decode("utf-8"))
        except Exception as e:
            print(f"  retry {attempt+1}: {e}", file=sys.stderr); time.sleep(2**attempt)
    raise SystemExit("giving up")

def meta(tid): return http(urllib.request.Request(BASE+tid))

def fetch(tid, selections):
    body={"query":[{"code":c,"selection":{"filter":"item","values":v}} for c,v in selections.items()],
          "response":{"format":"json-stat2"}}
    req=urllib.request.Request(BASE+tid, data=json.dumps(body).encode(),
        headers={"Content-Type":"application/json","Accept":"application/json"})
    return http(req)

def parse(js):
    """Return list of dicts keyed by dim-code with `value`."""
    dim_ids=js["id"]; sizes=js["size"]
    cats={}; labels={}
    for d in dim_ids:
        idx=js["dimension"][d]["category"]["index"]
        lbl=js["dimension"][d]["category"]["label"]
        if isinstance(idx,dict):
            codes=sorted(idx.keys(), key=lambda k: idx[k])
        else:
            codes=list(idx)
        cats[d]=codes
        labels[d]={c: lbl.get(c,c) for c in codes}
    values=js["value"]; total=1
    for sz in sizes: total*=sz
    if isinstance(values,dict):
        dense=[None]*total
        for k,v in values.items(): dense[int(k)]=v
        values=dense
    strides=[1]*len(sizes)
    for i in range(len(sizes)-2,-1,-1): strides[i]=strides[i+1]*sizes[i+1]
    rows=[]
    for cell,val in enumerate(values):
        if val is None: continue
        rem=cell; coord=[0]*len(sizes)
        for d,st in enumerate(strides):
            coord[d]=rem//st; rem=rem%st
        rec={"value":val}
        for d,dim in enumerate(dim_ids):
            rec[dim]=cats[dim][coord[d]]
            rec[dim+"_label"]=labels[dim][cats[dim][coord[d]]]
        rows.append(rec)
    return rows

# ================= 1. EDUCATION x AGE (08921) =================
print("[08921] education by age…", file=sys.stderr)
m = meta("08921")
vars_ = {v["code"]:v for v in m["variables"]}
latest = vars_["Tid"]["values"][-1]
# Age codes except the aggregate 00000
age_codes = [a for a in vars_["Alder"]["values"] if a != "00000"]
# Education levels except aggregate "00"
edu_codes = [e for e in vars_["UtdanNivaa"]["values"] if e != "00"]

js = fetch("08921", {
    "Region":["0"], "Kjonn":["0"], "Alder":age_codes,
    "UtdanNivaa":edu_codes, "ContentsCode":["PersonPros"], "Tid":[latest]})
rows = parse(js)
out = os.path.join(PKG,"education_by_age.csv")
with open(out,"w",newline="",encoding="utf-8") as fh:
    w=csv.writer(fh); w.writerow(["age_band","level_code","level_no","share_pct"])
    for r in sorted(rows, key=lambda r:(r["Alder"], r["UtdanNivaa"])):
        w.writerow([r["Alder_label"], r["UtdanNivaa"], r["UtdanNivaa_label"],
                    f'{r["value"]:.2f}'])
print(f"  wrote {out} ({len(rows)} rows, year={latest})")

# ================= 2. DISABILITY BY AGE (11715) =================
print("[11715] disability share by age…", file=sys.stderr)
m = meta("11715")
vars_ = {v["code"]:v for v in m["variables"]}
latest_u = vars_["Tid"]["values"][-1]
age_codes = [a for a in vars_["Alder"]["values"] if a != "18-67"]  # drop aggregate

js = fetch("11715", {
    "Region":["0"], "Alder":age_codes,
    "ContentsCode":["UforetrygdPros"], "Tid":[latest_u]})
rows = parse(js)
out = os.path.join(PKG,"disability_by_age.csv")
with open(out,"w",newline="",encoding="utf-8") as fh:
    w=csv.writer(fh); w.writerow(["age_band","share_pct"])
    for r in sorted(rows, key=lambda r:r["Alder"]):
        w.writerow([r["Alder_label"], f'{r["value"]:.2f}'])
print(f"  wrote {out} ({len(rows)} rows, year={latest_u})")

# ================= 3. OCCUPATION SALARY + PART-TIME (11418) =================
print("[11418] occupation × salary × schedule…", file=sys.stderr)
m = meta("11418")
vars_ = {v["code"]:v for v in m["variables"]}
latest_w = vars_["Tid"]["values"][-1]
# Use 4-digit STYRK codes (407 occupations)
yrke_codes = [y for y in vars_["Yrke"]["values"] if len(y)==4 and y.isdigit()]
print(f"  {len(yrke_codes)} occupation codes", file=sys.stderr)

# We want: median & mean monthly salary, and headcount, for each (occupation x schedule).
# MaaleMetode 01=median, 02=mean, 10=headcount. ContentsCode=Manedslonn for salary;
# headcount is independent of content so we'll just use any.
js = fetch("11418", {
    "MaaleMetode":["01","02","10"],
    "Yrke": yrke_codes,
    "Sektor":["ALLE"],
    "Kjonn":["0"],
    "AvtaltVanlig":["0","5","6"],  # I alt / Heltid / Deltid
    "ContentsCode":["Manedslonn"],
    "Tid":[latest_w],
})
rows = parse(js)
# Pivot to one row per (Yrke, AvtaltVanlig) with columns median/mean/count
pivot = collections.defaultdict(dict)
for r in rows:
    key=(r["Yrke"], r["Yrke_label"], r["AvtaltVanlig"], r["AvtaltVanlig_label"])
    metric = {"01":"median_nok","02":"mean_nok","10":"headcount"}[r["MaaleMetode"]]
    pivot[key][metric] = r["value"]

schedule_map = {"0":"all", "5":"full", "6":"part"}
out = os.path.join(PKG,"occupations_salary.csv")
with open(out,"w",newline="",encoding="utf-8") as fh:
    w=csv.writer(fh)
    w.writerow(["code","label","schedule","median_nok","mean_nok","headcount"])
    for (code,label,sch,_),vals in sorted(pivot.items()):
        w.writerow([code, label, schedule_map.get(sch,sch),
                    int(vals.get("median_nok") or 0),
                    int(vals.get("mean_nok") or 0),
                    int(vals.get("headcount") or 0)])
print(f"  wrote {out} ({len(pivot)} rows, year={latest_w})")
print("\n[done]")
