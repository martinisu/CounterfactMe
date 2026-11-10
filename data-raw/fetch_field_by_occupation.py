#!/usr/bin/env python3
"""Regenerate inst/extdata/field_by_occupation_edu.csv from SSB 12849:
employed persons aged 20-66 by 2-digit field of study (NUS fagfelt),
2-digit STYRK-08 occupation and education level, 4th quarter 2025.

Why
---
.cond_nus_field() drew the field of study from nus_by_styrk.csv, which is
keyed on the 2-digit occupation only, with small hand-set nudges for
education level. Group 22 (medisinske yrker) mixes doctors and nurses,
so a bachelor nurse could get "medisin" and a VGS-level person "videre-
utdanning for laerere". On the live site: "bachelorgrad i medisin,
legespesialistutdanning" for an occupational therapist; "farmasi" for a
nurse. 12849 gives the joint distribution of field, occupation and
education level, so the field is drawn as P(fagfelt | yrke, nivaa).

Education levels as in 12849: 1 grunnskole, 2 vgs/fagskole (NUS 3-5),
3 universitet 1-4 aar (NUS 6), 4 over 4 aar (NUS 7-8), 0+9 uoppgitt.
"""
import csv, json, os, urllib.request

here = os.path.dirname(os.path.abspath(__file__))
out = os.path.join(here, "..", "inst", "extdata", "field_by_occupation_edu.csv")
URL = "https://data.ssb.no/api/v0/no/table/12849"
meta = json.load(urllib.request.urlopen(URL))
var = {v["code"]: v for v in meta["variables"]}
yrke2 = [c for c in var["Yrke"]["values"] if len(c) == 2 and c.isdigit()]
fag2 = [c for c in var["Fagfelt"]["values"] if len(c) == 2 and c.isdigit()]
EDU = ["1", "2", "3", "4", "0+9"]
q = {"query": [
    {"code": "UtdNivaa", "selection": {"filter": "item", "values": EDU}},
    {"code": "Fagfelt", "selection": {"filter": "item", "values": fag2}},
    {"code": "Yrke", "selection": {"filter": "item", "values": yrke2}},
    {"code": "Alder", "selection": {"filter": "item", "values": ["20-66"]}},
    {"code": "Tid", "selection": {"filter": "item", "values": ["2025"]}}],
    "response": {"format": "json-stat2"}}
req = urllib.request.Request(URL, data=json.dumps(q).encode(),
                             headers={"Content-Type": "application/json"})
d = json.load(urllib.request.urlopen(req))
def order(dim):
    ix = d["dimension"][dim]["category"]["index"]
    return sorted(ix, key=ix.get) if isinstance(ix, dict) else ix
U, F, Y = order("UtdNivaa"), order("Fagfelt"), order("Yrke")
rows = []
for ui, u in enumerate(U):
    for fi, f in enumerate(F):
        for yi, y in enumerate(Y):
            v = d["value"][(ui * len(F) + fi) * len(Y) + yi]
            if v:
                rows.append([y, u, f, int(v)])
with open(out, "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh)
    w.writerow(["styrk2", "edu_level", "fagfelt2", "count"])
    w.writerows(rows)
print(f"wrote {out}: {len(rows)} rows")
