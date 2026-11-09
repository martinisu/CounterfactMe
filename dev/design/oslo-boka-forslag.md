# Forslag fra «Oslo – samme by, ulike livssjanser»

Ljunggren, J. (red.) (2026). *Oslo – samme by, ulike livssjanser.*
Cappelen Damm Akademisk. CC BY-NC-ND 4.0, åpen tilgang.

520 sider, 21 kapitler, og forfatterlista er kjernen av norsk
stratifiseringssosiologi. Lest 15. oktober 2026 med ett spørsmål: finnes det
dimensjoner eller mekanismer her som pakka mangler?

Svaret er ja, på tre punkter. Ett er droppet, to foreslås.

## Droppet: Oslo bydel som egen dimensjon

Boka dokumenterer at forventet levealder ved fødsel spenner fra **76,5 år på
Sagene til 83,2 år i Vestre Aker** (2002–2016; Heggebø, kap. 15) — 6,7 år
innad i én by. I pakka er Oslo én rad i `municipalities.csv` med 728 714
innbyggere og null treff på «bydel» i hele `conditional.R`.

Besluttet droppet. Begrunnelsen er ikke at funnet er uviktig, men at 15
bydeler ville gitt Oslo en oppløsning Bergen, Trondheim og Stavanger ikke
har. Asymmetrien ville sagt noe om hvor forskningen er gjort, ikke om hvor
forskjellene finnes.

---

## Forslag 1: Oppvekstbolig som dimensjon

### Hva pakka har i dag

`housing_tenure` finnes — men bare for ego *nå*, trukket av
`.housing_own_prob(age, income_nok, parents_capital)` som et **binært**
eier/leier. Ingen skille mellom privat og kommunal leie.

Og pakka har **ingen oppvekstforhold overhodet**: null treff på `childhood`
eller `oppvekst`. Den trekker foreldrenes nåværende kjennetegn, men aldri
respondentens barndomsvilkår.

### Hva boka viser

Aarland, Umblijs & von Simson (kap. 9, s. 201–219, doi 10.23865/cdf.304.ch9)
følger barn som var 5–16 år i Oslo i 2001 og måler utfall i 2021. Andel med
lang høyere utdanning (master+):

| oppvekstbolig | andel master+ |
|---|---|
| eid bolig, samlet | ~38 % |
| alle i Oslo i aldersgruppen | 25 % |
| kommunal bolig, samlet | ~17 % |
| eid bolig, øst | ~19 % |
| privat leie, øst | 11 % |
| kommunal leie, øst | 9 % |

19 000 barn bodde i kommunal bolig i Oslo i 2021; 87 % hadde
innvandrerbakgrunn.

### Fella, og hvordan den unngås

**Tallene over er rå assosiasjoner.** Kapittel 9 bruker trinnvis regresjon
nettopp for å pelle fra hverandre hva som er foreldrenes inntekt, utdanning
og innvandrerbakgrunn. Å legge 38 mot 17 inn som en effektstørrelse ville
vært samme feil som rammeverksvelgeren i `mechanisms.md`:

> En svakere korrelasjon kalt meritokrati er ikke en teori om meritokrati.

Riktig konstruksjon: oppvekstbolig **trekkes betinget** på det pakka alt vet
om foreldrene — utdanning, inntekt, innvandrerbakgrunn, sentralitet — og
utdanningsgradienten får falle ut av den betingede strukturen. Den påtvinges
ikke.

Da blir bokas tall en **validering** i stedet for en sirkel: produserer den
modellerte kjeden en gradient i nærheten av 38/25/17, uten at noen har lagt
den inn?

### Datakrav

Boka er Oslo-spesifikk; pakka er nasjonal. Trengs:

1. **Nasjonal fordeling av eieform for barnefamilier**, helst etter
   foreldrenes inntekt/utdanning. SSB publiserer boforhold etter
   husholdningstype. Må spores og hentes som `ssb_api`, ikke anslås.
2. **Andel kommunale leieboliger nasjonalt**, og hvordan den varierer med
   sentralitet. Oslo er en ytterlighet (flest kommunale boliger i landet) og
   kan ikke generaliseres.
3. Avklaring: skal oppvekstbolig gjelde alle aldre, eller bare kohorter der
   vi har data? Eldre kohorter vokste opp i et helt annet boligmarked.

Uten punkt 1 og 2 blir dimensjonen et anslag, og da skal den merkes slik.

---

## Forslag 2: Utdanningsbetinget foreldredødelighet

### Hva `.death_year()` gjør i dag

```r
p_dead <- if (implied_age >= 105) 1.00
          else if (implied_age >= 95) 0.95
          ...
```

Ren aldersplan. Ingen sosioøkonomi, ingen geografi.

Merk at funksjonen ligger **inne i `.cond_parents()`** og bare brukes på mor
og far — aldri på ego, som er i live per konstruksjon. Det gjør forslaget
smalere enn det høres ut, men ikke mindre viktig: om foreldrene lever
påvirker `parents_capital`, `parents_both_dead` og arv, og dermed formue
nedstrøms. Det er også noe av det mest merkbare i en narrativ.

### Hva boka viser

Heggebø (kap. 15, s. 349–382, doi 10.23865/cdf.304.ch15) analyserer
registerdata for hele Oslos befolkning 30–80 år i to tiårsperioder, og finner
at varig eksponering for sosioøkonomiske ressurser — arbeidstilknytning,
husholdningsinntekt, formue, bolig — predikerer dødsrisiko sterkt.

Et funn verdt å merke seg: bydelsvariasjonen i *krefthyppighet* er liten,
mens variasjonen i *kreftdødelighet* er stor. Flere blir ikke syke i øst —
flere dør av det. Det peker mot materielle og behandlingsmessige forhold, ikke
livsstil alene.

### Fella

Bokas gradient er **geografisk** (bydel), og kapittelet handler nettopp om å
skille geografi fra sammensetning. Å bruke 6,7-årsgapet mellom Sagene og
Vestre Aker som en SES-effekt ville vært å gjenta feilen i annen drakt.

Riktig kilde er **SSBs nasjonale forventet levealder etter utdanningsnivå**,
som er personnivå og direkte anvendbar på `.death_year()`. Boka begrunner
*at* gradienten finnes og er stor; SSB gir tallet.

### Datakrav

1. SSB: forventet levealder etter utdanningsnivå og kjønn. Må hentes og
   legges i `inst/extdata/` med `data-raw/`-skript, som formue og inntekt.
2. Beslutning: gradienten gjelder foreldrenes *egen* utdanning, som pakka
   allerede trekker. Ingen ny dimensjon kreves — bare en betingelse på en
   eksisterende.

Det gjør dette til det billigste av de to forslagene.

---

## Et sidefunn som treffer åpent arbeid

Wiborg & Hansen (kap. 3) rapporterer at **33 % av dem med topp 1 %
bruttoformue bor i Oslo**, og ytterligere 11 % i Asker og Bærum. Altså bor
44 % av landets formuestopp i tre kommuner.

Pakkas formueshale (`wealth_tail_person.csv`) er **nasjonal og
alderskondisjonert, men ikke geografisk**. Det betyr at en trukket person i
Vadsø har samme sannsynlighet for å være i formuestoppen som en i Oslo.

Dette henger sammen med kalibreringsfeilen i
`dev/design/kalibrering-2026-09-19.md`, men er ikke den samme feilen — den
handler om nivå, denne om geografisk fordeling. Begge må løses før
formuestall kan brukes aggregert.

---

## Rekkefølge

1. **Utdanningsbetinget foreldredødelighet.** Billigst: én SSB-tabell, ingen
   ny dimensjon, kobler seg på data pakka alt trekker.
2. **Geografisk betinging av formueshalen.** Henger sammen med den åpne
   kalibreringsfeilen og bør vurderes i samme omgang.
3. **Oppvekstbolig.** Størst gevinst sosiologisk, men krever nasjonale data
   som ikke er sporet ennå, og en avklaring om kohortdekning.

Ingen av dem bør bygges før `population_calibration()` går grønt for formue
— ellers legger vi nye mekanismer oppå et grunnlag vi vet er skjevt.

---

# Hva som faktisk ble bygget (15. oktober 2026, v0.9.72)

## Korreksjon av dette notatet

Over står det at SSB publiserer forventet levealder etter utdanningsnivå, og
at dødelighetsforslaget derfor var «billigst». **Den tabellen finnes ikke i
Statistikkbanken** — sjekket 15. oktober. Nærmeste er en SSB-artikkel med
11–12 års gap mellom yrkesgrupper, men fra 1996–2000 og 2009–2013.

Det som finnes er bedre: **SSB 14569, forventet gjenstående levetid** fra
faktisk alder, etter enkeltår 0–99, kjønn og sentralitet. 2025-tall.

## Bygget

**Nivå, SSB 14569.** Kjønnsgap 3,2 år, sentralitet 2,2 (menn) / 1,2 (kvinner).
`.death_year()` kjønnsjusteres nå — mor og far døde før etter identisk skjema,
og det var den største enkeltfeilen. Målt etterpå: far død i 85,6 % av
trekkene mot mor i 74,8 %.

**Spredning, Kinge et al. (JAMA 2019).** 13,9 år mellom p1 og p99 for menn,
8,4 for kvinner. Null-sum, så nivået forblir SSBs: målt 84,53 uten gradient
mot 84,62 med.

Nøyaktig **én** gradient, gjennom inntekt. `.cond_health()` og
`.cond_alcohol()` betinger begge på utdanning, så helse og alkohol som egne
ledd ville talt samme utdanning om igjen.

## Realisert spenn: 4,9 år, og hvorfor det ikke er en feil

800 trekk, alder 30–60, begge kjønn. Dekomponert:

| | spenn |
|---|---|
| kvartilmedianer havner på p19 og p85, ikke p1/p99 | 6,8 |
| aldersavtrapping (snittfaktor 0,78 over 30–60) | ~5,3 |
| blandet kjønn, kvinners gradient flatere | 4,9 |

Hvert ledd er som det skal. Gradienten fyrer i 800 av 800 trekk.

**Men toppen brukes aldri.** Den trukne inntektsfordelingen når ikke p99 —
`.cond_income()` produserer ingen over 2 mill. En person i reell topp 1 %
ville fått +7 år; sampleren kan ikke lage en. Det er inntektskalibreringen
som slår gjennom, se `kalibrering-2026-09-19.md`. Gradienten blir riktigere
når den fikses, uten at noe her må endres.

## Forbehold som må stå i UI-et, ikke bare her

**Omvendt kausalitet.** Kinges forfattere peker på at kronisk sykdom forklarer
mye av gapet — de fattigste har delvis lav inntekt *fordi* de er syke. Dette er
verst i ytterpunktene, altså nettopp der tallet er mest slående.

**Inntekt er proxy for hele bunten**, ikke en påstand om at penger forlenger
liv.

**Marginal sammenheng, ikke spådom.** Gjennomsnitt for folk med disse
kjennetegnene.

**Vintage.** Kinge er 2005–2015, og gapet vokste i perioden.

**Foreldrenes inntektsproxy er yrkets medianlønn**, ikke deres egen inntekt —
alle i samme yrke får samme tall, så spredningen komprimeres ytterligere der.

## Fortsatt ikke gjort

Oppvekstbolig. Krever nasjonale tall som ikke er sporet.
