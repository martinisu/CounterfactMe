# Historisk modus

Notat om hvorfor «hvilket parti fikk viljen sin»-velgeren ble avvist, og hva
som kom i stedet.

## Utgangspunktet

Forslaget var å la brukeren stille inn et samfunn der de ulike partiene på
høyre–venstre-aksen hadde fått gjennomslag, og se hvordan livet endret seg.

Det ble avvist av samme grunn som rammeverksvelgeren i `mechanisms.md`:

> En svakere korrelasjon kalt meritokrati er ikke en teori om meritokrati.

En «FrP»-parameter som senker innvandringsandel og marginalskatt er ikke FrPs
politikk. Den er to tall noen har valgt, med et partinavn på. Det finnes ingen
registerdata som forteller hva et norsk liv hadde blitt under et kontrafaktisk
partiflertall — det er ikke et datasettproblem, det ligger utenfor rekkevidde.

I tillegg: en simulator bygget for å vise én velgergruppe at deres politikk gir
dårligere liv er et partsinnlegg, og blir lest som det. Det ville brenne den
kredibiliteten `SOURCES.csv` og provenansstatusene er der for å beskytte.

## Hva som kom i stedet

Norge har allerede kjørt eksperimentet. Den kontrafaktiske som faktisk
opplyser er den historiske: samme person, født i et annet Norge.

Ingen partinavn. Ingen antatt politikk. Bare de faktiske fordelingene, slik de
var.

## Fella: samme alder eller samme observasjonsår?

Dette er det avgjørende designvalget, og det er lett å ta feil.

Pakka er forankret i `ref_year` (registerdata for inneværende år). Å endre
fødselsår der betyr å endre **dagens alder**. En person «født i 1935» blir
dermed observert som 91-åring: pensjonist, ofte enke, ferdig med boligkarrieren.

Det er en ekte registersammenlikning, men den er ubrukelig til formålet, fordi
alderseffekt og kohorteffekt er fullstendig sammenfiltret. Leseren ser ikke
1935-Norge, hun ser alderdom.

Det som trengs er motsatt: **fast observasjonsalder, variabelt fødselsår.**
Hvordan så livet ut ved 40 for en født i 1935, mot en født i 1980?

Med den vrien følger en grense som er lett å overse: en kohort kan bare
beskrives ved 40 hvis den *har fylt* 40. Født 1995 er 31 i dag, og registeret
kan ikke si hva hun har ved 40 — det har ikke skjedd. Første versjon merket
31-åringen som «sett ved 40 år». `counterfact_era_lives()` dropper nå slike
kohorter med advarsel, og en test vokter det.

## Hvilke dimensjoner har vi faktisk æra-data for?

Bare to overlever. Modusen dekker disse og ikke flere.

| Dimensjon | Mekanisme | Kilde | Status |
|---|---|---|---|
| Utdanningsnivå | registerbånd × `.edu_cohort_tertiary_mult()` (0,11 før 1920 → 1,0 fra 1965) | SSB 08921 + historisk utdanningsstatistikk | **sammensatt** — se oppløsningsfella |
| ~~Yrke~~ | *overlever ikke overføringen — se under* | — | **ikke brukt** |
| Boligpris | indeks 1920–2025 | SSB 07230 + KPI 08981, `real_factor` som anslag | forankret, anslag dokumentert |

### Yrke falt ut, og det er lærerikt

Yrkesstruktur *finnes* som æra-data (`.styrk_cohort_mult()`, ankere 1955 og
1980). Men metoden over virker bare for kjennetegn som ligger fast etter at de
er satt. Utdanning gjør det — den er ferdig ved 30, så en 91-årings
registrerte utdanning *er* utdanningen hennes ved 40.

Yrke gjør det ikke. Slår du opp yrket til en 91-åring får du «pensjonist», ikke
jobben hun hadde i 1975. Første versjon av `era.R` listet yrke som forankret.
Det var feil, og det er samme feilklasse som Elsa-tilfellet: et kjennetegn hentet
fra ett tidspunkt og presentert som om det gjaldt et annet.

For å få yrke inn må det *trekkes* fra æraens fordeling via
`.styrk_cohort_mult()`, ikke leses av personen. Det er neste steg, og
multiplikatorene ligger allerede på resultatet som `$styrk_mult`.

**Ikke dekket, og skal ikke simuleres:** inntektsfordeling per æra, familie­dannelse,
helse, holdninger, tillit. Vi har dagens fordelinger for disse og ingenting
historisk. Å skalere dem med en gjettet faktor ville vært nøyaktig den feilen
notatet åpner med.

Felt utenfor de to skal derfor merkes eksplisitt i output, ikke stilltiende
arves fra dagens fordeling.

## Hva som holdes fast

«Samme person» kan ikke bety samme absolutte utdanning — en mor med mastergrad
i 2026 hadde ikke hatt mastergrad i 1935, for da fantes den knapt for kvinner.
Det som holdes fast er **relativ posisjon**: foreldrenes plassering i sin egen
kohorts fordeling, ikke etiketten.

Holdes fast: kjønn, region, foreldrenes utdanningsrangering, innvandrerbakgrunn.
Redrawes: alt nedstrøms, betinget av de faste, i den nye æraens fordelinger.

## Oppløsningsfella — funnet som endret designet

Første kjøring, 18. september, avslørte at kjernepåstanden over ikke holdt.

SSBs utdanningstabell er inndelt i åtte aldersbånd, og det øverste er
«67 år eller eldre». I 2026 dekker det **fødselsårene 1926–1959 — 34 kohorter
i ett tall.** Lest rett av registeret er en født i 1935 og en født i 1950
dermed *samme trekk*. Forskjellen mellom dem er ren samplingstøy.

Demoen viste det tydelig: gjennomsnittlig NUS-kode kom ut som 3,73 (1935),
4,70 (1950), 4,27 (1965), 4,93 (1980) — altså ikke monotont, fordi 1965 ligger
under 1950.

Og nivået var galt. Båndet «67+» gir 27,5 % med høyere utdanning. Pakkens egen
`.edu_cohort_tertiary_mult()` sier at 1935-kohorten lå rundt 10 % og
1950-kohorten rundt 18 %. Registeret alene ga altså 1935 nesten tre ganger for
mye utdanning.

### Løsningen: båndet gir nivået, multiplikatoren gir vippen

`.demote_edu_for_cohort()` brukes allerede på foreldre, nettopp fordi
foreldrenes alder ikke er et registerfaktum. Ego i era-modus er i samme
situasjon: vi spør om en kohort, ikke om en levende registerperson. Samme
justering brukes derfor nå på ego.

**Det endrer provenansen, og det skal stå tydelig:** utdanning i era-modus er
*registerbånd × dokumentert kohortmultiplikator*, ikke et rent registeruttrekk.
Det rå båndtrekket beholdes som `$education_band` slik at begge kan ses.

Multiplikatoren rører ikke kohorter fra 1965 og senere (den er 1), så fiksen
treffer bare der båndene faktisk er for grove. Tre tester vokter det: at
kohorter innenfor samme bånd nå skilles, at justeringen bare kan senke nivået,
og at unge kohorter står urørt.

## Den svakheten som gjenstår: overlevelsesseleksjon

Metoden leser 1935-kohorten av dagens 91-åringer. Men de 91-åringene er de som
*fortsatt lever*, og overlevelse til 91 er positivt selektert på utdanning og
inntekt. Den registrerte utdanningen til dagens 91-åringer ligger derfor over
det 1935-kohorten faktisk oppnådde.

Retningen er kjent: skjevheten **undervurderer** utdanningsekspansjonen. Den
sanne forskjellen mellom 1935 og 1980 er større enn appen viser, ikke mindre.
Det gjør den trygg å vise — vi overdriver ikke poenget — men det bør stå i UI-et
for de eldste kohortene.

Merk at dette er en *annen* og mindre feil enn båndsammenslåingen over. Da jeg
først skrev dette notatet tilskrev jeg hele skjevheten overlevelsesseleksjon.
Det var for pent: dominerende årsak var at 34 fødselsår delte ett tall.
Seleksjonen står igjen etter at den er fikset, men den er den minste av de to.

Skjevheten vokser med alder og er ubetydelig under ca. 70 år. Standardvalget
1935/1950/1965/1980 har den derfor bare i den første kohorten.

Å korrigere krever dødelighetstabeller etter utdanning per kohort. Det er
mulig fra SSB, men det er ikke gjort, og til det er gjort skal tallet ikke
presenteres som 1935-kohortens utdanning — bare som de overlevendes.

## Andre trinn: tallfestede politikk-knapper

Utsatt til historisk modus er bygget og evaluert. Hvis de fortsatt virker
nødvendige, er den forsvarlige formen navngitte, tallfestede parametere
(marginalskatt, barnetrygd, innvandringskvote) som brukeren setter selv, og som
leseren kan mappe et hvilket som helst partiprogram på. Da tar ikke appen
stilling.

Krav hvis det bygges:
- ny provenansstatus `scenario` på alt nedstrøms
- UI må si rett ut at å dempe én kanal ikke isolerer den: kutter du innvandring
  i modellen får du ikke «Norge uten innvandring», du får dagens Norge med
  færre innvandrere. Arbeidstilbud, næringsstruktur og fruktbarhet henger
  ikke med.
