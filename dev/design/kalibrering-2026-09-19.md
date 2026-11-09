# Første kalibrering: hva den fant

`population_calibration()` kjørt på 1500 liv, 19. september 2026.
Begge dimensjoner feiler, og **spådommen min var feil motsatt vei.**

Jeg skrev i `NESTE-KJORING.md`: *«formue vil treffe godt — den kommer fra 08589
via `.cond_wealth()`, altså samme kilde som fasiten. Inntekt vil trolig bomme
mer.»* Resultatet: formue bommer med 0,46 i absolutt andel, inntekt med 0,08.

Feilslutningen er verdt å notere: **samme kilde garanterer ingenting når det
skjer transformasjoner nedstrøms.** Bracket-trekket kommer fra registeret, men
alt som skjer etter det er pakkelogikk som aldri har vært holdt mot registeret.

## Formue: 0,46 i avvik

| intervall | SSB | trukket |
|---|---|---|
| ≤0 | 43,1 % | **13,7 %** |
| 2 mill.+ | 13,1 % | **59,6 %** |

Median trukket nettoformue er 2,81 mill. Med 43 % under null skal
personmedianen ligge under en halv million.

### Årsak 1: `00-`-overstyringen (R/conditional.R ~2356)

```r
if (identical(bracket, "00-")) {
  if (re_wealth > 200000L) {
    nw <- re_wealth - extra_debt   # blir POSITIV, ofte millioner
  } else {
    nw <- -exp(...)
  }
}
```

En person trukket til «≤0» som eier bolig blir konvertert til positiv
nettoformue lik boligegenkapitalen minus moderat gjeld.

Kommentaren begrunner det slik: *«if ego has substantial real estate equity,
pure negative wealth is implausible — they have collateral.»*

**Den begrunnelsen er feil.** SSBs 43,1 % med ≤0 skattepliktig nettoformue
*inkluderer allerede boligeiere*. Nettoformue er eiendeler minus gjeld, og en
som nylig har kjøpt bolig med stort lån har reelt negativ nettoformue. Det er
den normale situasjonen i Norge, ikke en implausibilitet. Overstyringen retter
et faktum som ikke er galt.

### Årsak 2: verdsettingsrabatten anvendes aldri

SSB 08589 er **skattepliktig** nettoformue. Primærbolig verdsettes til 25 % av
markedsverdi — de 75 prosentene ligger i `policy_parameters.csv` som
`rabatt_primaerbolig = 0.75`, verifisert mot Skatteetaten, og brukes ingen
steder i sampleren.

`housing_equity_nok` mates inn som markedsegenkapital. Hver boligeier blir
dermed omtrent 4× for formuende målt mot registeret.

De to feilene forsterker hverandre: overstyringen flytter folk fra ≤0 til
positivt, og den manglende rabatten gjør det positive beløpet firedoblet.

## Inntekt: 0,08 i avvik

Trukket fordeling er klemt mot midten — for mange i 200–400k, for få i
500–750k og oppover — og **null trekk over 2 mill.** mot SSBs 1,79 %.

Årsak: `.cond_income()` har ingen kapitalinntekt. Null treff på `capital` i
hele funksjonen. SSBs bruttoinntekt inkluderer kapitalinntekt, og det er der
toppen av inntektsfordelingen bor. Lønn alene har et tak.

Merk at `.cond_wealth()` *beregner* `capital_income_nok` og legger det på
livet — men inntekten er allerede satt da, så tallet brukes ikke til noe.

## Hva dette betyr

Ingen fordelingseffekt kan regnes på denne populasjonen ennå. Det var nettopp
det kalibreringen skulle avgjøre, og svaret er nei — foreløpig.

Rekkefølge for neste økt:

1. **Verdsettingsrabatten.** Minst inngripende, klarest begrunnet, og
   parameteren er allerede verifisert i katalogen.
2. **`00-`-overstyringen.** Krever en beslutning, ikke bare en fiks: skal
   negativ nettoformue for boligeiere tillates? Registeret sier ja.
3. **Kapitalinntekt inn i `.cond_income()`.** Større jobb, og den må trekkes
   *før* inntekten settes, ikke etter.

Kalibreringen kjøres på nytt etter hver, som en regresjonstest.
