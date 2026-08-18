# Structural frameworks as mechanisms — deferred

Status: **not built, and should not be built yet.** This records why the
web app's framework selector was removed and what a defensible version
would require.

## What was there

A selector offering five theoretical frameworks: pure chance,
meritocracy, functionalism, Bourdieusian class society, Rawls' veil.

## Why it was removed

**It did not work in R mode.** `cfm-translate.js` hardcoded
`frame: 'funksjonalisme'` on every draw routed to R, so the selector
changed labels and nothing else. The frameworks existed only in the
JavaScript sampler, which runs for the few seconds before webR loads.

**Where it did work, it was one mechanism at five strengths.** A single
parameter, `frameInheritance`, scaled the parent-child correlation:

| framework | frameInheritance |
|---|---|
| pure chance | 0.00 |
| Rawls' veil | 0.00 |
| meritocracy | 0.15 |
| functionalism | 0.30 |
| Bourdieu | 0.65 |

Pure chance and Rawls' veil were numerically identical. If two
frameworks are meant to describe different social worlds and produce the
same distribution, they are not two frameworks. And a weaker correlation
labelled "meritocracy" is not a theory of meritocracy — it is the same
transmission, turned down.

## What a defensible version needs

It belongs in the R package, not the app, and the modes have to be
different mechanisms rather than one dial in five positions.

```r
counterfact_me(mechanism = c("observed", "no_transmission",
                             "cultural_capital", "economic_capital"))
```

- **`observed`** — the current conditional draw. Default.
- **`no_transmission`** — parent-to-child channels cut explicitly:
  education, occupation, wealth and geography drawn marginally. This is
  the one counterfactual that is straightforwardly interpretable: what
  the distribution would look like with no transmission at all.
- **`cultural_capital`**, **`economic_capital`** — each dampens one
  channel: parental education against the child's education, parental
  wealth against the child's wealth. A decomposition rather than a menu
  of theories, and defensible empirically.

Philosophers' names belong in the documentation as references, not as
mode names in the interface.

## Caveats that would need answering first

**Dampening one channel does not isolate it.** The sampler draws in a
fixed order — parental education shapes the ego's education, which
shapes occupation, which shapes income. Cutting only the
education-to-education link leaves income still inheriting through
occupation. The number a user would read off as "the cultural channel"
is not that channel's total contribution, and presenting it as such
would be worse than not offering it.

**`no_transmission` is not free either.** The conditional chain is what
keeps lives coherent. Breaking the parental links while leaving the rest
intact will push more draws into the impossibility filter, and the
resulting distribution is not simply "the same world without
inheritance" — it is a different generative process whose artefacts need
checking.

**This is a do-operator on a DAG.** Saying that plainly would help:
it makes clear which quantities are identifiable and which are not, and
it connects the feature to a literature rather than inventing a private
vocabulary.

**There is already a related feature.** `counterfact_parallel_lives()`
varies one dimension and holds the rest. The mechanism argument is a
generalisation of that idea, and the two should be designed together
rather than arriving as separate concepts.

## The prior question

None of this should be built until someone decides what the app is for:

- *What does a random Norwegian life look like?* — which is what it does
  now, and it works.
- *How much of a life is due to background?* — a different and much
  heavier question.

The first is finished. The second is a research project.
