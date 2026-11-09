#!/bin/sh
# Committer alt unntatt politikk-katalogen. Kjoeres fra hvor som helst.
# Bruk: sh dev/commit.sh [meldingsfil]   (standard: dev/commit-melding.txt)
set -e
cd "$(dirname "$0")/.."
MSG="${1:-dev/commit-melding.txt}"
[ -f "$MSG" ] || { echo "STOPP: finner ikke $MSG"; exit 1; }

if [ -e .git/index.lock ]; then
  echo "STOPP: .git/index.lock finnes. Fjern den og kjoer igjen."
  exit 1
fi

git add -A
git reset -q -- inst/extdata/policy_parameters.csv dev/design/policy-mode-plan.md

# SOURCES.csv staged UTEN policy-raden. Ellers feiler manifest-testen i CI
# (fila listes, men finnes ikke i repoet). Lokal fil beholder raden.
blob=$(grep -v '^policy_parameters\.csv,' inst/extdata/SOURCES.csv | git hash-object -w --stdin)
git update-index --cacheinfo 100644,"$blob",inst/extdata/SOURCES.csv

if git diff --cached --name-only | grep -qi polic; then
  echo "STOPP: noe med 'polic' i navnet ligger i staging:"
  git diff --cached --name-only | grep -i polic
  exit 1
fi
if git show :inst/extdata/SOURCES.csv | grep -q '^policy_parameters'; then
  echo "STOPP: policy-raden ligger fortsatt i staget SOURCES.csv"
  exit 1
fi

echo "Staget:"
git diff --cached --stat | tail -1
echo
echo "Holdt utenfor (skal bare vaere politikk-filene + lokal SOURCES.csv):"
git status --short | grep -v '^[MADR] ' || true
echo

git commit -q -F "$MSG"
git log --oneline -1
echo
echo "Ser det riktig ut? Da: git push"
