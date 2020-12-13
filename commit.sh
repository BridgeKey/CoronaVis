#!/bin/sh
cd "c:/users/frbrit/documents/r/Projects/Covid Analysis"
git init
git remote rm origin
git remote add origin https://BridgeKey:Engin33r2020%21@github.com/BridgeKey/CoronaVis
git add --all
timestamp() {
  date +"at %H:%M:%S on %d/%m/%Y"
}
git commit -am "Regular auto-commit $(timestamp)"
git push --force origin HEAD:main