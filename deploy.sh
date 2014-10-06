#! /bin/sh

site clean && site build &&
  rsync -avz ../mrordinaire.github.io/.git _site/ &&
  rsync -avz --delete _site/ ../mrordinaire.github.io &&
  pushd ../mrordinaire.github.io &&
  git add -A &&
  git commit -m "updated at `date +%Y-%m-%dT%H:%M:%S`" &&
  popd
