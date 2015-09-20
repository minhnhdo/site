#! /bin/sh

stack exec site clean && stack exec site build &&
  rsync -avz ../mrordinaire.github.io/.git _site/ &&
  rsync -avz --delete _site/ ../mrordinaire.github.io &&
  echo $(cd ../mrordinaire.github.io &&
         git add -A &&
         git commit -m "updated at `date +%Y-%m-%dT%H:%M:%S`" &&
         git push origin master)
