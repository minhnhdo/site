#! /bin/sh

stack exec site clean && stack exec site build &&
  rsync -avz ../minhnhdo.github.io/.git _site/ &&
  rsync -avz --delete _site/ ../minhnhdo.github.io &&
  echo $(cd ../minhnhdo.github.io &&
         git add -A &&
         git commit -m "updated at `date +%Y-%m-%dT%H:%M:%S`" &&
         git push origin master)
