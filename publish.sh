#!/usr/bin/env zsh

githubURI="https://github.com/ArthurVard/gtk.am.git"
hakylldir=~/haskell/hakyll-sites/gtk.am
pubdir=$hakylldir/_publish

[[ ! -e $pubdir ]] && git clone $githubURI _publish

cd $hakylldir       && \
git pull            && \
./rebuild.sh        && \
cd $pubdir          && \
git checkout gh-pages && \
print -- "get latest modif from github" && \
git pull && \
print -- "Remove all files except .git" && \
\rm -rf * && \
print -- "Copy _site" && \
\cp -rf ../_site/* . && \
print -- "Adding files to repository" && \
git add . && \
git add -u && \
print -- "Commit and publish" && \
git commit -m "publishing"
git push
