#!/bin/bash
echo "Repo backup starting at `date`"

export PATH=/usr/local/share/python/:/sbin:/usr/local/bin/:$PATH
export HOME_DIR=/Users/channing
export DOCUMENTS=$HOME_DIR/Documents
export ARCHIVE=$DOCUMENTS/Archive/Code

# eclipsewiki svn
echo "SKIPPING Getting EclipseWiki archive from SVN"
#rsync -av eclipsewiki.svn.sourceforge.net::svn/eclipsewiki/* $ARCHIVE/eclipsewiki.svn

# GIT mirrors created with git clone --mirror https://github.com/lancewalton/treelog.git
declare -a repos=("dbcsi" "dotfiles" "eclipse-accelerate" "eclipse-saveme" "eclipse-sortit" "eclipse-subtext" "ergo" "fly-java" "fly-scala" "http4sWS" "intro-to-programming" "nash" "owl-challenge" "qanda" "questions" "rescue" "rxlift" "scala-contracts" "simple-examples" "sublime_underscore" "symplegades" "textdsl" "transaction-monad" "treelog" "typeclassopedia" "FuzzyGeneticProgrammingFX")

for repo in "${repos[@]}"
do
    echo Updating ${repo}
    git -C $ARCHIVE/${repo}.git remote update
done

# finished
echo "Repo backup Complete at `date`"