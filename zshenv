export PATH=$MY_BIN:${HOME}/Code/dev/sbt-extras:/usr/local/bin:/usr/local/sbin:$PATH
export MANPATH=/opt/local/share/man:$MANPATH

# python
export PATH=/usr/local/share/python:$PATH

# Add GHC 7.8.3 to the PATH, via http://ghcformacosx.github.io/
export GHC_DOT_APP="/Applications/ghc-7.8.3.app"
export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
