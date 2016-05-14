# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# themes: lambda kolo cloud kphoen wezm sunaku pygmalion
#ZSH_THEME="lambda"
ZSH_THEME="agnoster"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(brew git git-flow github osx svn)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
# set the default user to remove my user name from the prompt
export DEFAULT_USER=channing

alias vi=vim
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias scalaz="sbt -sbt-create 'set libraryDependencies += \"org.scalaz\" %% \"scalaz-core\" % \"7.1.7\"' 'set initialCommands := \"import scalaz._; import Scalaz._\"' 'console'"
alias shapeless="sbt -sbt-create 'set libraryDependencies += \"com.chuusai\" %% \"shapeless\" % \"2.0.0\"' 'set initialCommands := \"import shapeless._\"' 'console'"

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

alias bu="brew update; brew upgrade; brew cleanup; brew linkapps"

export MY_BIN=/Users/channing/bin
export EDITOR=vim
