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
plugins=(brew git github osx)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
# set the default user to remove my user name from the prompt
export DEFAULT_USER=channing

alias vi=vim
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias up='cd ..'

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

alias bu="brew update; brew upgrade; brew cleanup; cd ~/dotfiles; ./update.sh"

alias scalactags="ctags -R . --exclude=target --exclude=vendor"

export MY_BIN=/Users/channing/bin
export EDITOR=vim

fpath=(/usr/local/share/zsh-completions $fpath)

# Limits - see https://gist.github.com/tombigel/d503800a282fcadbee14b537735d202c
ulimit -n 200000
ulimit -u 2048
