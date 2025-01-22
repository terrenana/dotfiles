# set history options
HISTFILE=~/.zhistfile
HISTSIZE=10000
SAVEHIST=10000

# init zoxide
eval "$(zoxide init zsh)"

# load aliases
source ~/.config/aliases.zsh

# print a nice graphic
pfetch

# add .local/bin to path
export PATH="$HOME/.local/bin/:$HOME/.cargo/bin:$PATH"

# enable gentoo completions
autoload -U compinit promptinit
compinit
promptinit; prompt gentoo

# init starship
eval "$(starship init zsh)"

# startx if we are on tty1
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then 
  exec startx 
fi

[ -f "/Users/cruise/.ghcup/env" ] && . "/Users/cruise/.ghcup/env" # ghcup-env