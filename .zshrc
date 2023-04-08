# set history options
HISTFILE=~/.zhistfile
HISTSIZE=10000
SAVEHIST=10000

# init starship
eval "$(starship init zsh)"

# init zoxide
eval "$(zoxide init zsh)"

# load aliases
source ~/.config/aliases.zsh

# print a nice graphic
pfetch

# add .local/bin to path

export PATH="$HOME/.local/bin/:$HOME/.cargo/bin:$PATH"

# startx if we are on tty1
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then 
  exec startx 
fi
