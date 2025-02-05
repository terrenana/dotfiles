alias cd="z"
alias cat="bat"
alias grep="rg"
alias edemon="/usr/bin/emacs --daemon"
alias c="clear"
alias eshutdown="doas emerge --verbose --deep --newuse --update --with-bdeps=y @world | tee emerge-log.log ; doas shutdown -h now"
alias vim="nvim"

if [[ $(uname) == "Darwin" ]]; then
    alias ls="exa -l"
    alias la="exa -la"

elif command -v emerge > /dev/null; then
    alias ls="eza -l"
    alias la="eza -la"
	alias emacs="devour emacsclient -c -a emacs"
fi
