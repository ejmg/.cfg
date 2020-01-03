# sourced by .bashrc

alias dotgit="/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME"
alias emcl="nohup emacsclient -c > ~/nohup.out &"  # starts new gui frame & detatches from parent process (the terminal) & throws stdout/err to nohup.out
alias emclox="emacsclient -t"
alias tmux='tmux -2'
alias gghci='stack exec ghci' # global ghci
alias ghci='stack ghci' # local, but defaults to global if no local proj.
alias ppt-to-pdf='libreoffice --headless --invisible --convert-to pdf *ppt*'
alias batp='bat -p'
# give a query, like case-insensitive search for for: -i "foo", will then search txt output of all pdfs for "foo"
alias rg-pdftotxt='rg --pre pre-pdftotext --pre-glob "*.pdf"'
alias spotifydr='systemctl --user restart spotifyd'

# Sends a popup via KDE Plasma's notification lib.
# @param The message for the dialogue popup.
kmsg() {
    kdialog --passivepopup "$1";
}

# Makes directory and automatically changes to it.
# @param The directory to create and change to
mcdir() {
    mkdir "$1" && cd "$1";
}
