# sourced by .bashrc
# always a friendly reminder, lads: NO SPACES ON EITHER SIDE OF THE EQUAL SIGN FOR AN ALIAS.

alias dotgit="/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME"
# alias MISSION_SHAOLIN_FOCUS_GO="nvlc -Z -L ~/Music/MISSION\ SHAOLIN\ FOCUS/0-PLAYLIST.xspf"
alias emcl="nohup emacsclient -c > ~/nohup.out &"  # starts new gui frame & detatches from parent process (the terminal) & throws stdout/err to nohup.out
alias emclox="emacsclient -t"
alias signal-desktop='nohup signal-desktop --disable=gpu &'
alias slack='nohup slack --disable-gpu &'
alias toma='nohup /home/spook/.pyenv/versions/toma/bin/tomaty &'
alias ghci='stack exec -- ghci'

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
