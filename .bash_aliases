# sourced by .bashrc
# always a friendly reminder, lads: NO SPACES ON EITHER SIDE OF THE EQUAL SIGN FOR AN ALIAS.

alias dotgit="/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME"
alias cs310="g++ -O0 -Wall -std=c++11 -o"
alias MISSION_SHAOLIN_FOCUS_GO="nvlc -Z -L ~/Music/MISSION\ SHAOLIN\ FOCUS/0-PLAYLIST.xspf"
alias focus-timer-30="./projects/etcetera/scripts/timer.py 10 30 & disown"
alias emax="emacsclient -t" # -t is no-window
alias su-emax="sudo emacsclient -t"
alias emcl="nohup emacsclient -c > ~/nohup.out &"  # starts new gui frame & detatches from parent process (the terminal) & throws stdout/err to nohup.out
alias tomaty='/home/spook/.pyenv/versions/toma/bin/tomaty & disown'
alias getTheBLOCK="sudo cp /etc/hosts.tw-fb /etc/hosts"
