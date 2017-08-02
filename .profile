# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# PYTHON CONFIGURATIONS

export PATH="/home/spook/.pyenv/bin:$PATH"
export WORKON_HOME="/home/spook/.pyenv/"


# android

export ANDROID_HOME=/home/spook/Android/Sdk
export PATH=$PATH:/home/spook/Android/Sdk/tools
export PATH=$PATH:/home/spook/Android/Sdk/platform-tools

# ruby

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/spook/.sdkman"
[[ -s "/home/spook/.sdkman/bin/sdkman-init.sh" ]] && source "/home/spook/.sdkman/bin/sdkman-init.sh"
PATH="/usr/games:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"

