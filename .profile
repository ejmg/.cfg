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


## PATHS
############################################

# RUST
export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"

# HASKELL, STACK
# export PATH="$HOME/.stack/programs/x86_64-linux/ghc-8.2.1/bin:$PATH"
# stack told me to add this after installing, not sure if actually needed
# export PATH="$PATH:$HOME/.local/bin"

# Games? 
# export PATH="/usr/games:$

# android

export ANDROID_HOME=/home/spook/Android/Sdk
export PATH=$PATH:/home/spook/Android/Sdk/tools
export PATH=$PATH:/home/spook/Android/Sdk/platform-tools

# export SDKMAN_DIR="/home/spook/.sdkman"
# [[ -s "/home/spook/.sdkman/bin/sdkman-init.sh" ]] && source "/home/spook/.sdkman/bin/sdkman-init.sh"

# Ruby
export PATH="$HOME/.rbenv/bin:$PATH"
export GEM_HOME=$HOME/gems
export PATH=$HOME/gems/bin:$PATH

# set PATH so it includes user's private bin directories
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
eval "$(rbenv init -)"
