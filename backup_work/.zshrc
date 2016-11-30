#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    echo "found prezto"
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi
echo "zsh done"

# Customize to your needs...

alias spcmacs="LC_CTYPE=zh_CN.UTF-8 emacs"
