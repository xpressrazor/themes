#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export JAVA_HOME=<JAVA-DIR>

export PATH=$PATH:~/bin:$JAVA_HOME/bin
alias vi=vim
alias c360="cclive -s medium_mp4_i18_360p"
alias emacs="emacs -nw"


[[ -z /tmp/tmux-1000/default,1584,11 ]] && exec tmux
