#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export JAVA_HOME=<JAVA_DIR>

export PATH=$PATH:~/bin:~/bin/commands:$JAVA_HOME/bin
alias vi=vim
alias c360="cclive -s medium_mp4_i18_360p"
alias emacs="emacs -nw"
alias settings="cd <SETTINGS-DIR>"
alias today="echo ""; date +\"%Y-%m-%d %A %I:%M %p\"; echo ""; echo ""; cal"

[[ -z "$TMUX" ]] && exec tmux

# transmission daemon 
tsm() { transmission-remote -l; }
tsm-pause() { transmission-remote -t "$1" --stop; }
tsm-start() { transmission-remote -t "$1" -s; }
tsm-enablealtspeed() { transmission-remote --alt-speed; }
tsm-disablealtspeed() { transmission-remote --no-alt-speed; }
tsm-purge () { transmission-remote -t "$1" --remove-and-delete; }
tsm-remove () { transmission-remote -t "$1" -r; }
tsm-info () { transmission-remote -t "$1" -i; }
tsm-speed () { transmission -t"$1" -i | grep speed; }
tsm-add () { 
	transmission-remote -a "$1"; 
}
