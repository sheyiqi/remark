alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias df='df -h'
alias du='du -h'

alias less='less -r'                          # raw control characters
alias whence='type -a'                        # where, of a sort
alias grep='grep --color'                     # show differences in colour
alias egrep='egrep --color=auto'              # show differences in colour
alias fgrep='fgrep --color=auto'              # show differences in colour

alias ls='ls -hF --color=tty'                 # classify files in colour
alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'
alias ll='ls -l'                              # long list
alias la='ls -A'                              # all but . and ..
alias l='ls -CF'                              #

alias ..='cd ..'
alias h=history
alias viim=vim
alias ee='emacsclient -c -a gvim \!* &'
alias what='ps aux |grep $USER |less'

alias zz='zile'

alias ssh='ssh -X'

alias tl='tmux ls'
alias ta='tmux attach -t'
alias tca='tmux -c \!:1 attach -t \!:2'

