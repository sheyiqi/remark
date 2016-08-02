set prompt = "%{\033[0m%}%{\033[42;1;35m%}{%!}%{\033[33m%}%U%M%u:%B%c02%b %% %{\033[0m%}"
if ( $?prompt ) then
    set history=256
    set savehist=64
    set filec
    set ignoreeof
    set autolist
    set symlinks=ignore
    set color
    set colorcat
    set nobeep
endif

source ~/.csh_aliases
source ~/.csh_eterm
