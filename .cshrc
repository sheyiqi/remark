#set prompt = "%{\033[0m%}%{\033[42;1;35m%}{%!}%{\033[33m%}%U%M%u:%B%c02%b %% %{\033[0m%}"
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


set os = `uname`
set host = `hostname`
set date = `date`

# su does not change this but I'd like it to

set user = `whoami`
# ...

if ( $TERM == eterm-color ) then
   echo --------------------------------------------------------------
   echo Hello $user
   echo Today is $date
   echo We are on $host running $os under Emacs term mode
   echo --------------------------------------------------------------

   setenv EDITOR emacsclient

 # Notice: $host and $user have been set before to 'hostname' and 'whoami'
 # this is necessary because, f.e., certain versions of 'su' do not change
 # $user, YMMV: if you don't want to fiddle with them define a couple
 # of new variables and use these instead.
 # NOTICE that there is a space between "AnSiT?" and $whatever NOTICE

 # These are because we want the real cwd in the messages, not the login
 # time one !

   set cwd_hack='$cwd'
   set host_hack='$host'
   set user_hack='$user'

 # Notice that the ^[ character is an ESC, not two chars.  You can
 # get it in various ways, for example by typing
 # echo -e '\033' > escape.file
 # or by using your favorite editor

   foreach temp (cd pushd)
      alias $temp "$temp \!* ; echo 'AnSiTc' $cwd_hack"
   end
   alias popd 'popd ;echo "AnSiTc" $cwd'

 # Every command that can modify the user/host/directory should be aliased
 # as follows for the tracking mechanism to work.

    foreach temp ( rlogin telnet rsh sh ksh csh tcsh zsh bash tcl su )
       alias $temp "$temp \!* ; echo 'AnSiTh' $host_hack ; \
                echo 'AnSiTu' $user_hack ;echo 'AnSiTc' $cwd_hack"
    end

 # Start up & use color ls
    echo "AnSiTh" $host
    echo "AnSiTu" $user
    echo "AnSiTc" $cwd

 # some housekeeping
    unset cwd_hack
    unset host_hack
    unset user_hack
    unset temp
    eval `dircolors ~/.emacs_dircolors`
endif

# ...

# Let's not clutter user space
set my_os = $os
unset os
unset date

source ~/.csh_aliases

