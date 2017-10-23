################################################################################
# NAME:		    .bash_aliases
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    This is just some useful stuff.
#
# CREATED:	    10/23/2017
#
# LAST EDITED:	    10/23/2017
###

alias ls='ls -A'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias python='python3'
alias emacs='/usr/bin/emacs24 -nw'
alias repo-check='/Users/ethantwardy/Git/Tools/Git-Tools/post.sh'
alias screen='screen -c /Users/ethantwardy/.screenrc'
alias svn='svn --no-auth-cache'
alias t='python ~/t/t.py --task-dir ~/Git/Tools/tasks/ --list tasks'
alias b='python ~/t/t.py --task-dir . --list bugs --delete-if-empty'

function b-update {
    BUGS=`awk -F'TODO:? ' '/(# |\/* )TODO:? /{ print FILENAME": "$2 }'\
 \`find -type f\``
    IFS="
"
    for f in $BUGS; do
	b $f
    done
}

################################################################################
