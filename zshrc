if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
else

    if [[ "$TTY" = "/dev/tty1" ]]; then
	startx
    fi	
    
    
    if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
	source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
    fi

    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

    export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/andreas/bin"
    export EDITOR='emacsclient -s /tmp/emacs1000/server -c -a ""'
    export VISUAL='emacsclient -s /tmp/emacs1000/server -c -a ""'
    export LANG=en_GB.UTF-8
    export LC_ALL=en_GB.UTF-8

    if [ -d "$HOME/.composer/vendor/laravel/installer/" ]; then
	export PATH=$PATH:"$HOME/.composer/vendor/laravel/installer/"
    fi

    alias ec='emacsclient -s /tmp/emacs1000/server -c -a ""'
    alias em='emacsclient -s /tmp/emacs1000/server -nw -a ""'
    alias -g grt='$(git rev-parse --show-toplevel)'

    alias prezget='git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"'

    if [ -z $STY ]; then
	# screen -x
    else
	PS1=screen$PS1
    fi

    if [ -x "$(command -v fortune)" ] && [ -x "$(command -v cowsay)" ] && [ -x "$(command -v lolcat)" ]
    then
	fortune | cowsay -s | lolcat
    else
	echo "you should really get fortune, cowsay and lolcat"
    fi


fi
