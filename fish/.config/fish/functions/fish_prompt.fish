function fish_prompt
    set -l color_cwd
    set -l suffix
    switch "$USER"
        case root toor
            if set -q fish_color_cwd_root
                set color_cwd $fish_color_cwd_root
            else
                set color_cwd $fish_color_cwd
            end
            set suffix '#'
        case '*'
            set color_cwd $fish_color_cwd
            set suffix '$'
    end

    printf "\033]7;file://$PWD\033\\"
    echo -n -s (set_color $color_cwd) (prompt_pwd) (set_color normal) " $suffix "
end
