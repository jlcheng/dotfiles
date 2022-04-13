ppactivate ()
{
    venvdir=python_env
    if [[ $PWD == *"playground"* ]]; then
        venvdir=$HOME/.venv/pp_playground
    fi

    if [[ -d $venvdir ]]; then
        if [[ -n "$VIRTUAL_ENV" ]]; then
            deactivate
        fi
        source $venvdir/bin/activate
    else
        echo "venv directory not found"
    fi
}

alias grep="grep --color"