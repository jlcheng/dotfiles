ppactivate ()
{
    envdir=python_env
    if [[ -d $envdir ]]; then
        if [[ -n "$VIRTUAL_ENV" ]]; then
            deactivate
        fi
        source $envdir/bin/activate
    else
        echo "venv directory not found"
    fi
}
