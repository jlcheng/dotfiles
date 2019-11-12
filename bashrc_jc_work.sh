ppactivate ()
{
    envdir=python_env
    if [[ -d $envdir ]]; then
        echo 1
        test -z $VIRTUAL_ENV && deactivate || true
        echo 2
        source $envdir/bin/activate
    else
        echo "venv directory not found"
    fi
}
