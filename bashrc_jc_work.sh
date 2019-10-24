ppactivate ()
{
    envdir=python_env
    if [[ -d $envdir ]]; then
	deactivate || true
	source $envdir/bin/activate
    else
	echo "venv directory not found"
    fi
}
