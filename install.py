#!/usr/bin/env python3

import os.path

def install_line(cid, file_name, line):
    file_name = os.path.expanduser(file_name)
    do_install = False
    with open(file_name) as f:
        if line not in f.read():
            do_install = True
    
    if do_install:
        print(f"installing {cid} to {file_name}")
        with open(file_name, "a") as f:
            f.write(line)
            f.write("\n")
    else:
        print(f"{cid} already installed")

def main():
    install_line("bashrc_jc.sh", "~/.bashrc", ". ~/privprjs/dotfiles/bashrc_jc.sh")


if __name__ == '__main__':
    main()
