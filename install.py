#!/usr/bin/env python3

import os.path
import os


def install_line(cid, file_name, line):
    file_name = os.path.expanduser(file_name)
    do_install = False

    if not os.path.exists(file_name):
        with open(file_name, "a+"):
            pass

    with open(file_name, "r") as f:
        if line not in f.read():
            do_install = True

    if do_install:
        print(f"installing {cid} to {file_name}")
        with open(file_name, "a+") as f:
            f.write(line)
            f.write("\n")
    else:
        print(f"{cid} already installed")


def install_link(src, dst):
    dst = os.path.expanduser(dst)
    do_install = not os.path.exists(dst)

    if do_install:
        print(f"installing {src} to {dst}")
        os.symlink(os.path.abspath(src), dst)
    else:
        print(f"{src} already installed")


def main():
    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    install_line("bashrc_jc.sh", "~/.bashrc", ". ~/privprjs/dotfiles/bashrc_jc.sh")
    install_line("init.el", "~/.emacs.d/init.el", '(load-file (expand-file-name "~/privprjs/dotfiles/site-start.el"))')
    install_link("tmux.conf", "~/.tmux.conf")
    install_link("flake8rc", "~/.flake8rc")


if __name__ == '__main__':
    main()
