#!/usr/bin/env python3

from pathlib import Path
import os.path
import os


def install_line(cid, file_name, line):
    file_path = Path(file_name).expanduser()
    do_install = False

    file_path.parent.mkdir(mode=0o777, parents=True, exist_ok=True)
    file_path.touch(mode=0o666, exist_ok=True)

    with file_path.open(mode="r") as f:
        if line not in f.read():
            do_install = True

    if do_install:
        print(f"installing {cid} to {file_name}")
        with file_path.open("a+") as f:
            f.write(line)
            f.write("\n")
    else:
        print(f"{cid} already installed")


def install_link(src, dst):
    dst = Path(dst).expanduser()

    if not dst.exists():
        print(f"installing {src} to {dst}")
        os.symlink(os.path.abspath(src), dst)
    else:
        print(f"{src} already installed")


def main():
    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    install_line("bashrc_jc.sh", "~/.bashrc", ". ~/privprjs/dotfiles/bashrc_jc.sh")
    install_line(
        "init.el",
        "~/.emacs.d/init.el",
        '(load-file (expand-file-name "~/privprjs/dotfiles/site-start.el"))',
    )
    install_link("tmux.conf", "~/.tmux.conf")
    install_link("flake8rc", "~/.flake8rc")


if __name__ == "__main__":
    main()
