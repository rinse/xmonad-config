# xmonad-config

This is my config for XMonad.

The goal of this project is the following:

1. It allows you to develop `xmonad.hs` with Stack.
2. It doesn't replace the original XMonad with a custom XMonad out of this project.

The first one is the main purpose of this project. I want to build `xmonad.hs` with Stack so that I can develop `xmonad.hs` more flexibly.

The second one is for stability which means it keep the `xmonad` command as it is. It likely happens that you cannot log in with XMonad because of compilation errors when it replaces the original XMonad with a custom XMonad. Thus, it leaves the `xmonad` command and make it build and restart this project. It runs XMonad with the default settings when it cannot generate a binary of a custom XMonad for any reason.

```bash
$ stack install xmonad
$ git clone https://github.com/rinse/xmonad-config.git $HOME/.xmonad
$ sudo cat << EOF > /usr/share/xsession/xmonad.desktop
[Desktop Entry]
Name=XMonad
Comment=Lightweight tiling window manager
Exec=xmonad
Icon=xmonad.png
Type=Application
EOF
```
