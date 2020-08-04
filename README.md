# Environment setup
## zsh, oh-my-zsh
Currently trying out zsh. There are some adjustments, but I might stick with it.

Installation is pretty straightforward from: https://github.com/ohmyzsh/ohmyzsh

```
cp .zshrc ~/
cp .zprofile ~/
```
I like the idea of keeping a common `.profile` that gets sourced 
by `.zprofile`.

## powerline
```
sudo apt install powerline
```
See powerline config in the powerline directory.

## emacs
See [emacs.readme.md](emacs.readme.md)

## kde autostart
```
mkdir -p ~/.config/autostart-scripts
cp autostart-scripts/* ~/.config/autostart-scripts
```
## kde shortcuts
I'm not sure if I can copy the `khotkeysrc` directly into ~/.config/. It should 
be possible to import shortcuts from `theanswer42.khotkeys`
