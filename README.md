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
(out of date)

See [emacs.readme.md](emacs.readme.md)

## kde autostart
```
mkdir -p ~/.config/autostart-scripts
cp autostart-scripts/* ~/.config/autostart-scripts
```
## kde shortcuts
I'm not sure if I can copy the `khotkeysrc` directly into ~/.config/. It should 
be possible to import shortcuts from `theanswer42.khotkeys`

## Tools
### fzf
see https://github.com/junegunn/fzf

Looks to be very useful! Only used it once, but already loving the history
search with ctrl-R.

## linters
### yamllint
```
sudo apt install yamllint
```

### shellcheck
```
sudo apt install shellcheck
```

### python, ruby
`pylint`, install via pip.
`rubocop`, install via gem/bundler.

### golang
(most of the tools come installed, but there's something to be said about 
the right way to install and set up go)
TODO
