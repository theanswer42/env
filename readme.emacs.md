# Emacs setup
This document needs a LOT of work. 

## Resources


* [emacs prelude](https://github.com/bbatsov/prelude)
* [color themes in emacs](http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/)

## Running log of things:

Basic stuff done. Color theme is next. This is leading to more interesting 
things. 

## Zenburn

Looking for a good color theme, ran across zenburn. 
[zenburn](http://slinky.imukuppi.org/zenburnpage/)

Looks like zenburn was written for gvim and ported to a lot of platforms. 
I will have to look into getting it for netbeans at work. 

This is the port for emacs:
[zenburn-emacs](https://github.com/bbatsov/zenburn-emacs)

## MELPA

I see this mentioned multiple times, along with **Marmalade**. Package managers
for emacs?
[MELPA](http://melpa.milkbox.net/#/getting-started)


## GNU ELPA

What is this? [ELPA](http://elpa.gnu.org/)

I'll have to look at what this has later. 

Back to melpa, lets see if this worked. Nope. Ok, here:
[emacs package system](http://ergoemacs.org/emacs/emacs_package_system.html)

I needed to intialize the package system before adding the melpa repo. Added this to init.el: 

`
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
`

Ok. Most of that is good. The customizations will be a problem. Move them out to a different file? 

worked. 
`
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
`

## More emacs customizations
[Aaron Bedra's emacs.d](http://www.aaronbedra.com/emacs.d/)


## org-mode

What is org mode? Can it be useful to me?
[org-mode features](http://orgmode.org/features.html)

I'm going to try this out right now. 

org-mode is sounding more and more like something really awesome. 
org-babel is something to look at too. 

## markdown-mode

Looks like this could be very useful to quickly write documents.
[markdown](http://daringfireball.net/projects/markdown/)

## Emacs starter kit

Aaron mentions using the [emacs starter kit](http://eschulte.github.io/emacs-starter-kit/).
[The github page](https://github.com/eschulte/emacs24-starter-kit/)

There is a LOT of stuff here. I'm not going to install it blindly, instead, I will learn my way
through it. 


