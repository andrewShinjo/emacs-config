#!/bin/bash

BASEDIR=$(pwd)

unlink "$HOME/.emacs.d/init.el"
unlink "$HOME/.emacs.d/org-study"
ln -s "$BASEDIR/init.el" "$HOME/.emacs.d/init.el"
ln -s "$BASEDIR/org-study" "$HOME/.emacs.d/org-study"
