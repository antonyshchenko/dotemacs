```
EMACS="/Applications/Aquamacs.app/Contents/MacOS/Aquamacs" cask install
rm ~/Library/Preferences/Aquamacs\ Emacs/Preferences.el
rm ~/Library/Preferences/Aquamacs\ Emacs/customizations.el
ln -s ~/.aquamacs.d/init.el ~/Library/Preferences/Aquamacs\ Emacs/Preferences.el
ln -s ~/.aquamacs.d/customizations.el ~/Library/Preferences/Aquamacs\ Emacs/customizations.el
```
