```
EMACS="/Applications/Aquamacs.app/Contents/MacOS/Aquamacs" cask install
rm ~/.emacs.d/init.el ~/Library/Preferences/Aquamacs\ Emacs/Preferences.el
rm ~/.emacs.d/init.el ~/Library/Preferences/Aquamacs\ Emacs/customizations.el
ln -s ~/.emacs.d/init.el ~/Library/Preferences/Aquamacs\ Emacs/Preferences.el
ln -s ~/.emacs.d/customizations.el ~/Library/Preferences/Aquamacs\ Emacs/customizations.el
```
