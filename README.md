[![License GPL 3][badge-license]][copying]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]

# ahk-mode

An Emacs major mode for editing [AutoHotkey][] scripts (`.ahk`).

## Features

- Syntax highlighting
- company-mode and ac-complete compatibility
- indentation and commenting functionality
- Documentation lookup capability

## Installation

Available on all major `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

MELPA Stable is recommended as it has the latest stable version.
MELPA has a development snapshot for users who don't mind breakage but
don't want to run from a git checkout.

You can install `ahk-mode` using the following command:

<kbd>M-x package-install [RET] ahk-mode [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```el
(unless (package-installed-p 'ahk-mode)
  (package-refresh-contents)
  (package-install 'ahk-mode))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

## TODO

* Improve indentation heuristics.
* Allow documentation lookup with local chm files.
* Allow commenting of block using alternate commenting syntax (`/* */`).

## License

Copyright © 2015-2016 Rich Alesi and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: http://melpa.org/packages/ahk-mode-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/ahk-mode-badge.svg
[melpa-package]: http://melpa.org/#/ahk-mode
[melpa-stable-package]: http://stable.melpa.org/#/ahk-mode
[COPYING]: http://www.gnu.org/copyleft/gpl.html
[contributors]: https://github.com/ralesi/ahk-mode/contributors
[melpa]: http://melpa.org
[melpa stable]: http://stable.melpa.org
[AutoHotkey]: https://autohotkey.com/
