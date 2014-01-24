# cucumber.el

Emacs mode for editing plain text user stories

## Installation

### Manual

Copy all the files to ~/.emacs.d/elisp/feature-mode, for example,
and add this to your .emacs to load the mode

```lisp
(add-to-list 'load-path "~/.emacs.d/elisp/feature-mode")
```

### Package.el

`feature-mode` is available in both [Marmalade](http://marmalade-repo.org)
and [MELPA](http://melpa.milkbox.net).

You can install it with the following command:

```
M-x package-install feature-mode
```


### Optional configurations

Set default language if .feature doesn't have "# language: fi"
```lisp
(setq feature-default-language "fi")
```

Point to cucumber languages.yml or gherkin i18n.yml to use
exactly the same localization your cucumber uses
```lisp
(setq feature-default-i18n-file "/path/to/gherkin/gem/i18n.yml")
```

Load feature-mode
```lisp
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
```

## Key Bindings

Keybinding          | Description
--------------------|------------------------------------------------------------
<kbd>C-c ,v</kbd>   | Verify all scenarios in the current buffer file.
<kbd>C-c ,s</kbd>   | Verify the scenario under the point in the current buffer.
<kbd>C-c ,f</kbd>   | Verify all features in project. (Available in feature and ruby files)
<kbd>C-c ,r</kbd>   | Repeat the last verification process.
<kbd>C-c ,g</kbd>   | Go to step-definition under point (requires ruby_parser gem >= 2.0.5)


## Supported languages

At the moment, Cucumber.el supports whatever your Cucumber supports.
Just configure it to load i18n.yml from your Gherkin gem sources.

## LICENSE

Copyright (C) 2008 — 2014 Michael Klishin and other contributors

You can redistribute it and/or modify it under the terms of the GNU
General Public License either version 2 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful,
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License if
not, write to the Free Software Foundation, Inc., 51 Franklin Street,
Suite 500, Boston, MA 02110.


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/michaelklishin/cucumber.el/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

