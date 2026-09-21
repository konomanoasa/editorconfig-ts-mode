# editorconfig-ts-mode

[![CI](https://github.com/konomanoasa/editorconfig-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/editorconfig-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for EditorConfig.

## Requirement

- Emacs 31.1 or later

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/editorconfig-ts-mode")
```

## Automatic Activation

Enabled for `.editorconfig` files.

## Features

- Comment Commands
- Font Lock
- Imenu: sections
- Indentation
- Navigation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock |
| --- | --- |
| 1 | Comments |
| 2 | Property keys, values, and glob text |
| 3 | Numeric range bounds and escapes |
| 4 | Operators, glob wildcards, punctuation, and brackets |

## Grammar

[tree-sitter-editorconfig](https://github.com/konomanoasa/tree-sitter-editorconfig)

## License

[MIT](LICENSE)
