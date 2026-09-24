# table-highlight

`table-highlight-mode` adds low-contrast background colors to Org and Markdown pipe tables.
It can alternate rows, cycle colors across columns, or combine both dimensions.
The package defines separate light and dark face defaults, and composes them with the major mode's existing syntax faces.
Its faces set only backgrounds and take precedence over the major mode's faces, so they show even where the major mode shades tables, while the major mode's foregrounds and weights stay visible.

The implementation is a Font Lock rule, so it is demand-driven through Emacs's JIT fontification.
It examines and colors only requested lines, while still deriving row parity from the beginning of a table.
Edits invalidate only the surrounding tables for deferred refresh, instead of synchronously rescanning the buffer.

## Installation

Add this directory to `load-path`, require the package, and add its automatic entry point to the desired hooks.

```elisp
(add-to-list 'load-path "/path/to/table-highlight")
(require 'table-highlight)
(add-hook 'org-mode-hook #'table-highlight-turn-on)
(add-hook 'markdown-mode-hook #'table-highlight-turn-on)
```

The dotfiles configuration already installs these hooks.

## Configuration

Customize `table-highlight-style` to control automatic highlighting.
Its choices are `none`, `rows`, `columns`, and `rows-and-columns`, with `rows-and-columns` as the default.

Use `M-x table-highlight-set-style` to override the style in one buffer.
`M-x table-highlight-toggle-rows` and `M-x table-highlight-toggle-columns` independently toggle each dimension.
No global keys are bound by the package.

Customize `table-highlight-column-face-count` to change the column cycle length.
The default value is seven, for the rainbow hues in the order red, yellow, blue, violet, orange, green, and indigo.
That order steps through the rainbow two hues at a time, so adjacent columns never have neighboring hues.
The package defines `table-highlight-column-even-0` through `table-highlight-column-even-6` and the corresponding `table-highlight-column-odd-N` faces.
Column-only highlighting uses the even variants, while combined highlighting selects even or odd variants from the zero-indexed row.

Define both even and odd faces for every additional index before increasing `table-highlight-column-face-count`.
The mode reports the first missing computed face instead of silently using an invalid cycle.

## Table syntax

Org pipe tables may contain ordinary rows and horizontal separator rows.
Markdown pipe tables must contain a valid delimiter row immediately after the header.
Escaped Markdown pipes remain within their cell.
Org literal blocks and Markdown fenced code blocks are not highlighted.
Header and separator rows participate in zero-indexed row parity.

## Tests

Run the isolated ERT suite from this directory.

```sh
./run-tests.sh
```
