# `purescript-prettyprinter`

A port of the Haskell [`prettyprinter`][prettyprinter] library by Wadler and friends. They did a phenomenal job of documenting the API so I suggest using their docs as your primary reference.

## Comparison to the Haskell package

-   Rather than having annotation nodes and string nodes, I chose to have only `Renderable` nodes. Something is renderable if it defines a `newline` value, a `space` value, and a function for computing `width` (e.g. `String.length`). I think this is simpler, but you might disagree. It may also be less efficient. If you have thoughts on this let me know.

## Comparison to other Purescript packages

-   [`paf31/purescript-pprint`](https://github.com/paf31/purescript-pprint): Written by the boss man himself, this package defines some clean and simple document primitives but doesn't handle alternative layouts. Use this if your layout logic is fairly simple.
-   [`paulyoung/purescript-prettier-printer`](https://github.com/paulyoung/purescript-prettier-printer): A faithful implementation of [Wadler's original paper][prettier-printer], but lacking some of the powerful extensions added by the [`prettyprinter`][prettyprinter] package. If you don't need the extra functionality then this might be a better choice.

## TODO

-   [ ] Port more functionality (if necessary)
-   [ ] Implement ribbon widths
-   [ ] More tests
-   [ ] Optimisations
-   [ ] Stack safety

[prettyprinter]: http://hackage.haskell.org/package/prettyprinter
[prettier-printer]: https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
