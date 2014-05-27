MiniParser
==========

The module provides a monadic, backtracking parser with an emphasis on
*simplicity*: the aim is to provide a flexible, barebone parser that has no
external dependencies.

Only the most primitive parsers and combinators are provided.  There is no
optimization at all.

The interface is intentionally minimalistic: most of the features are meant to
be accessed via the type classes.
