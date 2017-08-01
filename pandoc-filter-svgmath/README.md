# `pandoc-filter-svgmath`

Pandoc filter for converting TeX math into SVGs.  Based on Ralf Stephan and John MacFarlane's https://gist.github.com/rwst/1437841.

    pandoc -t json input.md |
        pandoc-filter-svgmath [--preamble <preamble-file>] |
        pandoc -f json -o output.html

External dependencies:

  - `pdflatex`
  - `pdf2svg`

Known limitations:

  - Only inline math constructs like `\begin{aligned} ... \end{aligned}` work, but this is usually not a problem since there is almost always an equivalent for every block math construct.
  - `\tag{...}` doesn't work.  Consider `sed 's/\\tag{[^}]*}//g'` for now.
  - The math is sometimes not properly aligned.
