This tool is very much a work in progress.  Beware of incomplete features,
bugs, clunky UX etc.

There is no support for mobile platforms, sorry.  You need a relatively modern
browser (no IE) and a working mouse with a scroll-wheel button (middle
button).

Features not yet available:
- Editing the tableau information (transporting j and lines over deltas)

Layout
======

The main area shows the diagram.

The right panel shows the tableau, which tracks phases, weights, and other
tedious details.

The bottom panel, if clicked, will display the equation.  This equation is not
updated automatically (because MathJax is really slow).

Usage
=====

There are two kinds of things you can do in the editor:

  - Editing:
      - These often violate equivalence.
      - You can disable editing by toggling the frozen mode (<kbd>f</kbd> key).

  - Applying rules:
      - These always preserve equivalence (if not then it's a bug...)

You can "undo" by clicking the back button on your browser.  Note that every
change to the diagram automatically updates the URL, so you can "save"
diagrams by bookmarking the URL.  You can also send it to other people.  (Be
aware that the URL can get very very long for complicated diagrams!)

Editing mode
------------

- <kbd>w</kbd>: create a Wigner 3-jm symbol at the location of your cursor
- <kbd>c</kbd>: create a Clebsch-Gordan coefficient at the location of your cursor
- <kbd>a</kbd>: attach/join two nearby terminals, forming a single line
- <kbd>x</kbd>: delete a node or a free line (can be indirectly used to detach lines)
- <kbd>f</kbd>: switch between editing mode and frozen mode
- <kbd>r</kbd>: reset and clear the entire diagram
     (note if you encounter a bug in the app and <kbd>r</kbd> doesn't work,
      you can still click on this button as a link to reset!)
- <kbd>Right-click</kbd> on a line will add/flip/remove the arrow.
- <kbd>Middle-click</kbd> on a line will change its j to a fresh value.

The tableau is editable too!  You can click on things, etc.  Some of these
modifications are still available (albeit more restricted) while frozen.

Frozen mode
-----------

Rules preserve the meaning of the diagram.

First of all, you're allowed to move things around freely (with the exception
of the terminals, which remain fixed).  You can drag nodes, lines, arrows, and
labels using the left mouse button.

Here are the explicit rules that you can use.  Don't worry about the
phase factors or weights: the editor will automatically adjust them :3

Minor rules:

- **Orientation reversal:** <kbd>Right-click</kbd> on a node to flip the
  orientation.  This changes the phase by (-1)<sup>j<sub>1</sub> +
  j<sub>2</sub> + j<sub>3</sub></sup>

- **Double j phase**: <kbd>Shift</kbd> + <kbd>Right-click</kbd> on a node to
  change the phase by (-1)<sup>2 j<sub>1</sub> + 2 j<sub>2</sub> + 2
  j<sub>3</sub></sup>

- **Triple arrow rule**: <kbd>Middle-click</kbd> on a node to change the
  surrounding arrows

- **Arrow reversal**: <kbd>Right-click</kbd> on a line to flip the arrow.
  This changes the phase by (-1)<sup>2 j</sup>.  On j = 0 lines, this can also
  conjure arrows out of thin air.

- **Swap j variable**: <kbd>Middle-click</kbd> on a line cycles through the
  variables that are known to be equal due to Kronecker deltas.

Major rules (six rules and they come in pairs):

- **(I) Pinching rule:** <kbd>Middle-drag</kbd> a line onto another line will
  introduce a resolution of the identity as two 3-jm nodes, with a summed j in
  between.

- **(I') Prying rule:** <kbd>Right-drag</kbd> a summed line onto itself to remove
  a resolution of the identity, destroying two 3-jm nodes.

- **(II) Pruning rule:** <kbd>Right-drag</kbd> a loop line onto its neighboring
  node will the set the opposite line to zero, and try to erase the loop
  entirely along with the two adjacent 3-jm nodes.

- **(II') Growing rule:** <kbd>Middle-drag</kbd> a line onto empty space will create
  two 3-jm nodes along with a loop (dual of pruning rule).

- **(III) Cutting rule:** <kbd>Middle-drag</kbd> a line onto itself will split the
  diagram into two separate pieces, creating two 3-jm nodes in the process.
  This only works if at least one of the subdiagrams is orientable.

- **(III') Gluing rule:** <kbd>Right-drag</kbd> a line onto another line will join
  the two lines together, creating two 3-jm nodes in the process.

Interpretation
==============

Here, arrows on the diagram are interpreted literally as (-1)<sup>j - m</sup>
phase factors, accompanied by a reversal of the sign of m.  This makes their
behavior somewhat different from the conventional Jucys diagrams where arrows
signify variance.  The end result is that the rules are more mechanical and
have fewer special cases to worry about (no distinction between
internal/external lines).  The cost is that non-orientable subdiagrams can
appear if the diagrams are not manipulated carefully.  This affects the
cutting rule, which is invalid for non-orientable diagrams, but the program
should prevent you from trying to do that on non-orientable diagrams.

Tableau
-------

The tableau consists of two parts:

  - The upper table:
      - Leftmost column (usually invisible): whether the j is being summed over.
      - 2nd column: label of the j
      - 3rd column: phase (each dot stands for a single (-1)<sup>j</sup> phase)
      - Rightmost column: sqrt(2 j + 1) weights (only the exponent is shown)
  - The lower list shows the Kronecker deltas.
