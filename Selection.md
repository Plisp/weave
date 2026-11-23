# Selections and search
Following lessons about rigidity from the MPS project, the mechanism for selection must be adaptable to selecting and replacing arbitrary sequences of nodes while validating that syntactic sorts match up with the destination. This information is already in syntax specifications for all special forms. Macros can have relaxed requirements when we don't have a specification in advance of macroexpansion.

Being interested in semantic diffs, we also need to preserve enough provenance (where a pasted node came from) information through the copy. Search and replacement also should factor through the same mechanism of coherent selections and validated pastes.

## Search
The second most common selection operation besides an ordinary cursor selection is search.
We need two kinds, depending on whether to search structurally or textually.
We need textual search for a particular string representation within all nodes including comments (but not crossing a node boundary). But also:

### Expression search
Searching for a particular binding context, especially multiline is unscalable and usually inaccurate with text:
```lisp
(let (...
      (x (foo)))
```
We want to type an actual expression like `(let ((x (foo))) _)` or `defmethod foo :around (_) _` and have our holes match against syntax according to context. `&rest/body` and macros support fuzzy sequence matching by default, positional parameters are matched one-to-one, with holes matching anything. Typed symbols and literals indicate an exact textual match (subject to change).

This form of search only works over a specified syntactic context. For example, should a typed `(list _)` match against the binding representation of a `let` and the call `(list _)` even though you're probably looking for one (and visually neither is rendered with parens)?

TODO By adding an unquoted search mode, lookups within quoted code `&tree`s is still possible.

## Zipper selection
[Zipper selection](https://arxiv.org/pdf/2411.16571) is a mechanism for structured editing of *contexts*. Read the paper and try it in the editor to see how it elegantly solves various editing problems without unbalancing parens. It also efficiently subsumes paredit operations like raise
```lisp
(foo a)
;; cursor on a, select context (foo _) using shift + up, cut
a
```
and convolute
```lisp
(let ((x 3))
  (baz)
  (with-bar (b)
    z))
;; cursor on z, shift + up, cut, alt-p to select let, paste around let
(with-bar (b)
  (let ((x 3))
    (baz)
    z))
```
which is why these are not implemented. Situations requiring forward slurp/barf can usually be done by cutting first, but for the common case I'll be introducing auto-wrapping by typing into a selected node.

As mentioned in the README, selection implements a sequence-structured extension that Henry Blanchette suggested to me at ICFP 2026. The idea is that if our ast datatype is s-expression like, instead of a hole containing a single AST it can represent a *sequence* of nodes. This allows edits like the following.

```lisp
a
(progn
 b
 c)
// select b and c, then after cutting and pasting progn
(progn
 a
 b)
c
```

## refactor plan from old implementation
1. Decouple binders from node classes, these should be sorts determined by an owning node
   * ~~remove usages of change-class~~
   * ~~macro-calls store a set of binders~~
2. Introduce `map-node node f(child,spec,id)` for mapping over locations.
   * ~~manual cases~~
   * ~~generated bindings~~
3. Implement sequence search using a basic non-backtracking approach
   * ~~spec-match~~
4. Introduce update records for provenance tracking later, add a parameter `keep-identity` that works for single substitutions
   * for slots (replace :id id :node newnode)
   * for list positions (splice :id id :index i insert :insert (list nodes) :delete int)
   * change signature to `update node update-records`
5. Implement replacement planning: from input selection specs <-> update records, and replacement target spec, compute update records for the replacement. This must return both a final new node and a translated focus (last replacement?) for code sharing with search/replace.
