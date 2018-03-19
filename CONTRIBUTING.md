## Intro

This project follows the contributing recommendations outlined by [saamwerk](https://lorenzwalthert.github.io/saamwerk/).
In particular, issues labelled with `Status: Postponed` are closed even if they
are not resolved.

## File Structure

The source code is organized as follows:

| File           | Description                                                |
| -------------: |:-----------------------------------------------------------|
| addins.R       | ui and helpers for the Addins of styler.                   |
| communicate.R  | function to communicate to the user via the console.       |
| compat-dplyr.R | compatibility functions. Since styler does not depend on dplyr, we define the dplyr functions ourself.| 
| compat-tidyr.R | compatibility functions. Since styler does not depend on tidy, we define the tidyr functions ourself.| 
| expr-is.R      | Functions to check whether an expression matches a predicate (e.g. whether it *is* a function call, a curly brace expression etc.). |
| indent.R       | Computation of whether indention is needed (needs_indention()), if so which indices are indented and how indention is it is triggered. |
| initialize.R   | initializer called with the visitor at each nest. | 
| nest.R         | converting from a text representation into a flat and then into a nested parse table representation. |
| nested-to-tree.R | utilities to create a tree representation from text (after text was converted into a nested parse table). |  
| parse.R        | parse text into parse table, minor token manipulation, verification of parsed objects. | 
| reindent.R     | Deals with token-dependent indention and re-indention, opposed to indent.R where all indention is token independent (i.e. a brace just adds one level of indention, whereas in function declaration headers (if mutli-line), indention depends on token position of "function"). |
| relevel.R | Reorganizing the nested parse table, namely relocates expressions on both sides of "%>%" to the same nest. |
| rules-line-break.R, rules-other.R, rules-replacement.R, rules-spacing.R | transformer rules | 
| serialize.R | converts flattened parse table into text representation. Complement operation to the functions in nest.R | 
| set-assert-args.R | Assertion and setting of arguments. | 
| style-guides.R | How to create style guide objects from transformers. | 
|styler.R | General package information. |
| testing.R | function used for testing. | 
| token-create.R | Utilities for creating tokens, mostly to insert braces around mutli-line if statements. | 
| token-define.R | Defines which tokens belong to which group. |
| transform-code.R, transform-files.R | Transformation of code for APIs that manipulate files (e.g. style_file()). |
| ui.R | User interaces. Top-level functions for styling. | 
| unindent.R | Certain tokens cause unindention, e.g. closing braces. | 
| utils.R | low-level general purpose utilities. |
| vertical.R | S3 class for pretty printing of styled code. | 
| visit.R | Functions that apply functions to each level of nesting, either inside out or outside in. | 
| zzz.R | backport imports. |
