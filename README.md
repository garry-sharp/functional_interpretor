# A Basic Demo of something I did a while back

### Purpose/Overview

This was a project for a Haskell Functional Programming class at the University of Glasgow. The purpose was to write an interpretor for a customer programming language (I aptly chose to call this "Garry Script").

The essense of the project was to first parse any code file written in Garry Script and then build up an abstract syntax tree from that (AST) [See wikipedia article on ASTs](https://en.wikipedia.org/wiki/Abstract_syntax_tree).

Once an AST was built this could then be analysed. Since haskell is purely functional language it's important to distinguish between an *expression* << a mathematical expression which yields a result >> and a *statement* << a non-functional statement such as I/O operations (printing) which can't be handled natively in haskell (monads must to be used) >> These need to be treated differently by the interpretor.

### GIT repo and instructions.

Compilation can be done using the ghc [Glasgow Haskell Compiler](https://www.haskell.org/ghc/). All instructions on syntax can be found within the main.hs file.

This repo was recently created for demo purposes only.
