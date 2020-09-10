# MPL


# The Code Base..
`src/*` contains source files as as library (most of the source code resides in this folder).
`app/*` includes the files to generate the executable.
`test/*` includes the files for automated unit testing of the project.

`examples/*` includes the files when the author was playing around with the grammar. Most of it should be deprecated by now.

# Understanding the code base
We only concern ourselves with the contents of the `src/` folder which contain most of the code anyways.

`src/Language` includes automatically generated BNFC parsing files
`src/MplAST` includes data types and utilities related to the program's AST
`src/MplPasses` includes transformations to the AST
`src/MplUtil` includes utility functions for this project

This project extensively uses the following libraries:
- Optics (for lenses)
- mtl / transformers  (monad transformers)

Referring to those libraries' documentation will be critical to understanding how this system works.

For the AST, we used the ``Trees that Grow Idiom" (which is used in GHC). A quick google search should bring up the original paper, and there is a simliar concept implemented with MPL (see `src/MplAST/*`)

# TODO
- For the TODO, root directory of everything.. Lots of stuff is unfinished!

# Runnning automated tests
In the project root directory, type `stack test` and all tests should be run
