# MPL

# Helpful developer notes:
- Setting up a Hoogle data base with stack
https://blog.ssanj.net/posts/2019-10-19-running-hoogle-locally-for-haskell-dev.html

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

# May 3rd
- Categorical Message Passing Language
    - Parser (operators) / folds, unfolds
    - Precedences: 
        - define your own:
            - Not really anyone knows what you're doing
            - Give things default precedences based on the first character 
                (and let you define stuff over that)
            - (favor this solution)
```
(**) _ _
operator precedene
```
            - if you redefine the operator precedence, 
                then that takes precedence
        - vs 
        - predefined
            - some standard way of using operators
            - Look at first character to determine the predefined 
                precedence
            - e.g. && ,++, @, *, +, ++, >>=, <=, ==, := 
            - problems: a ** b * c ??
    - Haskell look at precedence in Haskell 

    - Type inference
      - Looks like updating floating, 
      - floats vs ints
      - e.g. Float + Int -> float
        i.e., float >= Float (take to the biggest type)
      - Float -> Int (ceiling or floor function) 
      - Int -> Float (coerce to a float)

      - Char:
        - 'a'<='b'
        - char2Int :: Char -> Int

    - Lambda lifting
    - Removal of pattern matching
    - Code generation

    - Services
        - 
