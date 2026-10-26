module MplPasses.Parser.AddServices (addServices) where

import MplPasses.Parser.BnfcParse as B

-- A module which defines the service channel types and prepends them to the
-- input program

-- still to do is to add a walk of the program to remove any user defined services
-- from before this was added

import Control.Monad.Writer

-- this just adds the service definitions to the start of the program
addServices :: B.MplProg -> B.MplProg
addServices (MPL_PROG stmts) = MPL_PROG (services ++ stmts)
  where
    services = [terminalDefn, consoleDefn, timerDefn]

{- The injected Terminal, equivalent to the source text

    protocol Terminal => S =
        StringTerminalGet :: Get([Char]| S) => S
        StringTerminalPut :: Put([Char]| S) => S
        StringTerminalClose :: TopBot => S
        IntTerminalGet :: Get(Int| S) => S
        IntTerminalPut :: Put(Int| S) => S
        IntTerminalClose :: TopBot => S
        CharTerminalGet :: Get(Char| S) => S
        CharTerminalPut :: Put(Char| S) => S
        CharTerminalClose :: TopBot => S
-}
terminalDefn :: B.MplStmt
terminalDefn =
    MPL_STMT
        ( MPL_CONCURRENT_TYPE_DEFN
            ( PROTOCOL_DEFN
                [ CONCURRENT_TYPE_CLAUSE
                    (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Terminal")))
                    (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                    [ CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "StringTerminalGet")]
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Get")
                            lbr
                            [ MPL_TYPE
                                ( MPL_LIST_TYPE
                                    lsbr
                                    (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Char")))
                                    rsbr
                                )
                            ]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "StringTerminalPut")]
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Put")
                            lbr
                            [ MPL_TYPE
                                ( MPL_LIST_TYPE
                                    lsbr
                                    (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Char")))
                                    rsbr
                                )
                            ]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "StringTerminalClose")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "TopBot")))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "IntTerminalGet")]
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Get")
                            lbr
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Int"))]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "IntTerminalPut")]
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Put")
                            lbr
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Int"))]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "IntTerminalClose")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "TopBot")))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "CharTerminalGet")]
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Get")
                            lbr
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Char"))]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "CharTerminalPut")]
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Put")
                            lbr
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Char"))]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "CharTerminalClose")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "TopBot")))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                    ]
                ]
            )
        )
  where
    npos = (-1, -1)
    uident s = UIdent (npos, s)
    lbr = LBracket (npos, "(")
    rbr = RBracket (npos, ")")
    lsbr = LSquareBracket (npos, "[")
    rsbr = RSquareBracket (npos, "]")

{- The injected Console, equivalent to the source text

    coprotocol S => Console =
        ConsolePut :: S => Get([Char]| S)
        ConsoleGet :: S => Put([Char]| S)
        ConsoleClose :: S => TopBot
        IntConsolePut :: S => Get(Int| S)
        IntConsoleGet :: S => Put(Int| S)
        IntConsoleClose :: S => TopBot
        CharConsolePut :: S => Get(Char| S)
        CharConsoleGet :: S => Put(Char| S)
        CharConsoleClose :: S => TopBot
        ConsoleStringTerminal :: S => S (*) Neg(Terminal)
-}
consoleDefn :: B.MplStmt
consoleDefn =
    MPL_STMT
        ( MPL_CONCURRENT_TYPE_DEFN
            ( COPROTOCOL_DEFN
                [ CONCURRENT_TYPE_CLAUSE
                    (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                    (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Console")))
                    [ CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "ConsolePut")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Get")
                            lbr
                            [ MPL_TYPE
                                ( MPL_LIST_TYPE
                                    lsbr
                                    (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Char")))
                                    rsbr
                                )
                            ]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "ConsoleGet")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Put")
                            lbr
                            [ MPL_TYPE
                                ( MPL_LIST_TYPE
                                    lsbr
                                    (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Char")))
                                    rsbr
                                )
                            ]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "ConsoleClose")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "TopBot")))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "IntConsolePut")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Get")
                            lbr
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Int"))]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "IntConsoleGet")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Put")
                            lbr
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Int"))]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "IntConsoleClose")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "TopBot")))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "CharConsolePut")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Get")
                            lbr
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Char"))]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "CharConsoleGet")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Put")
                            lbr
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Char"))]
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S"))]
                            rbr))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "CharConsoleClose")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "TopBot")))
                    
                    -- ConsoleStringTerminal :: S => S (*) Neg(Terminal)
                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "ConsoleStringTerminal")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (TENSOR_TYPE 
                            (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                            tensor 
                            (MPL_TYPE (MPL_UIDENT_ARGS_TYPE 
                                (uident "Neg")
                                lbr
                                [(MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Terminal")))]
                                rbr))
                            ))
                    ]
                ]
            )
        )
  where
    npos = (-1, -1)
    uident s = UIdent (npos, s)
    lbr = LBracket (npos, "(")
    rbr = RBracket (npos, ")")
    lsbr = LSquareBracket (npos, "[")
    rsbr = RSquareBracket (npos, "]")
    tensor = Tensor (npos, "(*)")


{- The injected Timer (in microseconds), equivalent to the source text

    coprotocol S => Timer =
        Timer :: S => Get(Int | S (*) Put(()| TopBot))
        TimerClose :: S => TopBot
-}
timerDefn :: B.MplStmt
timerDefn =
    MPL_STMT
        ( MPL_CONCURRENT_TYPE_DEFN
            ( COPROTOCOL_DEFN
                [ CONCURRENT_TYPE_CLAUSE
                    (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                    (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Timer")))
                    [ CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "Timer")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                            (uident "Get")
                            lbr
                            [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "Int"))]
                            [MPL_TYPE (TENSOR_TYPE 
                                (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                                tensor 
                                (MPL_TYPE (MPL_UIDENT_SEQ_CONC_ARGS_TYPE
                                    (uident "Put")
                                    lbr
                                    [MPL_UNIT_TYPE lbr rbr]
                                    [MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "TopBot"))]
                                    rbr)))]
                            rbr))

                    , CONCURRENT_TYPE_PHRASE
                        [TYPE_HANDLE_NAME (uident "TimerClose")]
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "S")))
                        (MPL_TYPE (MPL_UIDENT_NO_ARGS_TYPE (uident "TopBot")))
                    ]
                ]
            )
        )
  where
    npos = (-1, -1)
    uident s = UIdent (npos, s)
    lbr = LBracket (npos, "(")
    rbr = RBracket (npos, ")")
    tensor = Tensor (npos, "(*)")

