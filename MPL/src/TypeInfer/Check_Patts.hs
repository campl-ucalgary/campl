module TypeInfer.Check_Patts where
{-

checkAllConses :: [PatternTermPhr] -> Either ErrorMsg Bool 
checkAllConses _ = undefined


checkConses_Helper :: [Pattern] -> SymbolTable -> Either ErrorMsg Bool
checkConses_Helper patts symTab = do 
    case checkVPatt_DCare patts of 
        Right _  -> do 
            case length patts == 1 && (not.isConsPatt.head) patts of 
                True -> 
                    return True
                False -> do 
                    -- I know I have hit the case of constructors
                    let 
                       ConsPattern (name,_,cpn) 
                           = head patts
                       lVal= Val_Cons (name,pn)
                    case lookup_ST lval symTab of 
                        Left emsg -> 
                            Left emsg 

                        Right retVal -> do 
                            let 
                              ValRet_Cons ((dname,allcons),_,_,_)
                                = retVal
                             





        Left emsg ->
            Left emsg 




isConsPatt :: Pattern -> Bool
isConsPatt p 
    = case p of 
          ConsPatt _ -> True 
          otherwise  -> False   



-- check if there is only one Var or Don't care patt
-- and that it is only at the end.
checkVPatt_DCare :: [Pattern] -> Either ErrorMsg Bool 
checkVPatt_DCare patts 
    = case length patts == 1 of 
          True  -> 
              return True
          False -> do 
              -- check if there is a don't care/Var except at the last place
              let 
                remPatts   = init patts 
                rlvntPatts = filter isVarDCarePatt remPatts
              case rlvntPatts == [] of
                  True  ->
                      return True
                  False -> do 
                      let
                        msg
                          = "Varibale pattern/match anything patterns " ++ 
                            " can only come at end.Following patterns are not ok.\n"
                        somefun 
                          = \x -> show x ++ printPosn (getPattPosn x) 
                        ePatts 
                          = intercalate "\n" (map somefun rlvntPatts)
                      Left $ msg ++ ePatts



isVarDCarePatt :: Pattern -> Bool 
isVarDCarePatt patt 
    = case patt of 
          DontCarePattern _ -> True 
          VarPattern _      -> True
          otherwise         -> False


-- All patterns can be either cons pattern or var pattern
-- All patterns should be of the same data type.
-- if there is a var pattern as the first pattern followed by first pattern
-- then tell the user that it is an issue.
-- if a constructor name has been repeated , then throw an error.
-- if a constructor name is not present then check if the default case is present.
-- if a default case is present then everything is hunky dory else throw an error saying
-- that not all cases have been handled and there is no default case.



-- 1 > hidden pattern (if the 1st pattern is a  var pattern)

-}
