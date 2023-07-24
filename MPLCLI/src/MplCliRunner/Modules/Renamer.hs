

module MplCliRunner.Modules.Renamer (astRefactor,astDefRefactor,refToModule) where

-- For the ast
import MplPasses.Parser.BnfcParse as B

-- a module which takes a raw AST and renames object usage and definitions:
-- astRefactor renames function/process usages
-- astDefRefactor renames function/process definitions

-- A function which takes a qualified object (Module,ObjectName) and returns
-- either a qualified or unqualified object.
type QFunc = ((String,String) -> Either String (String,String))
-- A function which takes an unqualified object (Module,ObjectName) and returns
-- either a qualified or unqualified object.
type UFunc = (String -> Either String (String,String))
-- A tuple of the four functions. See the 'astRefactor' definition for which is which.
type UQFuncs = (UFunc,QFunc,UFunc,QFunc)

-- a function which takes four input functions and returns a function.
-- See the definitions of QFunc and UFunc for how the functions work.
-- astRefactor handles the AST traversal, unwrapping the strings, re-wrapping the strings,
-- line numbers, etc. All you have to do is give it functions for each case.
-- If you want the function to do nothing, call 'astRefactor Left Right Left Right ast'
astRefactor ::
    UFunc -> -- convert unqualified function names
    QFunc -> -- convert qualified   function names
    UFunc -> -- convert unqualified process  names
    QFunc -> -- convert qualified   process  names
    B.MplProg -> B.MplProg -- The function which renames things
astRefactor uf qf up qp (MPL_PROG ls) =
    MPL_PROG $ map (refStmt (uf,qf,up,qp)) ls


-- henceforth, all functions just take the tuple of functions, the node, and return the node.
-- they use the functions to refactor whatever needs to be refactored.
-- (this refactors object uses, not object definitions)


refStmt :: UQFuncs -> MplStmt -> MplStmt
refStmt fs (MPL_DEFN_STMS_WHERE defns whers) =
    MPL_DEFN_STMS_WHERE (map (refDefn fs) defns) (map (refWher fs) whers)
refStmt fs (MPL_DEFN_STMS defns) =
    MPL_DEFN_STMS (map (refDefn fs) defns)
refStmt fs (MPL_STMT defn) =
    MPL_STMT (refDefn fs defn)


refWher :: UQFuncs -> MplWhere -> MplWhere
refWher fs (MPL_WHERE stmt) =
    MPL_WHERE (refStmt fs stmt)


refDefn :: UQFuncs -> MplDefn -> MplDefn
refDefn fs (MPL_FUNCTION_DEFN defn) =
    MPL_FUNCTION_DEFN (refFuncDefn fs defn)
refDefn fs (MPL_PROCESS_DEFN defn) =
    MPL_PROCESS_DEFN (refProcDefn fs defn)
refDefn _ a = a -- type definitions, test definition, imports


refFuncDefn :: UQFuncs -> FunctionDefn -> FunctionDefn
refFuncDefn fs (TYPED_FUNCTION_DEFN name ts t peps) =
    TYPED_FUNCTION_DEFN name ts t (map (refPEP fs) peps)
refFuncDefn fs (FUNCTION_DEFN name peps) =
    FUNCTION_DEFN name (map (refPEP fs) peps)
refFuncDefn fs (TYPED_FUNCTION_DEFN_UINFIX lb name rb t1 t2 t3 peps) =
    TYPED_FUNCTION_DEFN_UINFIX lb name rb t1 t2 t3 (map (refPEP fs) peps)
refFuncDefn fs (FUNCTION_DEFN_UINFIX lb name rb peps) =
    FUNCTION_DEFN_UINFIX lb name rb (map (refPEP fs) peps)
refFuncDefn _ a = a -- the internal typed function definition


refProcDefn :: UQFuncs -> ProcessDefn -> ProcessDefn
refProcDefn fs (TYPED_PROCESS_DEFN name t1s t2s t3s pps) =
    TYPED_PROCESS_DEFN name t1s t2s t3s (map (refPP fs) pps)
refProcDefn fs (PROCESS_DEFN name pps) =
    PROCESS_DEFN name (map (refPP fs) pps)
refProcDefn _ a = a -- the internal typed process definition

-- PattExprPhrase
refPEP :: UQFuncs -> PattExprPhrase -> PattExprPhrase
refPEP fs (PATTERN_TO_EXPR ps expr) =
    PATTERN_TO_EXPR ps (refExpr fs expr)


-- ProcessPhrase
refPP :: UQFuncs -> ProcessPhrase -> ProcessPhrase
refPP fs (PROCESS_PHRASE ps name1s name2s pcb) =
    PROCESS_PHRASE ps name1s name2s (refPCB fs pcb)


-- ProcessCommandsBlock
refPCB :: UQFuncs -> ProcessCommandsBlock -> ProcessCommandsBlock
refPCB fs (PROCESS_COMMANDS_DO_BLOCK pcs) =
    PROCESS_COMMANDS_DO_BLOCK (map (refPC fs) pcs)
refPCB fs (PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK pc) =
    PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK (refPC fs pc)


-- ProcessCommand
refPC :: UQFuncs -> ProcessCommand -> ProcessCommand
refPC fs (PROCESS_PUT put expr name) =
    PROCESS_PUT put (refExpr fs expr) name
refPC fs (PROCESS_HCASE hCase name hcps) =
    PROCESS_HCASE hCase name (map (refHCP fs) hcps)
refPC fs (PROCESS_ON name pops) =
    PROCESS_ON name (map (refPOP fs) pops)
refPC fs (PROCESS_FORK fork name pfps) =
    PROCESS_FORK fork name (map (refPFP fs) pfps)
refPC fs (PROCESS_RACE prps) =
    PROCESS_RACE (map (refPRP fs) prps)
refPC fs (PROCESS_PLUG ppps) =
    PROCESS_PLUG (map (refPPP fs) ppps)
refPC fs (PROCESS_CASE theCaseCommand expr pcps) =
    PROCESS_CASE theCaseCommand (refExpr fs expr) (map (refPCP fs) pcps)
refPC fs (PROCESS_IF expr pcb1 pcb2) =
    PROCESS_IF (refExpr fs expr) (refPCB fs pcb1) (refPCB fs pcb2)
refPC fs (PROCESS_SWITCH psps) =
    PROCESS_SWITCH (map (refPSP fs) psps)
-- the two 'refactoring' cases. Unwrap names, apply correct function, re-wrap however needed.
refPC fs@(_,_,f,_) (PROCESS_RUN (PIdent (xy,name)) lb exprs c1s c2s rb) = 
    case (map (refExpr fs) exprs) of
        es -> case (f name) of
            (Left n) -> PROCESS_RUN (PIdent (xy,n)) lb es c1s c2s rb
            (Right (n1,n2)) ->
                PROCESS_QRUN (UIdent (xy,n1)) (PIdent (xy,n2)) lb es c1s c2s rb
refPC fs@(_,_,_,f) (PROCESS_QRUN (UIdent (xy1,modName)) (PIdent (xy2,objName)) lb exprs c1s c2s rb) =
    case (map (refExpr fs) exprs) of
        es -> case (f (modName,objName)) of
            (Left n) -> PROCESS_RUN (PIdent (xy1,n)) lb es c1s c2s rb
            (Right (n1,n2)) ->
                PROCESS_QRUN (UIdent (xy1,n1)) (PIdent (xy2,n2)) lb es c1s c2s rb
-- can ignore NEG,ID,CLOSE,SPLIT,HPUT,GET,HALT
refPC _ a = a


-- HCasePhrase
refHCP :: UQFuncs -> HCasePhrase -> HCasePhrase
refHCP fs (HCASE_PHRASE name pcb) =
    HCASE_PHRASE name (refPCB fs pcb)


-- process 'on' phrase
refPOP :: UQFuncs -> OnPhrase -> OnPhrase
refPOP fs (ON_PUT put expr) =
    ON_PUT put (refExpr fs expr)
refPOP fs (ON_HCASE hCase hcps) =
    ON_HCASE hCase (map (refHCP fs) hcps)
refPOP fs (ON_FORK fork pfps) =
    ON_FORK fork (map (refPFP fs) pfps)
-- CLOSE, HALT, GET, HPUT, SPLIT can be left alone
refPOP _ a = a


-- process fork phrase
refPFP :: UQFuncs -> ForkPhrase -> ForkPhrase
refPFP fs (FORK_PHRASE name pcb) =
    FORK_PHRASE name (refPCB fs pcb)
refPFP fs (FORK_WITH_PHRASE name names pcb) =
    FORK_WITH_PHRASE name names (refPCB fs pcb)


-- process race phrase
refPRP :: UQFuncs -> RacePhrase -> RacePhrase
refPRP fs (RACE_PHRASE name pcb) =
    RACE_PHRASE name (refPCB fs pcb)


-- process plug phrase
refPPP :: UQFuncs -> PlugPhrase -> PlugPhrase
refPPP fs (PLUG_PHRASE pcb) =
    PLUG_PHRASE (refPCB fs pcb)
refPPP fs (PLUG_PHRASE_AS name1s name2s pcb) =
    PLUG_PHRASE_AS name1s name2s (refPCB fs pcb)


refPCP :: UQFuncs -> ProcessCasePhrase -> ProcessCasePhrase
refPCP fs (PROCESS_CASE_PHRASE p pcb) =
    PROCESS_CASE_PHRASE p (refPCB fs pcb)


refPSP :: UQFuncs -> ProcessSwitchPhrase -> ProcessSwitchPhrase
refPSP fs (PROCESS_SWITCH_PHRASE expr pcb) =
    PROCESS_SWITCH_PHRASE (refExpr fs expr) (refPCB fs pcb)


------------------------------------------------------
------------------------------------------------------
-- Time for some expressions!
------------------------------------------------------
------------------------------------------------------


refExpr :: UQFuncs -> Expr -> Expr
refExpr fs (EXPR expr) =
    EXPR (refExpr fs expr)
refExpr fs (TYPED_EXPR expr t) =
    TYPED_EXPR (refExpr fs expr) t
refExpr fs (IF_EXPR expr1 expr2 expr3) =
    IF_EXPR (refExpr fs expr1) (refExpr fs expr2) (refExpr fs expr3)
refExpr fs (LET_EXPR leps expr) =
    LET_EXPR (map (refLEP fs) leps) (refExpr fs expr)
refExpr fs (INFIXR0_EXPR expr1 colon expr2) =
    INFIXR0_EXPR (refExpr fs expr1) colon (refExpr fs expr2)
refExpr fs (INFIXL1_EXPR expr1 op expr2) =
    INFIXL1_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXU1_EXPR expr1 op expr2) =
    INFIXU1_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXL2_EXPR expr1 op expr2) =
    INFIXL2_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXU2_EXPR expr1 op expr2) =
    INFIXU2_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXL3_EXPR expr1 op expr2) =
    INFIXL3_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXU3_EXPR expr1 op expr2) =
    INFIXU3_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXL4_EXPR expr1 op expr2) =
    INFIXL4_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXL5_EXPR expr1 op expr2) =
    INFIXL5_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXU5_EXPR expr1 op expr2) =
    INFIXU5_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXL6_EXPR expr1 op expr2) =
    INFIXL6_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXU6_EXPR expr1 op expr2) =
    INFIXU6_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXR7_EXPR expr1 op expr2) =
    INFIXR7_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXU7_EXPR expr1 op expr2) =
    INFIXU7_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXL8_EXPR expr1 op expr2) =
    INFIXL8_EXPR (refExpr fs expr1) op (refExpr fs expr2)
refExpr fs (INFIXU_SECT lb op rb lb2 expr1 expr2 rb2) =
    INFIXU_SECT lb op rb lb2 (refExpr fs expr1) (refExpr fs expr2) rb2
refExpr fs (INFIXL1_SECT lb op rb lb2 expr1 expr2 rb2) =
    INFIXL1_SECT lb op rb lb2 (refExpr fs expr1) (refExpr fs expr2) rb2
refExpr fs (INFIXL2_SECT lb op rb lb2 expr1 expr2 rb2) =
    INFIXL2_SECT lb op rb lb2 (refExpr fs expr1) (refExpr fs expr2) rb2
refExpr fs (INFIXL3_SECT lb op rb lb2 expr1 expr2 rb2) =
    INFIXL3_SECT lb op rb lb2 (refExpr fs expr1) (refExpr fs expr2) rb2
refExpr fs (INFIXL4_SECT lb op rb lb2 expr1 expr2 rb2) =
    INFIXL4_SECT lb op rb lb2 (refExpr fs expr1) (refExpr fs expr2) rb2
refExpr fs (INFIXL5_SECT lb op rb lb2 expr1 expr2 rb2) =
    INFIXL5_SECT lb op rb lb2 (refExpr fs expr1) (refExpr fs expr2) rb2
refExpr fs (INFIXL6_SECT lb op rb lb2 expr1 expr2 rb2) =
    INFIXL6_SECT lb op rb lb2 (refExpr fs expr1) (refExpr fs expr2) rb2
refExpr fs (INFIXR7_SECT lb op rb lb2 expr1 expr2 rb2) =
    INFIXR7_SECT lb op rb lb2 (refExpr fs expr1) (refExpr fs expr2) rb2
refExpr fs (INFIXL8_SECT lb op rb lb2 expr1 expr2 rb2) =
    INFIXL8_SECT lb op rb lb2 (refExpr fs expr1) (refExpr fs expr2) rb2
refExpr fs (INFIXPR_SECT par lb expr1 expr2 rb) =
    INFIXPR_SECT par lb (refExpr fs expr1) (refExpr fs expr2) rb
refExpr fs (INFIXTN_SECT tensor lb expr1 expr2 rb) =
    INFIXTN_SECT tensor lb (refExpr fs expr1) (refExpr fs expr2) rb
refExpr fs (LIST_EXPR lb exprs rb) =
    LIST_EXPR lb (map (refExpr fs) exprs) rb
refExpr fs (VAR_EXPR a) =
    VAR_EXPR a
refExpr fs (INT_EXPR a) =
    INT_EXPR a
refExpr fs (STRING_EXPR a) =
    STRING_EXPR a
refExpr fs (CHAR_EXPR a) =
    CHAR_EXPR a
refExpr fs (DOUBLE_EXPR a) =
    DOUBLE_EXPR a
refExpr fs (UNIT_EXPR lb rb) =
    UNIT_EXPR lb rb
refExpr fs (FOLD_EXPR expr feps) =
    FOLD_EXPR (refExpr fs expr) (map (refFEP fs) feps)
refExpr fs (UNFOLD_EXPR expr ueps) =
    UNFOLD_EXPR (refExpr fs expr) (map (refUEP fs) ueps)
refExpr fs (CASE_EXPR theCaseCommand expr peps) =
    CASE_EXPR theCaseCommand (refExpr fs expr) (map (refPEP fs) peps)
refExpr fs (SWITCH_EXP seps) =
    SWITCH_EXP (map (refSEP fs) seps)
refExpr fs (DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR name lb exprs rb) =
    DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR name lb (map (refExpr fs) exprs) rb
refExpr fs (DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR name) =
    DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR name
refExpr fs (TUPLE_EXPR lb expr tels rb) =
    TUPLE_EXPR lb (refExpr fs expr) (map (refTEL fs) tels) rb
refExpr fs (RECORD_EXPR lb reps rb) =
    RECORD_EXPR lb (map (refREP fs) reps) rb
refExpr fs (BRACKETED_EXPR lb expr rb) =
    BRACKETED_EXPR lb (refExpr fs expr) rb
-- the refactor cases. unwrap the names, apply the function,
-- re-wrap based on the function output.
refExpr fs@(f,_,_,_) (FUN_EXPR (PIdent (xy,name)) lb exprs rb) =
    case (map (refExpr fs) exprs) of
        newEs -> case (f name) of
            (Left n)        -> FUN_EXPR                   (PIdent (xy,n))  lb newEs rb
            (Right (n1,n2)) -> FUNQ_EXPR (UIdent (xy,n1)) (PIdent (xy,n2)) lb newEs rb
refExpr fs@(_,f,_,_) (FUNQ_EXPR (UIdent (xy,modName)) (PIdent (xy2,objName)) lb exprs rb) =
    case (map (refExpr fs) exprs) of
        newEs -> case (f (modName,objName)) of
            (Left n)        -> FUN_EXPR                   (PIdent (xy,n))   lb newEs rb
            (Right (n1,n2)) -> FUNQ_EXPR (UIdent (xy,n1)) (PIdent (xy2,n2)) lb newEs rb


-- LetExprPhrase
refLEP :: UQFuncs -> LetExprPhrase -> LetExprPhrase
refLEP fs (LET_EXPR_PHRASE stmt) = LET_EXPR_PHRASE (refStmt fs stmt)


-- FoldExprPhrase
refFEP :: UQFuncs -> FoldExprPhrase -> FoldExprPhrase
refFEP fs (FOLD_EXPR_PHRASE name colon ps expr) =
    FOLD_EXPR_PHRASE name colon ps (refExpr fs expr)


-- UnfoldExprPhrase
refUEP :: UQFuncs -> UnfoldExprPhrase -> UnfoldExprPhrase
refUEP fs (UNFOLD_EXPR_PHRASE p feps) =
    UNFOLD_EXPR_PHRASE p (map (refFEP fs) feps)


-- SwitchExprPhrase
refSEP :: UQFuncs -> SwitchExprPhrase -> SwitchExprPhrase
refSEP fs (SWITCH_EXPR_PHRASE expr1 expr2) =
    SWITCH_EXPR_PHRASE (refExpr fs expr1) (refExpr fs expr2)


-- TupleExprList
refTEL :: UQFuncs -> TupleExprList -> TupleExprList
refTEL fs (TUPLE_EXPR_LIST expr) =
    TUPLE_EXPR_LIST (refExpr fs expr)


-- RecordExprPhrase
refREP :: UQFuncs -> RecordExprPhrase -> RecordExprPhrase
refREP fs (RECORD_EXPR_PHRASE name expr) =
    RECORD_EXPR_PHRASE name (refExpr fs expr)
refREP fs (RECORD_EXPR_HIGHER_ORDER_PHRASE name pep) =
    RECORD_EXPR_HIGHER_ORDER_PHRASE name (refPEP fs pep)



------------------------------------------------------
------------------------------------------------------
--        Part 2: refactoring definitions.          --
------------------------------------------------------
------------------------------------------------------

-- The type of a function pair for renaming things.
type UFuncs = ((String->String),(String->String))

-- changes the names of function and process definitions.
astDefRefactor ::
    (String -> String) -> -- convert unqualified function names
    (String -> String) -> -- convert unqualified process  names
    B.MplProg -> B.MplProg -- The function which renames things
astDefRefactor f p (MPL_PROG ls) =
    MPL_PROG $ map (refDefStmt (f,p)) ls


refDefStmt :: UFuncs -> MplStmt -> MplStmt
refDefStmt fs (MPL_DEFN_STMS_WHERE defns whers) =
    MPL_DEFN_STMS_WHERE (map (refDefDefn fs) defns) (map (refDefWher fs) whers)
refDefStmt fs (MPL_DEFN_STMS defns) =
    MPL_DEFN_STMS (map (refDefDefn fs) defns)
refDefStmt fs (MPL_STMT defn) =
    MPL_STMT (refDefDefn fs defn)


refDefWher :: UFuncs -> MplWhere -> MplWhere
refDefWher fs (MPL_WHERE stmt) =
    MPL_WHERE (refDefStmt fs stmt)


refDefDefn :: UFuncs -> MplDefn -> MplDefn
refDefDefn fs (MPL_FUNCTION_DEFN defn) =
    MPL_FUNCTION_DEFN (refDefFuncDefn fs defn)
refDefDefn fs (MPL_PROCESS_DEFN defn) =
    MPL_PROCESS_DEFN (refDefProcDefn fs defn)
refDefDefn _ a = a -- type definitions, test definition, imports


refDefFuncDefn :: UFuncs -> FunctionDefn -> FunctionDefn
refDefFuncDefn (f,_) (TYPED_FUNCTION_DEFN name ts t peps) =
    TYPED_FUNCTION_DEFN (unReWrap f name) ts t peps
refDefFuncDefn (f,_) (FUNCTION_DEFN name peps) =
    FUNCTION_DEFN (unReWrap f name) peps
refDefFuncDefn _ a = a -- ignore infix operators


refDefProcDefn :: UFuncs -> ProcessDefn -> ProcessDefn
refDefProcDefn (_,p) (TYPED_PROCESS_DEFN name t1s t2s t3s pps) =
    TYPED_PROCESS_DEFN (unReWrap p name) t1s t2s t3s pps
refDefProcDefn (_,p) (PROCESS_DEFN name pps) =
    PROCESS_DEFN (unReWrap p name) pps
refDefProcDefn _ a = a

unReWrap :: (String -> String) -> PIdent -> PIdent
unReWrap f (PIdent (xy,n)) = PIdent (xy,f n)

------------------------------------------------------
------------------------------------------------------
-- Part 3: Refactoring an AST to a module
------------------------------------------------------
------------------------------------------------------

-- Takes:
--   the module name
--   The AST
-- refactors the AST to make it a module. The main module does not need to be refactored.
refToModule :: String -> B.MplProg -> B.MplProg
refToModule modName ast = -- For non-main modules
    astDefRefactor
        (\objName -> modName ++ ('.':objName))
        (\objName -> modName ++ ('.':objName))
    $ astRefactor
        (\objName -> Right (modName,objName))
        Right
        (\objName -> Right (modName,objName))
        Right
    $ ast

