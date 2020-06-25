module MPLPar.PtreeToAST_help where 

import MPLPar.AbsMPL
import qualified TypeInfer.MPL_AST as M 
import qualified TypeInfer.Gen_Eqns_CommFuns as E 

import Control.Monad.State.Lazy
import qualified Data.Set as S 
import Data.List


-- =================================================
-- ================================================= 

getChs_proc :: M.Process -> [M.PChannel]
getChs_proc pcomms = totchs \\ remChs
    where
       pairList = map getChfromComm pcomms
       totchs   = (nub.concat.map fst) pairList
       remChs   = (nub.concat.map snd) pairList


getChfromComm :: M.ProcessCommand -> ([M.PChannel],[M.PChannel])
getChfromComm pcomm 
    = case pcomm of 
          M.PRun   (_,_,ichs,ochs,_) -> 
              (ichs ++ ochs,[])
 
          M.PClose (ch,_) ->
              ([ch],[]) 

          M.PHalt  (ch,_) ->
              ([ch],[])

          M.PGet   (_,ch,_) ->
              ([ch],[])

          M.PPut   (_,ch,_) ->
              ([ch],[])

          M.PHPut  (_,ch,_) ->
              ([ch],[])

          M.PHCase (ch,tripList,_) ->
              (ch:lchs,delChs)
            where
              allPcomms = concat $ map (\(p,q,r) -> q) tripList
              pairList  = map getChfromComm allPcomms
              lchs      = concat $ map fst pairList         
              delChs    = concat $ map snd pairList

          M.PSplit (och,(sch1,sch2),_) ->
              ([och],[sch1,sch2])           
                
          M.PFork  (ch,tripList,_) ->
              (ch:allChs,[])
            where 
                allChs = (concat.map (\(a,b,c) -> b)) tripList 


          M.PPlug  (plugChs,((chs1,proc1),(chs2,proc2)),_) ->
              (
                chs1 ++ chs2,
                plugChs
              )
            {-  
            where 
                pairList1 = map getChfromComm proc1 
                pairList2 = map getChfromComm proc2 
                chs1      = concat $ map fst pairList1
                dchs1     = concat $ map snd pairList1
                chs2      = concat $ map fst pairList2
                dchs2     = concat $ map snd pairList2 
            -}

          M.PId    (ch1,ch2,_) -> 
              ([ch1,extractChan ch2],[])

          M.PNeg (ch1,ch2,pn) ->
              ([ch1,ch2],[])

          M.PCase  (_,pattPList,_) ->
              (chs,dchs)
            where
                 allPcomms= concat $ map snd pattPList
                 pairList = map getChfromComm allPcomms
                 chs      = concat $ map fst pairList
                 dchs     = concat $ map snd pairList  



convertToPlug :: [M.Process] -> M.PosnPair -> 
                 Either M.ErrorMsg M.ProcessCommand
convertToPlug procs posn 
    = case length procs == 1 of 
          True -> 
              Left $ 
                "Plug at least needs two processes. " ++
                "Incorrectly being called with one" ++
                E.printPosn posn 
          False -> do 
              let 
                (p1:p2:rest)
                    = procs 
                [chs1,chs2]
                    = map getChs_proc [p1,p2]
                (commChs,remChs)
                    = getCommonChs (chs1,chs2)
                remChs1
                    = chs1 \\ commChs
                remChs2
                    = chs2 \\ commChs
                plugComm 
                    = M.PPlug (commChs,((remChs1,p1),(remChs2,p2)),posn) 

              case rest == [] of 
                  True  -> 
                    return plugComm
                  False -> do 
                    convertToPlug ([plugComm]:rest) posn


{-
convertToPlug_Help :: [M.Process] -> (M.ProcessCommand,[M.PChannel]) ->
                      M.ProcessCommand
convertToPlug_Help [] (finalPlug,_) 
        = finalPlug
convertToPlug_Help (p:ps) (plugComm,accChs)
        = convertToPlug_Help ps (newPlugComm,remChs)
   where
     pchs  = getChs_proc p 
     pPosn = E.getProcPosn (head p) 
     (commChs,remChs)
          = getCommonChs (pchs,accChs)
     newPlugComm
          = M.PPlug (commChs,([plugComm],p),pPosn)
-}

-- output is the common channels from the input and the rest
getCommonChs :: ([M.PChannel],[M.PChannel]) -> 
                ([M.PChannel],[M.PChannel])
getCommonChs (chs1,chs2) 
        = (
           S.toList commChans,
           S.toList remChs
          )
    where
       s1 = S.fromList chs1 
       s2 = S.fromList chs2 
       commChans
          = S.intersection s1 s2 
       unionchs
          = S.union s1 s2 
       remChs
          = S.difference unionchs commChans 



extractChan :: M.Channel -> M.PChannel        
extractChan chan 
    = case chan of 
          M.PosChan p  -> p
          M.NegChan np -> np 

