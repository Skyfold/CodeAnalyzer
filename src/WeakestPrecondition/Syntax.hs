{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module WeakestPrecondition.Syntax (wkPrecondition) where

import HoareLogic.Structure
import FirstOrderLogic.Syntax 


-- | Find the weakest precondition given a 'ProofSequent'
wkPrecondition :: ProofSequent -> Either Error FOL
wkPrecondition p = do
    resultFOL <- mapOver 
                    (\post -> findWkPre (_loopInvariant p) post (_sequents p))
                    (_postCondition p)
    return $ mapSub (:->) resultFOL (_preCondition p)

mapOver :: (Condition -> Either Error Condition) -> FOL -> Either Error FOL
mapOver f fol = case fol of
    Forall var a -> do
        r <- mapOver f a
        return $ Forall var r
    Exists var a -> do
        r <- mapOver f a
        return $ Forall var r
    Formulae a -> do 
        r <- f a
        return $ Formulae r

mapSub :: (Condition -> Condition -> Condition) -> FOL -> FOL -> FOL
mapSub f preFOL postFOL = case postFOL of
    Forall variables _ -> Forall variables (mapSub f preFOL postFOL)
    Exists variables _ -> Exists variables (mapSub f preFOL postFOL)
    Formulae a -> (f a) <$> preFOL

-- | Find the weakest precondition given:
-- list of invariants, represented as '[Condition]'
-- A postcondition 'Condition'
-- list of 'Sequent' (your code)
--
-- >>> findWkPre [] ((Var "m" :>= Var "a") :& (Var "m" :>= Var "b")) [(IfThenElse (Var "a" :> Var "b") [Assignment "m" (Var "a")] [Assignment "m" (Var "b")])]
-- Right (((Var "a" :> Var "b") :-> ((Var "a" :>= Var "a") :& (Var "a" :>= Var "b"))) :& (Not (Var "a" :> Var "b") :-> ((Var "b" :>= Var "a") :& (Var "b" :>= Var "b"))))
--
findWkPre :: [Condition] -> Condition -> [Sequent] -> Either Error Condition
findWkPre inv con listSeq = case listSeq of
    [] -> return con
    x:xs -> do 
        newCon <- findWkPre inv con xs
        wkPre inv newCon x

-- | Apply Predicate transformer semantics
-- >>> wkPre [] (Var "a" :== Var "b") (Assignment "a" (Num 5))
-- Right (Num 5 :== Var "b")
--
wkPre :: [Condition] -> Condition -> Sequent -> Either Error Condition
wkPre inv con sequent = case sequent of
    Assignment str expr -> 
        Right $ mapVar (\a -> if (a == str) then expr else (Var a)) <$> con
    IfThenElse ifCon thenSeq elseSeq -> do
        l <- findWkPre inv con thenSeq
        r <- findWkPre inv con elseSeq
        return $ (ifCon :-> l) :& ((Not ifCon) :-> r)
    While e s -> do
        case inv of
            [] -> Left NoLoopInvariant
            i:xs -> do
                wlp <- findWkPre xs i s
                return $ (i :& (((e :& i) :-> wlp) :& (((Not e) :& i) :-> con)))
