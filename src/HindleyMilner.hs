module HindleyMilner (
    MonoTypeable(..),
    MonoType(..), Typing,
    fresh, unify, generalize, instantiate, runTyping,
    freeVars, subst,
    lift
) where

import Data.List
import Control.Monad.State

class MonoTypeable t where
    toMonoType :: t -> MonoType
    fromMonoType :: MonoType -> Maybe t

data MonoType
    = Tcon String [MonoType]
    | Tvar Int

type Typing m = StateT TypingContext m

fresh :: (Monad m, MonoTypeable t) => Typing m t
fresh = do
    st <- get
    let i = nextFresh st
    put (st {nextFresh = i + 1})
    return $ fromMonoTypeUnsafe $ Tvar i

unify :: (Monad m, MonoTypeable t) => t -> t -> Typing m ()
unify t0 t1 = do
    st <- get
    let cs = eqConstraints st
    put (st {eqConstraints = (toMonoType t0, toMonoType t1) : cs})

instantiate :: (Monad m, MonoTypeable t) => t -> Typing m t
instantiate t =
    let is = freeVars t
    in do
        js <- mapM (const fresh) [1 .. length is]
        return $ subst (zip is js) t

generalize :: MonoTypeable t => t -> t
generalize t =
    let is = sort $ freeVars t
    in fromMonoTypeUnsafe $ subst' (is `zip` map Tvar [0..]) (toMonoType t)

runTyping :: (Monad m, MonoTypeable t) => Typing m t -> m (Either String t)
runTyping t = do
    (t, eqs) <- runStateT t (TypingContext { nextFresh = 0, eqConstraints = [] })
    return $
        fmap (\ g -> fromMonoTypeUnsafe $ subst' g (toMonoType t)) $
        mgu (eqConstraints eqs)

data TypingContext = TypingContext {
    nextFresh :: Int,
    eqConstraints :: [(MonoType, MonoType)]
}

mgu :: [(MonoType, MonoType)] -> Either String [(Int, MonoType)]
mgu eqs =
    case eqs of
        [] -> return []
        ((t0, t1):eqs0) -> mguAux t0 t1 eqs0

mguAux :: MonoType -> MonoType -> [(MonoType, MonoType)] -> Either String [(Int, MonoType)]
mguAux t0 t1 eqs =
    case (t0, t1) of
        (Tcon x ts0, Tcon y ts1) ->
            if x == y && length ts0 == length ts1
            then mgu (zip ts0 ts1 ++ eqs)
            else Left $ "Type mismatch in " ++ show t0 ++ " == " ++ show t1
        (Tcon _ _, _) -> mguAux t1 t0 eqs
        (Tvar i, _)
            | not (occurs i t1)
            , any (\ (t0, t1) -> occurs i t0 || occurs i t1) eqs
            -> let g = subst' [(i, t1)]
                   eqs' = map (\ (t0, t1) -> (g t0, g t1)) eqs
                in mgu (eqs' ++ [(t0, t1)])
        (Tvar i, Tvar j)
            | i == j -> mgu eqs
        (Tvar i, Tcon _ _)
            | occurs i t1 -> Left $ "Occurence check failed in " ++ show t0 ++ " == " ++ show t1
        (Tvar i, _) -> ((i, t1) :) <$> mgu eqs

fromMonoTypeUnsafe :: MonoTypeable t => MonoType -> t
fromMonoTypeUnsafe t =
    case fromMonoType t of
        Just t0 -> t0
        Nothing -> error "Ill-formed type"

occurs :: Int -> MonoType -> Bool
occurs i t =
    case t of
        Tvar j -> i == j
        Tcon _ ts -> any (occurs i) ts

subst' :: [(Int, MonoType)] -> MonoType -> MonoType
subst' g t =
    case t of
        Tvar i -> case lookup i g of
            Nothing -> t
            Just t0 -> t0
        Tcon x ts -> fromMonoTypeUnsafe $ Tcon x (map (subst' g) ts)

subst :: MonoTypeable t => [(Int, t)] -> t -> t
subst g t = fromMonoTypeUnsafe $ subst' (map (fmap toMonoType) g) (toMonoType t)

freeVars :: MonoTypeable t => t -> [Int]
freeVars t =
    case toMonoType t of
        Tvar i -> [i]
        Tcon _ ts -> foldl union [] (map freeVars ts)

instance MonoTypeable MonoType where
    toMonoType = id
    fromMonoType = return

instance Show MonoType where
    show t =
        case t of
            Tvar i -> show i
            Tcon x ts -> x ++ "(" ++ intercalate ", " (map show ts) ++ ")"
