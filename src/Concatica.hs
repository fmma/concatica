module Concatica where

import Data.List ( union, intercalate )
import System.Exit ( die )
import Control.Monad ( (>=>), foldM )
import Prelude hiding ( Word, drop )

import HindleyMilner

data Exp
    = Concat Exp Exp -- e e
    | App Exp Exp    -- e[e]
    | FlippedApp Exp Exp -- e{e}
    | Var String     -- x
    | Pair Exp Exp   -- e,e
    | Unit           -- ()
    | IntVal Int     -- n
    | Abs String Exp -- x => e
{-
   (         =>         )
   (   ,   )    (   ,   )
   ( )   ( )    ( )   ( )
   a b , c d => e f , g h


   a b(c) == a (b(c))
   a (b) == a b
   (a b)(c) == let f = a b in f(c)


   Example:

    (
       filter[x => x.age > 18]
       map[x => x.name]
       sort
    )[xs]

    ..or

    xs{
       filter[x => x.age > 18]
       map[x => x.name]
       sort
    }
-}


data Type
    = TypeVar Int
    | Fun Type Type
    | Tint | Tlist Type
    | Tpair Type Type

instance MonoTypeable Type where
    toMonoType t =
        case t of
            TypeVar i -> Tvar i
            Fun t0 t1 -> Tcon "->" [toMonoType t0, toMonoType t1]
            Tint -> Tcon "int" []
            Tlist t0 -> Tcon "list" [toMonoType t0]
            Tpair t0 t1 -> Tcon "," [toMonoType t0, toMonoType t1]
    fromMonoType t =
        case t of
            Tvar i -> Just $ TypeVar i
            Tcon "->" [t0, t1] -> Fun <$> fromMonoType t0 <*> fromMonoType t1
            Tcon "int" [] -> Just $ Tint
            Tcon "list" [t0] -> Tlist <$> fromMonoType t0
            Tcon "," [t0, t1] -> Tpair <$> fromMonoType t0 <*> fromMonoType t1
            _ -> Nothing
