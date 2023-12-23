module Test where

import Concatica

e1, e2 :: Exp
e1 = Stack []
e2 = Stack [Swap, Dup]

testExp :: Exp -> IO ()
testExp e = do
    print e
    print (typeExpForReal e)
    print =<< evalExp e (Stack [Vint 1000, Vint 1001, Vint 1002])

test :: IO ()
test = do
    mapM_ testExp [e1, e2]
    putStrLn "ALL GOOD :-)"
