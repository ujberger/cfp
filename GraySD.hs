module GraySD

where
import Control.Concurrent
import Control.Exception


-----------------------------------------------------------
-----------------------------------------------------------
-- Interpreting lists as nondeterminism 
-----------------------------------------------------------
-----------------------------------------------------------

ambL :: [a] -> IO a
ambL xs = do
     m <- newEmptyMVar
     acts <- sequence [forkIO $ evaluate x >>= putMVar m | x <- xs]
     z <- takeMVar m        
     sequence_ (map killThread acts)
     return z


-----------------------------------------------------------
-----------------------------------------------------------
-- Extracting data
-----------------------------------------------------------
-----------------------------------------------------------

data D = Nil | Le D | Ri D | Pair (D, D) | Fun (D -> D) | Amb (D, D) 


-- Extracting data, interpreting Amb as nondeterminism.
ed :: D -> IO D
ed (Le d) = do { d' <- ed d ; return (Le d') }
ed (Ri d) = do { d' <- ed d ; return (Ri d') }
ed (Pair (d, e)) = do { d' <- ed d ; e' <- ed e ; return (Pair (d', e')) }
ed (Fun _) = error "ed (Fun f)"
ed (Amb (a, b)) = do {c <- ambL [a,b] ; ed c}
ed d = return d                     


-----------------------------------------------------------
-----------------------------------------------------------
-- Gray code to Signed Digit Representation conversion
-----------------------------------------------------------
-----------------------------------------------------------
-- Note that 
-- Gray = (Le Nil), (Ri Nil), bot   for -1, 1, bot         ... subset D
-- SD = (Le (Le Nil), (Le (Ri Nil)) (Ri Nil) .for -1, 1, 0  ..  subset D   


-- From Section 5
mapamb :: (D -> D)  -> D -> D  -- (B -> C) -> A(B) -> A(C)
mapamb = \f -> \c -> case c of {Amb(a,b) -> Amb(f $! a, f $! b)}

leftright :: D -> D   -- B + C -> B + C
leftright = \b ->  case b of {Le _ -> Le Nil; Ri _ -> Ri Nil}

conSD :: D -> D    -- 2 x 2 -> A(3)
conSD = \c -> case c of {Pair(a, b) ->
      Amb(Le $! (leftright a), Ri $! (case b of {Le _ -> bot; Ri _ -> Nil}))}


-- From Section 6

gscomp :: D -> D   -- [2] -> A(3)
gscomp (Pair(a, Pair(b, p))) = conSD (Pair(a, b))

onedigit :: D -> D -> D   -- [2] -> 3 -> 3 x [2]
onedigit  (Pair(a, Pair (b, p))) c = case c of {
       Le d -> case d of {
              Le _ -> Pair(Le(Le Nil), Pair(b,p));
              Ri _  -> Pair(Le(Ri Nil), Pair(notD b,p))
        };
        Ri _  -> Pair(Ri Nil, Pair(a, nhD p))}

notD :: D -> D   -- 2 -> 2
notD a = case a of {Le _ -> Ri Nil; Ri _ -> Le Nil}

nhD :: D -> D  -- [2] -> [2]
nhD (Pair (a, p)) = Pair (notD a, p)

s :: D -> D    -- [2] -> A(3 x [2])
s p = mapamb (onedigit p) (gscomp p)

mon :: (D -> D) -> D -> D   -- (B -> C) -> A(3 x B) -> A(3 x C)
mon f p = mapamb (mond f) p  
   where  mond f (Pair(a,t)) = Pair(a, f t)

gtos :: D -> D    -- [2]  -> [3] 
gtos = (mon gtos) . s


-----------------------------------------------------------
-----------------------------------------------------------
-- Gray code generation with delayed digits
-----------------------------------------------------------
-----------------------------------------------------------

-- Gray Code in [Int] is represented as {-1, 1, 0} where 0 means bot.
-- 0 is changed to bot in graydigitToD (since it calls delay).

-- delay: Slow down the computation depending on |n| and
-- return (Ri Nil), 0, (Le Ni) according to sign(n)
delay :: Integer -> D
delay n  | n > 1     = delay (n-1)
         | n == 1    = Ri Nil
         | n == 0    = bot    -- 0 means bot, that is, nontermination
         | n == (-1) = Le Nil
         | n < (-1)  = delay (n+1)
bot = bot

graydigitToD :: Integer -> D  
graydigitToD a | a == (-1) = Le Nil
               | a == 1    = Ri Nil
               | True      = delay (a*100000)

-- list to Pairs
ltop :: [D] -> D
ltop = foldr (\x -> \y -> Pair(x,y)) Nil

grayToD :: [Integer] -> D
grayToD = ltop . (map graydigitToD)



-----------------------------------------------------------
-----------------------------------------------------------
-- Truncating the input and printing the result
-----------------------------------------------------------
-----------------------------------------------------------

-- Truncating d at depth n
takeD :: Int -> D -> D
takeD n d | n > 0 = 
  case d of
    {
      Nil        -> Nil ;
      Le a       -> Le (takeD (n-1) a) ;
      Ri a       -> Ri (takeD (n-1) a) ;
      Pair(a, b) -> Pair (takeD (n-1) a, takeD (n-1) b) ;
      Amb(a,b)   -> Amb(takeD (n-1) a, takeD (n-1) b) ;
      Fun _      -> error "takeD _ (Fun _)" ;
    }
            | otherwise = Nil

-- Showing a partial signed digit

dtosd :: D -> String
dtosd (Le (Ri Nil)) = " 1"
dtosd (Le (Le Nil)) = "-1"
dtosd (Ri Nil)      = " 0"
dtosd _             = " bot"

-- Printing an element of D that represents a finite deterministic 
-- signed digit stream (no Amb).

prints :: D -> IO ()
prints (Pair (d,e)) = putStr (dtosd d) >> prints e
prints Nil   = putStrLn ""
prints d = error "prints: not a partial signed digit stream"


-----------------------------------------------------------
-----------------------------------------------------------
-- Experiments
-----------------------------------------------------------
-----------------------------------------------------------

{-
*GraySD> ed (takeD 50 (gtos (grayToD (1:1:[-1,-1..])))) >>= prints
 1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1 bot
*GraySD> ed (takeD 50 (gtos (grayToD (-1:1:[-1,-1..])))) >>= prints
-1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 bot
*GraySD> ed (takeD 50 (gtos (grayToD (0:1:[-1,-1..])))) >>= prints
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bot
*GraySD> ed (takeD 50 (gtos (grayToD (2:1:[-1,-1..])))) >>= prints
 0 0 1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1 bot
*GraySD> ed (takeD 50 (gtos (grayToD (10:1:[-1,-1..])))) >>= prints
 0 0 0 0 0 0 0 0 0 1-1-1-1-1-1-1-1-1-1-1-1-1-1-1 bot
*GraySD> ed (takeD 50 (gtos (grayToD ([1,1..])))) >>= prints
 1-1 1-1 1-1 1-1 1-1 1-1 1 0-1-1 1-1 1-1 1-1 1-1 bot
*GraySD> ed (takeD 50 (gtos (grayToD (2:[1,1..])))) >>= prints
 0 1 1-1 1-1 1-1 1-1 0 1 1-1 1-1 1-1 1-1 1-1 1-1 bot
-}


-- END