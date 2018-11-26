{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a = State {runState :: s -> (a, s)}

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec :: State s a -> s -> s
exec s = snd . runState s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval :: State s a -> s -> a
eval s = fst . runState s

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get :: State s s
get = State (\s -> (s, s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put :: s -> State s ()
put s = State (const ((), s))

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) :: (a -> b) -> State s a -> State s b
  f <$> s = State (\x -> (f (eval s x), exec s x))

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (\s -> (a, s))
  (<*>) :: State s (a -> b) -> State s a -> State s b
  fs <*> s = State (\initState -> ((eval fs initState) (eval s initState), newState initState))
    where
      newState initState = exec s $ exec fs initState

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) :: (a -> State s b) -> State s a -> State s b
  g =<< s = State f
    where
      f initState =
        let (a, s') = runState s initState
        in runState (g a) s'

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
findM _ Nil = return Empty
findM p (x :. xs) = p x >>= (\b -> if b then pure (Full x) else findM p xs)


-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3

firstRepeat :: Ord a => List a -> Optional a
firstRepeat Nil = Empty
firstRepeat (y :. ys) = found $ eval (findM p ys) ()
  where
    p x = pure (x == y)
    found Empty = firstRepeat ys
    found x = x

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)

fromSet :: Ord a => S.Set a -> List a
fromSet = listh . S.toList

distinct :: Ord a => List a -> List a
distinct xs = fromSet $ exec (foldRight reducer get xs) S.empty
  where
    reducer x acc = acc >>= \a -> State (\s -> (a, S.insert x s))
-- distinct xs = listh . S.toList . S.fromList $ hlist xs

chain :: (Int, P.String)
chain = runState (State (\s -> (1, 'a' : s)) >>= (\a -> State (\s' -> (1 + a, 'b' : s')))) ""
-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy :: Integer -> Bool
isHappy x = computeHapiness x (State (const (0, digits x)))

computeHapiness :: Integer -> State (List Int) Integer -> Bool
computeHapiness x xs
  | digitsToNumber (exec xs Nil) == x &&  (eval xs Nil) > 0 = False
  | (exec xs Nil) == (1 :. Nil) && (eval xs Nil) > 0 = True
  | otherwise = computeHapiness x squareSumState
      where
        squareSumState = xs >>= (\a -> State (\s -> (a + 1, nextState s)))
        nextState = digits . toInteger . sum . map (P.^2)

digits :: Integer -> List Int
digits 0 = Nil
digits x = reverse $ P.fromIntegral (x `mod` 10) :. digits (x `div` 10)

digitsToNumber :: List Int -> Integer
digitsToNumber ys = toInteger $ go (reverse ys) 1
  where
    go Nil _ = 0
    go (x :. xs) mult = x * mult + go xs (mult * 10)

-- computeHapiness' :: Integer -> List Int -> Integer -> Bool
-- computeHapiness' x xs n
--   | digitsToNumber xs == x &&  n > 0 = False
--   | xs == (1 :. Nil) && n > 0 = True
--   | otherwise = computeHapiness' x (digits squareSum) (n+1)
--       where
--         squareSum = toInteger . sum . map (P.^2) $ xs
