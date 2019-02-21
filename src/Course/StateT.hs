{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT1 where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a = StateT {runStateT :: s -> f (a, s)}

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) :: (a -> b) -> StateT s f a -> StateT s f b
  f <$> (StateT stateFn) = StateT (\s -> (\(a, s') -> (f a, s')) <$> stateFn s)

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure :: a -> StateT s f a
  pure a = StateT (\s -> pure (a, s))
  (StateT stateF) <*> (StateT stateV) = StateT (\s -> stateV s >>= doApp)
    where
      doApp (a, s) = stateF s >>= (\(f', s') -> pure (f' a, s'))

-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) :: forall a b. (a -> StateT s f b) -> StateT s f a -> StateT s f b
  g =<< (StateT stateTFn)  = StateT (\s -> stateTFn s >>= mon)
    where
      mon :: Monad f => (a, s) -> f (b, s)
      mon (a, s) = runStateT (g a) s

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a = StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' :: (s -> (a, s)) -> State' s a
state' f =  StateT (pure . f)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' :: State' s a -> s -> (a, s)
runState' (StateT fs) = runExactlyOne . fs

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT :: Functor f => StateT s f a -> s -> f s
execT (StateT f) s = snd <$> f s

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' :: State' s a -> s -> s
exec' state = snd . runState' state

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT :: Functor f => StateT s f a -> s -> f a
evalT (StateT f) s = fst <$> f s

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
eval' :: State' s a -> s -> a
eval' state = runExactlyOne . evalT state

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT :: Applicative f => StateT s f s
getT = StateT (\s -> pure (s, s))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT :: Applicative f => s -> StateT s f ()
putT s = StateT (\_ -> pure ((), s))

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' :: (Ord a, Num a) => List a -> List a
distinct' xs = eval' (filtering p xs) S.empty
  where
    p x = StateT (\s -> ExactlyOne (S.notMember x s, S.insert x s))

distinct'' :: (Ord a, Num a) => List a -> List a
distinct'' xs = go
  where
    initState = StateT (\s' -> ExactlyOne (Nil, s'))
    go = eval' (foldRight reducer initState xs) S.empty
    reducer x acc = acc >>=
      \a -> StateT (\s -> if S.member x s
                          then ExactlyOne (a, s)
                          else ExactlyOne (x :. a, S.insert x s))
      -- (\a -> if S.member x a
      --        then StateT (\s' -> ExactlyOne (a, s'))
      --        else StateT (\s' -> ExactlyOne (S.insert x a, x :. s')))

-- chain' = runStateT (StateT (\s -> ExactlyOne (0, "a" :. s )) >>= (\a -> StateT (\s' -> ExactlyOne (a + 1, "b" :. s')))) Nil
-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF :: (Ord a, Num a) => List a -> Optional (List a)
distinctF xs = reverse <$> evalT (filtering p xs) S.empty
  where
    p x
      | x < 100 = StateT (\s -> Full (S.notMember x s, S.insert x s))
      | otherwise = StateT (const Empty)

-- distinctF = filtering (\x -> pure (x < 100)) . distinct'

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a = OptionalT {runOptionalT :: f (Optional a)}

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  f <$> (OptionalT funct) = OptionalT (((<$>) . (<$>)) f funct)

-- | Implement the `Applicative` instance for `OptionalT f` given a Monad f.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]

-- onFull :: Applicative f => (t -> f (Optional a)) -> Optional t -> f (Optional a)
-- onFull g o =
--   case o of
--     Empty -> pure Empty
--     Full a -> g a
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Applicative (OptionalT f) where
  pure = OptionalT . pure . pure
  (<*>) :: forall a b. OptionalT f (a -> b) -> OptionalT f a -> OptionalT f b
  -- (OptionalT f) <*> (OptionalT val) = OptionalT ((<*>) <$> f <*> val)
  (OptionalT f) <*> (OptionalT val) = OptionalT (lift2 (<*>) f val)

    -- OptionalT appf
    -- where
    --   appf = f >>= \h -> onFull applyVal h
    --   applyVal :: (a -> b) -> f (Optional b)
    --   applyVal g = val >>= \optVal -> pure (g <$> optVal)

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  -- (=<<) :: (a -> OptionalT f (Optional b)) -> OptionalT f (Optional a) -> OptionalT f (Optional b)
  f =<< (OptionalT mon) = undefined
    -- OptionalT $ mon >>= runOptionalT . f

  -- (mon >>= doMon)
  --   where
  --     doMon (Full a) = runOptionalT (f a)
  --     doMon Empty = pure Empty

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a = Logger (List l) a deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  f <$> (Logger list log) = Logger list (f log)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure = Logger Nil
  (Logger list f) <*> (Logger list' val) = Logger (list ++ list') (f val)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  f =<< (Logger list val) = g $ f val
     where
       g (Logger list' a) = Logger (list ++ list') a

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 :: l -> a -> Logger l a
log1 l = Logger (l :. Nil)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty

-- data Logger l a = Logger (List l) a deriving (Eq, Show)
-- data OptionalT f a = OptionalT {runOptionalT :: f (Optional a)}

distinctG :: (Integral a, Show a) => List a -> Logger Chars (Optional (List a))
distinctG xs = filterEmptyness $ runOptionalT (evalT (filtering p (reverse xs)) S.empty)
  where
    filterEmptyness (Logger log vals) = Logger (filter (not . isEmpty) log) (reverse <$> vals)
    logFilter x = if even x then "even number: " ++ show' x else Nil
    p x
      | x < 100 = StateT (\s -> OptionalT (log1 (logFilter x) (Full (S.notMember x s, S.insert x s))))
      | otherwise = StateT (\s -> OptionalT (log1 ("aborting > 100: " ++ show' x) Empty))


onFull :: Applicative f => (t -> f (Optional a)) -> Optional t -> f (Optional a)
onFull g o =
  case o of
    Empty -> pure Empty
    Full a -> g a
