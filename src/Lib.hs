{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import qualified Data.Map as M
import qualified Data.Bifunctor as B
import System.Random as R hiding (uniform)
import Control.Monad
import Text.Printf (printf)

type Prob = Double

instance (Show a, Ord a) => Show (Discrete a) where
  show :: (Show a, Ord a) => Discrete a -> String
  show d = concatMap showRow $ toList d
    where showRow (elem, prob) = show elem ++ " (" ++ printf "%.4f" prob ++ ")\n"

-- | Represents categorical distributions only
newtype Discrete a = Discrete {toList :: [(a, Prob)]}

squish :: (Ord a) => Discrete a -> Discrete a
squish (Discrete xs) = Discrete $ M.toList $ M.fromListWith (+) xs

normalize :: Discrete a -> Discrete a
normalize (Discrete xs) = Discrete $ map (\(x, p) -> (x, p / total)) xs
  where total = sum $ map snd xs

expectation :: Discrete Double -> Double
expectation (Discrete xs) = foldl ( \sum (x, p) -> sum + (x * p) ) 0 xs

toDouble :: Discrete Int -> Discrete Double
toDouble (Discrete xs) = Discrete $ map (B.first fromIntegral) xs

sample :: R.StdGen -> Discrete a -> a
sample g (Discrete xs) = aux r xs
  where
    r = fst $ R.randomR (0, 1) g
    aux v ((x, p):ps) = if v < p then x else aux (v - p) ps

sampleN :: R.StdGen -> Int -> Discrete a -> [a]
sampleN g n d = sample g $ Control.Monad.replicateM n d

-- | Want to add squish to fmap
instance Functor Discrete where
  fmap f (Discrete xs) = Discrete $ map (B.first f) xs

instance Applicative Discrete where
    pure x = Discrete [(x, 1)]
    (Discrete fs) <*> (Discrete xs) = Discrete $ do
        (f, p) <- fs
        (x, q) <- xs
        return (f x, p * q)

instance Monad Discrete where
    return = pure
    (Discrete xs) >>= f = Discrete $ do
        (x, p) <- xs
        (y, q) <- toList $ f x
        return (y, p * q)


-- | Simple estimation 
simpleEstimation :: R.StdGen -> Int -> Discrete Double -> Double
simpleEstimation g n d = expectation $
    Discrete $ map (\x -> (x, 1 / fromIntegral n)) $ sampleN g n d

-- | Distributions
bern :: Double -> Discrete Int
bern p = Discrete [(0, 1 - p), (1, p)]

uniform :: [Int] -> Discrete Int
uniform xs = normalize $ Discrete $ map (\x -> (x, 1)) xs

