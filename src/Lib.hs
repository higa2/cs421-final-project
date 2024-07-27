{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import qualified Data.Map as M
import qualified Data.Bifunctor as B
import System.Random as R hiding (uniform)
import Control.Monad
import Text.Printf (printf)

type Prob = Double

class Sampleable d where
  sample :: R.StdGen -> d a -> a

data Dist a where
  Return :: a -> Dist a
  Bind :: Dist b -> (b -> Dist a) -> Dist a
  Primitive :: Sampleable d => d a -> Dist a

instance Functor Dist where
  fmap = liftM

instance Applicative Dist where
  pure = Return
  f <*> x = Bind f (\g -> Bind x (Return . g))

instance Monad Dist where
  (>>=) = Bind

instance Sampleable Dist where
  sample g (Return x) = x
  sample g (Bind d f) = sample g1 y where
    y = f (sample g2 d)
    (g1, g2) = split g
  sample g (Primitive d) = sample g d

sampleN :: (Sampleable d, Monad d) => R.StdGen -> Int -> d a -> [a]
sampleN g n d = sample g $ Control.Monad.replicateM n d

-- | Represents discrete distributions as samplable
instance (Show a, Ord a) => Show (Discrete a) where
  show :: (Show a, Ord a) => Discrete a -> String
  show d = concatMap showRow $ toList d
    where showRow (elem, prob) = show elem ++ " (" ++ printf "%.4f" prob ++ ")\n"

newtype Discrete a = Discrete {toList :: [(a, Prob)]}

normalize :: Discrete a -> Discrete a
normalize (Discrete xs) = Discrete $ map (\(x, p) -> (x, p / total)) xs
  where total = sum $ map snd xs

expectation :: Discrete Double -> Double
expectation (Discrete xs) = foldl ( \sum (x, p) -> sum + (x * p) ) 0 xs

toDouble :: Discrete Int -> Discrete Double
toDouble (Discrete xs) = Discrete $ map (B.first fromIntegral) xs

instance Sampleable Discrete where
  sample g (Discrete xs) = aux r xs
    where
      r = fst $ R.randomR (0, 1) g
      aux v ((x, p):ps) = if v < p then x else aux (v - p) ps

-- | Discrete Distributions
bern :: Double -> Dist Int
bern p = Primitive $ Discrete [(0, 1 - p), (1, p)]

uniform :: [a] -> Discrete a
uniform xs = normalize $ Discrete $ map (\x -> (x, 1)) xs
uniformD :: [a] -> Dist a
uniformD xs = Primitive $ uniform xs

binom :: Int -> Double -> Dist Int
binom 0 p = Return 0
binom 1 p = bern p
binom n p = liftM2 (+) (bern p) (binom (n - 1) p)

-- | Represent continuous distributions as sampleable
newtype Continuous a = Continuous {toInvCDF :: Prob -> a}

instance Sampleable Continuous where
  sample g (Continuous invCdf) = invCdf r
    where
      r = fst $ R.randomR (0, 1) g

-- | Continuous Distributions

uniformC :: Double -> Double -> Dist Double
uniformC 0 1 = Primitive $ Continuous id
uniformC a b = fmap (\x -> (b - a) * x + a) (uniformC 0 1)

-- | Credit to https://www.johndcook.com/blog/haskell-inverse-normal-cdf/
rationalApprox :: Double -> Double
rationalApprox t = t - ((0.010328*t + 0.802853)*t + 2.515517) /
               (((0.001308*t + 0.189269)*t + 1.432788)*t + 1.0);

phiInverse :: Prob -> Double
phiInverse p =
    if p < 0.5
        then  - rationalApprox ( sqrt (- (2.0 * log p)) )
        else  rationalApprox ( sqrt (- (2.0 * log (1.0 - p))) )
normal :: Double -> Double -> Dist Double 
normal 0 1 = Primitive $ Continuous phiInverse
normal m s = fmap (\x -> s * x + m) (normal 0 1)

-- | Simple MC estimation 
simpleEstimation :: R.StdGen -> Int -> Dist Int -> Double
simpleEstimation g n d = expectation $ toDouble $
  uniform $ sampleN g n d