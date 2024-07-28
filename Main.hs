import Src.Lib
import Control.Monad
import System.Random as R hiding (uniform)

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