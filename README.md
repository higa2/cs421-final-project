# cs421-final-project
Final Project for CS421

## How to run code
```bash
stack build random
stack ghci
:l main.hs
```

### Example: Sampling from a distribution
```haskell
sampleN g 100 $ normal 0 1
    where
        g = R.mkStdGgen 0
```

If you want to implement your own distributions there are several examples in Main.hs. 



