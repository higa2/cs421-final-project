# cs421-final-project
Final Project for CS421

## How to run code
```bash
stack build random
stack ghci
:l src/Lib.hs
```

### Simple estimation on discrete distribution
```haskell
g = R.mkStdGgen 0
simpleEstimation g 10 $ toDouble $ uniform [1..6]
```



