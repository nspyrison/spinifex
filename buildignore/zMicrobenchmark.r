library(microbenchmark)
## Fractions
(b<-microbenchmark(.8,
               8/10,
               8L/10L,
               print(.8),
               print(8/10),
               print(8L/10L)
               ))
## Integers
microbenchmark(1,
               1L,
               0+1,
               1/1,
               0L+1L,
               1L/1L
)
## Boolean
microbenchmark(TRUE,
               FALSE,
               T,
               F,
               0,
               1,
               0L,
               1L
)

