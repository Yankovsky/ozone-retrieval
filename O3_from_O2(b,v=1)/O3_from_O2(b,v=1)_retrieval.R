source('science/lib.R')
Init('science/O3_from_O2(b,v=1)/data.csv', 'science/O3_from_O2(b,v=1)/constants.csv')

MAX = 1000
result <- data.frame(a=numeric(), b=numeric(), c=numeric(), d=numeric(), e=numeric(), f=numeric())
for (i in 1:nrow(data)) {  
  atomic.oxygen.denceties <- numeric()
  for (j in 1:MAX) {
    new.dencity <- 
      (
        (
          D(i, 'O2(b,1)') * 
            (
              C('A(b1)') +
              C('K(b1;O)') * D(i, 'O(3P)') +
              C('K(b1;O2)') * exp(-312 / D(i, 'Tg')) * D(i, 'O2') +
              C('K(b1;N2)') * D(i, 'N2')
            ) -
            D(i, 'O2') * D(i, 'J_b1')
        ) * 
          (
            C('A(1D)') + 
              C('K(1D;O)') * D(i, 'O(3P)') +
              C('K(1D;O2)') * exp(67/D(i, 'Tg')) * D(i, 'O2') +
              C('K(1D;N2)') * exp(107 / D(i, 'Tg')) * D(i, 'N2') 
          ) /
          (
            C('F(D)') * C('K(1D;O2)') * exp(67 / D(i, 'Tg')) * C('F(b1)') *
              D(i, 'Hartley') * D(i, 'O2')
          )
      ) -
      (
        D(i, 'O2') * 
          (
            D(i, 'S.-R. C.') +
              C('F(alpha)') * D(i, 'LyA')
          ) /
          (
            C('F(D)') * D(i, 'Hartley')
          )
      )
    atomic.oxygen.denceties <- c(atomic.oxygen.denceties, new.dencity)
    if ((j <= 200 && j %% 20 == 0) || (j > 200 && j %% 50 == 0)) {
      m <- mean(atomic.oxygen.denceties)
      s <- sd(atomic.oxygen.denceties)
      v <- s / m
      r <- m / data[i, ]$O3
      result <- rbind(result, c(data[i, ]$'z, km', j, m, s, v, r))
    }
  }
}
colnames(result) <- c('height', 'size', 'mean', 'sd', 'variation', 'fraction')
write.csv(result, file="science/O3_from_O2(b,v=1)/results/result.csv")