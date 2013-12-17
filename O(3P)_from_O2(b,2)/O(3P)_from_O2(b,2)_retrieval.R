source('science/lib.R')
Init('science/O(3P)_from_O2(b,2)/data.csv', 'science/O(3P)_from_O2(b,2)/constants.csv')

MAX = 1000
result <- data.frame(a=numeric(), b=numeric(), c=numeric(), d=numeric(), e=numeric(), f=numeric())
for (i in 1:nrow(data)) {  
  atomic.oxygen.denceties <- numeric()
  for (j in 1:MAX) {
    new.dencity <- 
      (1 / C('K(b2;O)')) * 
      (
        D(i, 'J_b2') * D(i, 'O2') / data[i, ]$'Nb2' -
          (
            C('A(b2)') +
              D(i, 'O2') * C('K(b2;O2)') * exp(-596 / D(i, 'Tg')) +
              D(i, 'N2') * C('K(b2;N2)') +
              D(i, 'O3') * C('K(b2;O3)')
          )
      )
    atomic.oxygen.denceties <- c(atomic.oxygen.denceties, new.dencity)
    if ((j <= 200 && j %% 20 == 0) || (j > 200 && j %% 50 == 0)) {
      m <- mean(atomic.oxygen.denceties)
      s <- sd(atomic.oxygen.denceties)
      v <- s / m
      r <- m / data[i, ]$Etalon
      result <- rbind(result, c(data[i, ]$'z, km', j, m, s, v, r))
    }
  }
}
colnames(result) <- c('height', 'size', 'mean', 'sd', 'variation', 'fraction')
write.csv(result, file="science/O(3P)_from_O2(b,2)/results/result.csv")