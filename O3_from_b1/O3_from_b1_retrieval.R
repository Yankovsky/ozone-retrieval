data <- read.csv('science/O3_from_b1/data.csv', sep=';', check.names=F)
constants <- read.csv('science/O3_from_b1/constants.csv', sep=';', check.names=F)

GetValueWithError <- function(value, error) {
  value * (1 + rnorm(1, sd=error/2))
}

C <- function(name) {
  constant <- constants[, name]
  error = if (is.na(constant[2])) 0 else constant[2]
  GetValueWithError(constant[1], error)
}

D <- function(i, name) {
  if (is.null(data[i, name]))
    stop(paste('no parameter with name', name))
  GetValueWithError(data[i, name], constants[2, 'Rest'])  
}

MAX = 1000
result <- data.frame(a=numeric(), b=numeric(), c=numeric(), d=numeric(), e=numeric(), f=numeric())
for (i in 1:nrow(data)) {  
  atomic.oxygen.denceties <- numeric()
  for (j in 1:MAX) {
    new.dencity <- 
    (
      (
        data[i, ]$'O2(b,1)' * 
        (
          C('A(b1)') + C('K(b1;O2)') * D(i, 'O2') * exp(-312 / D(i, 'Tg')) +
          C('K(b1;N2)') * D(i, 'N2') + C('K(b1;O)') * D(i, 'O(3P)')
        ) -
        D(i, 'O2') * D(i, 'J_b1')
      ) * 
      (
        C('A(1D)') + D(i, 'O2') * C('K(1D;O2)') * exp(67/D(i, 'Tg')) +
        D(i, 'N2') * C('K(1D;N2)') * exp(107 / D(i, 'Tg'))
      ) /
      (
        D(i, 'Hartley') * C('F(D)') * C('K(1D;O2)') *
        exp(67 / D(i, 'Tg')) * D(i, 'O2') * C('F(b1)')
      )
    ) -
    (
      D(i, 'O2') * 
      (
        D(i, 'S.-R. C.') + D(i, 'LyA') * C('F(alpha)')
      ) /
      (
        D(i, 'Hartley') * C('F(D)')
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
write.csv(result, file="science/O3_from_b1/results/result.csv")