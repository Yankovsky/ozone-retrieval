data <- read.csv('science/data.csv', sep=';', check.names=F)
constants <- read.csv('science//constants.csv', sep=';', check.names=F)

GetValueWithError <- function(value, error) {
  value * (1 + rnorm(1, sd=error/2))
}

GetConstant <- function(name) {
  constant <- constants[, name]
  GetValueWithError(constant[1], constant[1])
}

GetDataValue <- function(i, name) {
  GetValueWithError(data[i, name], constants[2, 'Rest'])  
}

MAX = 1000
result <- data.frame(a=numeric(), b=numeric(), c=numeric(), d=numeric(), e=numeric(), f=numeric())
for (i in 1:nrow(data)) {  
  atomic.oxygen.denceties <- numeric()
  for (j in 1:MAX) {
    new.dencity <- 
      (1 / GetConstant('K(b2;O)')) * 
      (
        GetDataValue(i, 'J_b2') * GetDataValue(i, 'O2') / data[i, ]$'Nb2' -
          (
            GetConstant('A(b2)') +
              GetDataValue(i, 'O2') * GetConstant('K(b2;O2)') * exp(-596 / GetDataValue(i, 'Tg')) +
              GetDataValue(i, 'N2') * GetConstant('K(b2;N2)') +
              GetDataValue(i, 'O3') * GetConstant('K(b2;O3)')
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
write.csv(result, file="science//result.csv")