Init <- function(data.file.name, constants.file.name) {
  data <<- read.csv(data.file.name, sep=';', check.names=F)
  constants <<- read.csv(constants.file.name, sep=';', check.names=F)
}

GetValueWithError <- function(value, error) {
  value * (1 + rnorm(1, sd=error/2))
  # value * (1 + runif(1, -error, error))
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