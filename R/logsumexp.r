# logsumexp ===========================================================================
logsumexp <- function(x=x) {
  # Returns log(sum(exp(x)))
  # USAGE: log_sum_exp = logsumexp(x)
  #
  # INPUTS:
  #   x: vector of likelihood
  #
  # OUTPUTS:
  #   log_sum_exp: log sum exp of x
  #
  #  This function is developed by Sam Gershman as Matlab code(07/2016) and implemented as R code by Yoshiko Kunisato(09/2019)
  #  I did not avoide the numerical underflow.

  # subtract the largest in each column
  y <- max(x)
  x <- x-y
  s <- y + log(sum(exp(x)))
  return(s)
}
