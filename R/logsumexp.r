# logsumexp ===========================================================================
LCM_fit <- function(x=x,dim=dim) {
  # Returns log(sum(exp(x),dim)) while avoiding numerical underflow.
  # Default is dim = 1 (columns).
  #
  # USAGE: opts = LCM_opts([opts])
  #
  # INPUTS:
  #   x: matrix of likelihood
  #   dim: dimention of x
  #
  # OUTPUTS:
  #   log_sum_exp: log sum exp of x
  #
  #  This function is developed by Sam Gershman as Matlab code(07/2016) and implemented as R code by Yoshiko Kunisato(09/2019)

  if (nargs() == 1){
    dim = 1
  }

  # subtract the largest in each column
  y <- max(x[,dim])
  x <- x-y
  s <- y + log(sum(exp(x),dim))

  # Matlabだと以下。次元ごとにやっている感じがするな（上の感じだと１次元が前提）。
  # y = max(x,[],dim);
  # x = bsxfun(@minus,x,y);
  # s = y + log(sum(exp(x),dim));

  # if log_sum_exp have inifinit value, it is replace by max value
  replace(s, which(is.infinite(x)), y)

  # Matlabだと以下のように無限になったものだけ，maxと置き換えるみたい。
  # i = find(~isfinite(y));
  # if ~isempty(i)
  # s(i) = y(i);
  # end

  return(list(s=s))
}
