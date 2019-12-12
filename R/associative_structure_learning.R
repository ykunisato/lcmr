#' Latent cause-modulated Rescorla-Wagner model
#'
#' \code{learn_associative_structure} conduct local maximum a posteriori inference
#' for associative and structuallearning
#'
#' @importFrom pracma histc
#'
#' @param X matrix of stimulus inputs(intensity) consisting of the number of trial rows and
#' the number of stimulus features columns. The first feature (column 1) is the US,
#' and the rest of the features (column 2 through D) are CSs.
#' @param time vector of time(sec)
#' @param opts_asl (optional)list containing various options.
#' @return V: vector of conditioned response on each trial
#' @return Zp: latent cause posterior before observing US(Trial*K)
#' @return Z: latent cause posterior(Trial*K)
#' @export
#' @examples
#' # results <- ilearn_associative_structure(X, time, opts_asl)
#'
learn_associative_structure <- function(X, time, opts_asl){
  # warning
  if (nargs() < 2) {
    stop("please set the temporal distance")
  }

  # set options
  def_opts <- list()
  def_opts$c_alpha <- 0.1
  def_opts$g <- 1
  def_opts$psi <- 0
  def_opts$eta <- 0.2
  def_opts$maxIter <- 3
  def_opts$w0 <- 0
  def_opts$sr <- 0.4
  def_opts$sx <- 1
  def_opts$theta <- 0.03
  def_opts$lambda <- 0.005
  def_opts$K <- 15

  if (nargs() < 3) {
    opts_asl <- NULL
  }
  if (length(opts_asl) == 0) {
    opts_asl = def_opts
  } else {
    F <- names(def_opts)
    for (i in 1:length(F)) {
      if (eval(parse(text = paste0("length(opts_asl$", F[i], ")==0")))) {
        eval(parse(text = paste0("opts_asl$", F[i], "=def_opts$", F[i])))
      }
    }
  }

  # Initialization
  results <- NULL
  T <- nrow(X)
  D <- ncol(X) -1 #ecluding US
  if(length(opts_asl$c_alpha)==1){
    opts_asl$c_alpha <-opts_asl$c_alpha*matrix(1,T,1)
  }
  if(length(opts_asl$eta)==1){
    opts_asl$eta <-opts_asl$eta*matrix(1,T,1)
  }
  psi <- opts_asl$psi*matrix(1,T,1)
  Z <- matrix(0,T,opts_asl$K)
  results$V <- matrix(0,T,1)
  W <- matrix(0,D,opts_asl$K) + opts_asl$w0;

  # construct distance matrix
  Dist <- matrix(0,T,T)
  for (i in 1:T) {
    for (j in 1:T) {
      Dist[i,j] = abs(time[i]-time[j])
    }
  }
  S <- Dist^(-opts_asl$g);

  return(list(opts_asl = opts_asl, Dist = Dist))
}
