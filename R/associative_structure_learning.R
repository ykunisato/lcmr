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
  r <- X[,1]         #us
  X <- X[,2:ncol(X)] # cues
  D <- ncol(X)
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
  S <- Dist^(-opts_asl$g)


  # Run inference
  for (t in 1:T) {
    # determine how many EM iterations to perform based on ITI
    if(t == T){
      nIter <- 1
    }else{
      nIter <- min(opts_asl$maxIter,round(Dist[t,t+1]))
    }

    ##########################################################################################
    ######### not working from here
    ##########################################################################################

    # calculate (unnormalized) posterior, not including reward
    N <- sum(Z[1:t-1,])                    #cluster counts
    prior <- t(S[1:t-1,t])*Z[1:t-1,]          # ddCRP prior
    prior(find(N==0,1)) = opts_asl$c_alpha(t)    # probability of new cluster
    L <- prior/sum(prior)                  # normalize prior
    xsum <- t(X[1:t-1,])*Z[1:t-1,]           # [D x K] matrix of feature sums
    nu <- opts_asl$sx/(N+opts_asl$sx) + opts_asl$sx

    for (d in 1:D) {
      xhat <-  xsum[d,]/(N+opts_asl$sx)
      L <-  L.*normpdf(X[t,d],xhat,sqrt(nu))  # likelihood
    }

    # reward prediction, before feedback
    post <- L/sum(L)
    results$V[t] <- (X[t,]*W)*t(post)
    results$w <- W
    results$p = post
    if(is.nan(opts_asl$theta)){
      results$V(t) = 1-normcdf(opts_asl$theta,results$V(t),opts_asl$lambda);
    }
    # loop over EM iterations
    for (iter in 1:nIter){
      V <- X[t,]*W;                               # reward prediction
      post <- L.*normpdf(r(t),V,sqrt(opts_asl$sr))   # unnormalized posterior with reward
      post <- post./sum(post)
      results.Zp[t,] <- post
      rpe <- repmat((r[t]-V)*post,D,1)           # reward prediction error
      x <- repmat(t(X[t,]),1,opts_asl$K)
      W <- W + opts_asl$eta(t)*x*rpe            # weight update
      if (psi[t]>0){
        W <- W*(1-repmat(post,D,1))*psi[t]
      }

      results$W[t,iter] = W
      results$P[t,iter] = post
    }
  # cluster assignment
  k <- max(post)                  # maximum a posteriori cluster assignment
  Z[t,k] <- 1
  }


  # store results
  results$Z <- Z
  results$S <- S

  return(list(opts_asl = opts_asl, Dist = Dist, results = results))
}
