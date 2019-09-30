#' Fit latent cause model
#'
#' \code{LCM_fit} fit latent cause model to conditioning data.
#'
#' @param data long format data containing the following variables
#'        (Order and name is exactly the same as following):
#'
#'        ID  Subject ID
#'
#'        CR  Conditioned Response
#'
#'        US  Unconditioned Stimulus
#'
#'        CS  Conditioned Stimului. If using multiple CS, set variables name as CS1,CS2,CS3...
#' @param n_cs number of CS
#' @param opts (optional) structure defining LCM options (see LCM_opts)
#' @return post_mean_alpha: posterior mean alpha
#' @return logBF: log Bayes factor for the alpha>=0 model relative to the alpha=0 model
#' @examples
#' # results <- LCM_fit(data,n_cs,opts)
LCM_fit <- function(data,n_cs,opts) {
  # argument
  if (missing(opts)) {
    opts <- list()
  }
  if (missing(n_cs)) {
    warning("Please set the n_cs(number of CS)")
  }
  # number of alpha values to evaluate
  N <- 50
  # set alpha (range=0~10, number is N)
  alpha <- linspace(0,10,N)
  N_participants <- length(unique(data$ID))
  ID_list <- unique(data$ID)
  post_mean_alpha <- vector()
  logBF <- vector()
  for (s in 1:N_participants) {
    cat('Participants',s, "\n")
    data_subset <- subset(data, ID==ID_list[s])
    lik <- vector()
    b <- vector()
    sd <- vector()
    for (i in 1:N) {
      results <- LCM_lik(alpha[i],data_subset,n_cs,opts)
      lik[i] <- results$lik
    }
    L <- log(sum(exp(lik)))
    P <- exp(lik-L)
    post_mean_alpha[s] <- alpha%*%P
    logBF[s] <- L - log(N) - lik[1]
  }
  return(list(post_mean_alpha=post_mean_alpha,logBF=logBF))
}
