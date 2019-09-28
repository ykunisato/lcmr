#' Fit latent cause model to data.
#' INPUTS:
#'   data - long format data containing the following variables:
#'         ID: Subject ID
#'         CR: Conditioned Response
#'         US: Unconditioned Stimulus
#'         CS: Conditioned Stimului(CS1,CS2,CS3...)
#'         n_cs - number of CS
#'        opts (optional) - structure defining LCM options (see LCM_opts.m)
#' OUTPUTS:
#'   results - [nSubjects x 1] structure containing the following fields:
#'               .alpha - concentration parameter values
#'               .P - posterior probability distribution over alpha
#'               .lik - log-likelihood for each alpha value
#'               .latents - latent variables for each alpha value (see LCM_infer)
#'               .logBF - log Bayes factor for the alpha>=0 model relative to the alpha=0 model
#' @examples
#' results <- LCM_fit(data,opts)
LCM_fit <- function(data=data,n_cs=n_cs,opts=opts) {
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
