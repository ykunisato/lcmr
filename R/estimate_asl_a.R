#' Estimate alpha and logBF
#'
#' \code{estimate_asl_a} is function to estimate alpha and logBF
#' @importFrom tidyr nest
#'
#' @param data data from fit_asl or fit_asl_parallel
#' @param n_cs number of CS
#' @param opts (optional) structure defining ASL options
#' @param alpha vector of alpha
#' @return df data frame containing post_mean_alpha(posterior mean alpha),
#' logBF(log Bayes factor for the alpha>=0 model relative to the alpha=0 model),
#' loglikelihood and probability each alpha.
estimate_asl_a <- function(data, n_cs, opts, alpha) {
  lik <- numeric(length(alpha))
  for (i in 1:length(alpha)) {
    results <- compute_asl_loglik(alpha[i], data, n_cs, opts)
    lik[i] <- results$lik
  }
  L <- log(sum(exp(lik)))
  P <- exp(lik - L)
  post_mean_alpha <- alpha %*% P
  logBF <- L - log(length(alpha)) - lik[1]
  df <- data.frame(post_mean_alpha,
                   logBF,
                   data.frame(alpha,lik,P))%>%
    nest(prob_alpha=c(alpha,lik,P))
  return(df)
}
