#' Compute log likelihood in associative and structual learning model
#'
#' \code{compute_asl_loglik} compute log likelihood of data under the latent cause model.
#'
#' @param alpha concentration parameter
#' @param data single-subject data
#' @param n_cs number of CS
#' @param opts_asl options structure
#'
#' @return lik: log-likelihood
#' @return latents: structure containing latent variables:
#'
#'               b: beta coefficient mapping model CR to  measured CR
#'
#'               sd: maximum likelihood standard deviation
#'
#'               CR: predicted CR
#' @export
#' @examples
#' # results <- compute_asl_loglik(alpha,data,n_cs,opts_asl)

compute_asl_loglik <- function(alpha, data, n_cs, opts_asl) {
  # set concentration parameter
  if (nargs() < 4) {
    opts_asl <- list()
  }
  opts_asl$c_alpha <- alpha
  # un particle filter
  max_index_cs <- 3 + n_cs
  results <- learn_associative_structure(data[, 2:max_index_cs], opts_asl)
  # use linear regression to fit model output to CR
  N <- length(results$V)
  X <- results$V
  # maximum likelihood regression coefficients
  b <- (matrix(as.matrix(X),1,N)%*%matrix(as.matrix(data[,1]),N,1))/(matrix(as.matrix(X),1,N) %*% matrix(as.matrix(X),N,1))
  # predicted CR
  CRpred <- X %*% b
  # maximum likelihood standard deviation
  sd = sqrt(mean((data$CR - CRpred)^2))
  # log-likelihood
  lik = sum(log(dnorm(data$CR - CRpred, mean = 0, sd = sd)))
  # return latent variables
  latents <- list()
  latents$results = results
  latents$b = b
  latents$sd = sd
  latents$CR = CRpred
  return(list(lik = lik, latents = latents))
}
