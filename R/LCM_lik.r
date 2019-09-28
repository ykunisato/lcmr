#' Compute log likelihood
#'
#' \code{LCM_lik} compute log likelihood of data under the latent cause model.
#'
#' @param alpha concentration parameter
#' @param data single-subject data
#' @param n_cs number of CS
#' @param opts options structure
#'
#' @return lik: log-likelihood
#' @return latents: structure containing latent variables:
#'
#'               b: beta coefficient mapping model CR to  measured CR
#'
#'               sd: maximum likelihood standard deviation
#'
#'               CR: predicted CR
#' @examples
#' # results <- LCM_lik(alpha,data,n_cs,opts)

LCM_lik <- function(alpha,data,n_cs,opts) {
  # set concentration parameter
  opts <- list()
  opts$c_alpha <- alpha
  # un particle filter
  max_index_cs <- 3+n_cs
  results <- LCM_infer(cbind(data$US,data[,4:max_index_cs]),opts)
  # use linear regression to fit model output to CR
  N <- length(results$V)
  X <- results$V
  # maximum likelihood regression coefficients
  b <-(t(X)%*%data$CR)/(t(X)%*%X)
  # predicted CR
  CRpred <- X%*%b
  #  maximum likelihood standard deviation
  sd = sqrt(mean((data$CR - CRpred)^2))
  # log-likelihood
  lik = sum(log(dnorm(data$CR-CRpred, mean = 0, sd = sd)))
  # return latent variables
  latents <- list()
  latents$results = results
  latents$b = b
  latents$sd = sd
  latents$CR = CRpred
  return(list(lik=lik,latents=latents))
}
