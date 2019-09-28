#' Compute log-likelihood of data under the latent cause model.
#' \code{LCM_lik}
#'
#' @param
#'   alpha - concentration parameter
#'   data - single-subject data
#'   opts - options structure
#'
#' @return
#'   lik - log-likelihood
#'   latents - structure containing latent variables:
#'               .b - beta coefficient mapping model CR to  measured CR
#'               .sd - maximum likelihood standard deviation
#'               .CR - predicted CR
#' @examples
#' LCM_lik(alpha,data,[opts])

LCM_lik <- function(alpha=alpha,data=data,n_cs=n_cs,opts=opts) {
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
