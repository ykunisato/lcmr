# LCM_lik ===========================================================================
LCM_lik <- function(alpha=alpha,data=data,opts=opts) {
  # Compute log-likelihood of data under the latent cause model.
  #
  # USAGE: LCM_lik(alpha,data,[opts])
  #
  # INPUTS:
  #   alpha - concentration parameter
  #   data - single-subject data
  #   opts - options structure
  #
  # OUTPUTS:
  #   lik - log-likelihood
  #   latents - structure containing latent variables:
  #               .b - beta coefficient mapping model CR to  measured CR
  #               .sd - maximum likelihood standard deviation
  #               .CR - predicted CR
  #
  #  This function is developed by Sam Gershman as Matlab code(07/2016) and implemented as R code by Yoshiko Kunisato(09/2019)

  # set concentration parameter
  opts <- list()
  opts$c_alpha <- alpha
  # un particle filter
  results <- LCM_infer(cbind(data$US,data$CS),opts)

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
