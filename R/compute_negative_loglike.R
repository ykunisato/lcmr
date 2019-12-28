#' Compute log likelihood of model selected
#'
#' \code{compute_loglike} compute log likelihood of data under the latent cause model.
#'
#' @param param parameter
#' @param data single-subject data
#' @param model 1 = latent cause model, 2 = latent cause modulated RW model
#' @param opts options
#'
#' @return lik: log-likelihood
#' @return latents: structure containing latent variables:
#'
#'               b: beta coefficient mapping model CR to  measured CR
#'
#'               sd: maximum likelihood standard deviation
#'
#'               CR: predicted CR

compute_loglike <- function(param, data, model, opts) {
  # set concentration parameter
  if (missing(opts)) {
    opts <- list()
  }
  opts$c_alpha <- param[1]
  ncol_data <- ncol(data) 
  if(model == 1){
    results <- infer_lcm(data[, 2:ncol_data], opts)
  }else if(model==2){
    if(length(param)>1){
      opts$eta <- param[2]
    }
    results <- infer_lcm_rw(data[, 2:ncol_data], opts)
  }
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



#' Compute negative log likelihood of model selected
#'
#' \code{compute_negative_loglike} compute negative log likelihood of data under the latent cause model.
#'
#' @param param parameter
#' @param data single-subject data
#' @param model 1 = latent cause model, 2 = latent cause modulated RW model
#' @param opts options
#'
#' @return negative log likelihood

compute_negative_loglike <- function(param, data, model, opts) {
  if (missing(opts)) {
    opts <- list()
  }
  results <- compute_loglike(param, data, model, opts)
  return(-results$lik)
}