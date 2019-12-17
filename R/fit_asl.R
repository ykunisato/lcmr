#' Fit data to associative and structural learning model
#'
#' \code{fit_asl} fit latent cause model to conditioning data
#'
#' @importFrom pracma linspace
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom tidyr unnest
#'
#' @param data long format data containing the following variables
#'        (Order and name is exactly the same as following):
#'
#'        ID  Subject ID
#'
#'        Time stimulus onset time
#'
#'        CR  Conditioned Response
#'
#'        US  Unconditioned Stimulus
#'
#'        CS  Conditioned Stimului. If using multiple CS, set variables name as CS1,CS2,CS3...
#' @param n_cs number of CS
#' @param asl_opts (optional)
#' @param set_alpha (optional) option of alpha values to evaluate.
#'        You can change max value and numbers of alpha as followings(default: max value = 10, number = 50).
#'        list(num_alpha = 100, max_alpha = 5)
#'
#' @return return the fit, parameters and plc_vcr.
#' fit is fitting results. parameters is parameters estmated including post_mean_alpha(posterior mean alpha) and
#' logBFlog(Bayes factor for the alpha>=0 model relative to the alpha=0 model).
#' plc_vcr is matrix of latent cause posterior and V & CR predicted.
#'
#' @export
#' @examples
#'
#' # results <- fit_asl(data,n_cs,asl_opts,list(num_alpha = 100, max_alpha = 10))
fit_asl <- function(data, n_cs, asl_opts, set_alpha) {
  # check argument
  if (missing(asl_opts)) {
    asl_opts <- list()
  }
  if (missing(n_cs)) {
    warning("Please set the n_cs(number of CS)")
  }
  if (missing(set_alpha)) {
    # set default alpha to evaluate
    alpha <- linspace(0, 1, 50)
  }else{
    # set user defined alpha to evaluate
    alpha <- linspace(0, set_alpha$max_alpha, set_alpha$num_alpha)
  }
  # fitting
  fit <- data %>%
    group_by(ID) %>%
    nest() %>%
    mutate(fit = map(data, ~estimate_asl_a(data = ., n_cs, asl_opts, alpha))) %>%
    unnest(cols = fit)
  # extract matrix of latent cause posterior and V & CR predicted
  b <- NULL
  sd <- NULL
  plc_vcr <- NULL


  for (i in 1:length(fit$ID)) {
    estimate <- compute_asl_loglik(fit$post_mean_alpha[i], fit$data[[i]], n_cs, asl_opts)
    b[i] <- estimate$latents$b
    sd[i] <- estimate$latents$sd
    index_z <- as.data.frame(which(estimate$latents$results$Z == 1, arr.ind = TRUE))
    plc_vcr <-rbind(plc_vcr, data.frame(ID = rep(fit$ID[i],length(estimate$latents$results$V)),
                                        v =estimate$latents$results$V,
                                        cr = estimate$latents$CR,
                                        cause = index_z$col))
  }

  parameters <- data.frame(ID = fit$ID,
                           post_mean_alpha = fit$post_mean_alpha,
                           b = b,
                           sd = sd)

  return(list(fit = fit, parameters = parameters, plc_vcr = plc_vcr))
}
