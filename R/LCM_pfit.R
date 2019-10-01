#' Fit latent cause model using parallel computing
#'
#' \code{LCM_pfit} fit latent cause model to conditioning data using parallel computing
#'
#' @importFrom pracma linspace
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom furrr future_map
#' @importFrom future plan
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
#' @param parallel If set TRUE(default is FALSE), using parallel processing
#'
#' @return data post_mean_alpha(posterior mean alpha) and
#' logBFlog(Bayes factor for the alpha>=0 model relative to the alpha=0 model)
#' add to original data
#' @export
#' @examples
#'
#' # results <- LCM_pfit(data,n_cs,opts,parallel=TRUE)
LCM_pfit <- function(data,n_cs,opts,parallel=FALSE) {
  # check argument
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
  # parallel or single
  if(isTRUE(parallel) == 1){
    plan("multisession")
  }else{
    plan("sequential")
  }
  # fitting
  data <- data %>%
    group_by(ID) %>%
    nest() %>%
    mutate(fit=future_map(data,~LCM_pfit_single(data=.,n_cs,opts,alpha))) %>%
    unnest(cols=fit)
  return(data)
}


#' Function to fit latent cause model to data for parallel computing
#'
#' \code{LCM_fit} is function to fit latent cause model to data for parallel computing
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
#' @param alpha vector of alpha
#' @return df data frame containing post_mean_alpha(posterior mean alpha) and
#' logBF(: )log Bayes factor for the alpha>=0 model relative to the alpha=0 model)
LCM_pfit_single <- function(data,n_cs,opts,alpha) {
    lik <- vector()
    for (i in 1:length(alpha)) {
      results <- LCM_lik(alpha[i],data,n_cs,opts)
      lik[i] <- results$lik
    }
    L <- log(sum(exp(lik)))
    P <- exp(lik-L)
    post_mean_alpha <- alpha%*%P
    logBF <- L - log(length(alpha)) - lik[1]
    df <- data.frame(post_mean_alpha, logBF)
    return(df)
}
