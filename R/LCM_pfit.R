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
#'
#' @return data post_mean_alpha(posterior mean alpha) and
#' logBFlog(Bayes factor for the alpha>=0 model relative to the alpha=0 model)
#' add to original data
#' @export
#' @examples
#'
#' # results <- LCM_pfit(data,n_cs,opts,parallel=TRUE)
LCM_pfit <- function(data, n_cs, opts) {
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
    alpha <- linspace(0, 10, N)
    # set parallel computing
    plan("multisession")
    # fitting
    data <- data %>% group_by(ID) %>% nest() %>% mutate(fit = future_map(data, ~LCM_alpha(data = ., 
        n_cs, opts, alpha))) %>% unnest(cols = fit)
    return(data)
}
