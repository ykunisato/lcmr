#' Fit data to latent cause model
#'
#' \code{fit_lcm} fit latent cause model to conditioning data
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
#'        CR  Conditioned Response
#'
#'        US  Unconditioned Stimulus
#'
#'        CS  Conditioned Stimului. If using multiple CS, set variables name as CS1,CS2,CS3...
#' @param n_cs number of CS
#' @param opts (optional) structure defining LCM options (see LCM_opts)
#' @param set_alpha (optional) option of alpha values to evaluate.
#'        You can change max value and numbers of alpha as followings(default: max value = 10, number = 50).
#'        list(num_alpha = 100, max_alpha = 5)
#'
#' @return data post_mean_alpha(posterior mean alpha) and
#' logBFlog(Bayes factor for the alpha>=0 model relative to the alpha=0 model)
#' add to original data
#' @export
#' @examples
#'
#' # results <- fit_lcm(data,n_cs,opts,list(num_alpha = 100, max_alpha = 10))
fit_lcm <- function(data, n_cs, opts, set_alpha) {
    # check argument
    if (missing(opts)) {
        opts <- list()
    }
    if (missing(n_cs)) {
        warning("Please set the n_cs(number of CS)")
    }
    if (missing(set_alpha)) {
        # set default alpha to evaluate
        alpha <- linspace(0, 10, 50)
    }else{
        # set user defined alpha to evaluate
        alpha <- linspace(0, set_alpha$max_alpha, set_alpha$num_alpha)
    }
    # fitting
    data <- data %>%
        group_by(ID) %>%
        nest() %>%
        mutate(fit = map(data, ~estimate_lcm_a(data = ., n_cs, opts, alpha))) %>%
        unnest(cols = fit)
    return(data)
}
