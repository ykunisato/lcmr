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
#' @return return the fit, parameters and plc_vcr.
#' fit is fitting results. parameters is parameters estmated including post_mean_alpha(posterior mean alpha) and
#' logBFlog(Bayes factor for the alpha>=0 model relative to the alpha=0 model).
#' plc_vcr is matrix of latent cause posterior and V & CR predicted.
#'
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
    fit <- data %>%
        group_by(ID) %>%
        nest() %>%
        mutate(fit = map(data, ~estimate_lcm_a(data = ., n_cs, opts, alpha))) %>%
        unnest(cols = fit)
    # extract matrix of latent cause posterior and V & CR predicted
    b <- NULL
    sd <- NULL
    plc_vcr <- NULL

    for (i in 1:length(fit$ID)) {
        estimate <- compute_lcm_loglik(fit$post_mean_alpha[i], fit$data[[i]], n_cs, opts)
        b[i] <- estimate$latents$b
        sd[i] <- estimate$latents$sd
        plc_vcr <-rbind(plc_vcr, data.frame(ID = rep(fit$ID[i],length(estimate$latents$results$V)),
                             v =estimate$latents$results$V,
                             cr = estimate$latents$CR,
                             estimate$latents$results$post))
    }

    parameters <- data.frame(ID = fit$ID,
                             post_mean_alpha = fit$post_mean_alpha,
                             b = b,
                             sd = sd)

    return(list(fit = fit, parameters = parameters, plc_vcr = plc_vcr))
}
