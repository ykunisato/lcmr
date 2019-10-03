#' Set option
#'
#' \code{LCM_opts} set option of latent cause model.
#'
#' @param opts (optional)options structure with a subset of fields specified.
#'       All missing or empty fields will be set to default values.
#'
#'   Default values:
#'
#'   opts$M <- 100       (number of particles)
#'
#'   opts$a <- 1          (hyperparameter of beta prior: pseudo-count for feature presence)
#'
#'   opts$b <- 1          (hyperparameter of beta prior: pseudo-count for feature absence)
#'
#'   opts$alpha <- 0      (concentration parameter for Chinese restaurant process prior)
#'
#'   opts$stickiness <- 0 (stickiness parameer for Chinese restaurant process prior)
#'
#'   opts$K <- 10         (maximum number of latent causes)
#'
#' @return opts: fully specified options structure
#' @export
#'
#' @examples
#' # opts <- list()
#' # opts$M <- 100
#' # opts <- LCM_opts(opts)

LCM_opts <- function(opts) {
    def_opts <- list()
    def_opts$M <- 1000
    def_opts$a <- 1
    def_opts$b <- 1
    def_opts$c_alpha <- 0  # change parameter name form original
    def_opts$stickiness <- 0
    def_opts$K <- 10
    
    if (length(opts) == 0) {
        opts = def_opts
    } else {
        F <- names(def_opts)
        for (i in 1:length(F)) {
            if (eval(parse(text = paste0("length(opts$", F[i], ")==0")))) {
                eval(parse(text = paste0("opts$", F[i], "=def_opts$", F[i])))
            }
        }
    }
    # make sure parameters has positive value
    opts$a = max(opts$a, 0)
    opts$b = max(opts$b, 0)
    opts$c_alpha = max(opts$c_alpha, 0)
    opts$stickiness = max(opts$stickiness, 0)
    return(opts)
}
