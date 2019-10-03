#' Function to estimate alpha and logBF
#'
#' \code{LCM_alpha} is function to estimate alpha and logBF
#'
#' @param data data from LCM_pfit
#' @param n_cs number of CS
#' @param opts (optional) structure defining LCM options (see LCM_opts)
#' @param alpha vector of alpha
#' @return df data frame containing post_mean_alpha(posterior mean alpha) and
#' logBF(: )log Bayes factor for the alpha>=0 model relative to the alpha=0 model)
LCM_alpha <- function(data, n_cs, opts, alpha) {
    lik <- numeric(length(alpha))
    for (i in 1:length(alpha)) {
        results <- LCM_lik(alpha[i], data, n_cs, opts)
        lik[i] <- results$lik
    }
    L <- log(sum(exp(lik)))
    P <- exp(lik - L)
    post_mean_alpha <- alpha %*% P
    logBF <- L - log(length(alpha)) - lik[1]
    df <- data.frame(post_mean_alpha, logBF)
    return(df)
}
