#' Particle filtering or local maximum a posteriori inference
#'
#' \code{infer_latent_cause} conduct Particle filtering or local maximum a posteriori inference
#' for latent cause model of associative learning
#'
#' @importFrom pracma histc
#'
#' @param X matrix of stimulus inputs consisting of the number of trial rows and
#' the number of stimulus features columns. The first feature (column 1) is the US,
#' and the rest of the features (column 2 through D) are CSs.
#' @param opts (optional)structure containing various options (see LCM_opts).
#' Missing fields are set to defaults. If opts.M = 1, then the model computes a local MAP estimate.
#' @return opts: options used in inference
#' @return V: US prediction each trial
#' @return post: matrix of latent cause posterior consisting of the number of trial rows and
#' the probability of latent cause k being active on trial t, after observing the all the features.
#' K (the number of latent causes) is determined adaptively by the model.
#' @export
#' @examples
#' # results <- infer_latent_cause(X,opts)

infer_latent_cause <- function(X, opts) {
    # set parameters
    results <- list()
    if (nargs() < 2) {
        opts <- NULL
    }
    opts <- LCM_opts(opts)
    M <- opts$M
    a <- opts$a
    b <- opts$b
    results$opts <- opts

    # initialization
    if (opts$c_alpha == 0) {
        K <- 1
    } else {
        K <- opts$K
    }

    post <- matrix(0, 1, K)
    post[1] <- 1
    # posterior probability of state(M=number of particles, K=number of state)
    post0 <- matrix(0, M, K)
    post0[, 1] <- 1
    T <- nrow(X)
    D <- ncol(X)
    # feature-cause co-occurence counts(particle*state*stim) stimuli On
    N <- array(0, dim = c(M, K, D))
    # feature-cause co-occurence counts(particle*state*stim) stimuli off
    B <- array(0, dim = c(M, K, D))
    # cause counts(particle*state)
    Nk <- matrix(0, M, K)
    # cause assignments (trial*state，value of first row is 1)
    results$post <- cbind(matrix(1, T, 1), matrix(0, T, K - 1))
    # US predictions(trials)
    results$V <- matrix(0, T, 1)
    # number of particles
    z <- matrix(1, M, 1)

    # loop over trials
    for (t in 1:T) {
        # calculate likelihood Set Mkd in stimulus on (particles*state*stim)
        Mkd <- N
        # if simlus is not presented, insert Mkd in simulus off
        Mkd[, , X[t, ] == 0] <- B[, , X[t, ] == 0]
        # calculate likelihood using supple equation 6
        numerator_lik <- Mkd + a
        denominator_lik <- Nk + a + b
        lik <- N
        for (d in 1:D) {
            lik[, , d] <- numerator_lik[, , d]/denominator_lik
        }
        # only update posterior if concentration parameter is non-zero
        if (opts$c_alpha > 0) {
            # calculate CRP prior
            prior <- Nk
            for (m in 1:M) {
                # add stickiness(if stickiness is higher than 0, number of state leadns 1)
                prior[m, z[m]] <- prior[m, z[m]] + opts$stickiness
                # probability of a new latent cause(Insert alpha in non active state)
                prior[m, which(prior[m, ] == 0)[1]] <- opts$c_alpha
            }
            # posterior conditional on CS only element-wise product of prior and likelihood of CS
            # using supple 2nd term of equation 11
            num_add_cs <- D - 2  #if multiple CS are used, additional number of CS(use two CS, num_add_cs is 1)
            if (num_add_cs == 0) {
                prod_like_cs <- lik[, , 2]
            } else {
                prod_like_cs <- lik[, , 2]
                for (d in 1:num_add_cs) {
                  prod_like_cs <- prod_like_cs * lik[, , 2 + d]
                }
            }
            post <- prior * drop(prod_like_cs)
            # devide posterior rowsum of posterior(is mean prob of CS)
            post0 <- post/rowSums(post)

            # posterior conditional on CS and US element-wise product of posterior of CS and
            # likelihood of US using supple 1st term of equation 11
            post <- post * drop(lik[, , 1])
            # posterior of US is devided by row sum of posteriro of US（probability of US）
            post <- post/rowSums(post)
            # marginalize over particles
            post <- colMeans(post)
        }
        # output of results
        results$post[t, ] = post
        # posterior predictive mean for US likelihoof of US
        pUS = drop(N[, , 1] + a)/(Nk + a + b)
        # product of posteriro of CS and likelihood of US is devided by number of particles
        results$V[t, 1] = t(as.vector(post0)) %*% as.vector(pUS)/M

        # sample new particles
        x1 <- X[t, ] == 1
        x0 <- X[t, ] == 0
        if (M == 1) {
            # maximum a posteriori
            z = which.max(post)
        } else {
            # multinomial sample
            z <- histc(runif(M, 0, 1), c(0, cumsum(post)))$bin
        }
        Nk[, z] <- Nk[, z] + 1
        N[, z, x1] <- N[, z, x1] + 1
        B[, z, x0] <- B[, z, x0] + 1
    }
    # remove unused state
    results$post <- results$post[, colMeans(results$post) != 0]
    return(list(opts = opts, V = results$V, post = results$post))
}
