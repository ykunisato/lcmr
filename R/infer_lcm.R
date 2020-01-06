#' Latent cause model using particle filtering or local maximum a posteriori inference
#'
#' \code{infer_lcm} conduct Particle filtering or local maximum a posteriori inference
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
#' # results <- infer_lcm(X,opts)

infer_lcm <- function(X, opts) {
    # set default options
    def_opts <- list()
    def_opts$M <- 100
    def_opts$a <- 1
    def_opts$b <- 1
    def_opts$c_alpha <- 0
    def_opts$stickiness <- 0
    def_opts$K <- 10
    # set parameters
    results <- list()
    if (nargs() < 2) {
        opts = def_opts
    } else {
        F <- names(def_opts)
        for (i in 1:length(F)) {
            if (eval(parse(text = paste0("length(opts$", F[i], ")==0")))) {
                eval(parse(text = paste0("opts$", F[i], "=def_opts$", F[i])))
            }
        }
    }

    M <- opts$M
    a <- opts$a
    b <- opts$b
    K <- opts$K
    results$opts <- opts

    # initialization
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
            # divide posterior rowsum of posterior(is mean prob of CS)
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
        for (m in 1:M) {
            Nk[m, z[m]] <- Nk[m, z[m]] + 1
            N[m, z[m], x1] <- N[m, z[m], x1] + 1
            B[m, z[m], x0] <- B[m, z[m], x0] + 1
        }
    }
    # remove unused state
    # results$post <- results$post[, colMeans(results$post) != 0]
    return(list(opts = opts, V = results$V, post = results$post))
}





#' Latent cause modulated Rescorla-Wagner model
#'
#' \code{infer_lcm_rw} conduct local maximum a posteriori inference
#' for associative and structuallearning
#'
#' @importFrom pracma histc
#' @importFrom pracma repmat
#'
#' @param X matrix cotaining time(stimulus onset, unit is sec), US, CS.
#' @param opts (optional)list containing various options.
#' If you don't want to use  a nonlinear sigmoidal transformation, you set opts$nst = 0.
#' @return V: vector of conditioned response on each trial
#' @return Zp: latent cause posterior before observing US(Trial*K)
#' @return Z: latent cause posterior(Trial*K)
#' @export
#' @examples
#' # results <- infer_lcm_rw(X, opts)
#'
infer_lcm_rw <- function(X, opts){
    # set options
    def_opts <- list()
    def_opts$c_alpha <- 0.1
    def_opts$g <- 1
    def_opts$psi <- 0
    def_opts$eta <- 0.2
    def_opts$maxIter <- 3
    def_opts$w0 <- 0
    def_opts$sr <- 0.4
    def_opts$sx <- 1
    def_opts$theta <- 0.03
    def_opts$lambda <- 0.005
    def_opts$K <- 15
    def_opts$nst <- 0

    if (nargs() < 2) {
        opts <- NULL
    }
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

    # Initialization
    zp_save <- NULL
    w_save <- NULL
    p_save <- NULL
    w_before_r <- NULL
    post_before_r <- NULL
    X <- matrix(as.matrix(X), nrow(X), ncol(X))
    T <- nrow(X)
    time <- X[,1]      #Time
    r <- X[,2]         #us
    X <- X[,3:ncol(X)] # cues
    D <- ncol(X)
    if(length(opts$c_alpha)==1){
        opts$c_alpha <-opts$c_alpha*matrix(1,T,1)
    }
    if(length(opts$eta)==1){
        opts$eta <-opts$eta*matrix(1,T,1)
    }
    psi <- opts$psi*matrix(1,T,1)
    Z <- matrix(0,T,opts$K)
    V <- matrix(0,T,1)
    W <- matrix(0,D,opts$K) + opts$w0

    # construct distance matrix
    Dist <- matrix(0,T,T)
    for (i in 1:T) {
        for (j in 1:T) {
            Dist[i,j] = abs(time[i]-time[j])
        }
    }
    S <- Dist^(-opts$g)

    ## Run inference
    for (t in 1:T) {
        # determine how many EM iterations to perform based on ITI
        if(t == T){
            nIter <- 1
        }else{
            nIter <- min(opts$maxIter,round(Dist[t,t+1]))
        }

        ## calculate (unnormalized) posterior, not including reward
        #cluster counts
        if(t == 1){
            N <- matrix(0,1,opts$K)
        }else if(t == 2){
            N <- Z[1,]
        }else{
            N <- colSums(Z[1:t-1,])
        }

        if(opts$c_alpha[t]==0){
            prior <- matrix(0,1,opts$K)
            #always use cause1
            prior[1] <- 1
        }else{
            # ddCRP prior
            if(t == 1){
                prior <- matrix(0,1,opts$K)
            }else{
                prior <- S[1:t-1,t]%*%Z[1:t-1,]
            }
            # probability of new cluster
            prior[,which(prior == 0)][1] <- opts$c_alpha[t]
        }
        # normalize prior
        L <- prior/sum(prior)
        # [D x K] matrix of feature sums
        if(t == 1){
            xsum <- matrix(0,D,1)%*%matrix(0,1,opts$K)
        }else{
            xsum <- matrix(X[1:t-1,],D,t-1)%*%Z[1:t-1,]
        }
        nu <- opts$sx/(N+opts$sx) + opts$sx
        for (d in 1:D) {
            xhat <-  xsum[d,]/(N+opts$sx)
            L <-  L*dnorm(X[t,d],xhat,sqrt(nu))  # likelihood
        }
        # reward prediction, before feedback
        post <- L/sum(L)
        V[t] <- (X[t,]%*%W)%*%t(post)
        w_before_r  <- rbind(w_before_r, W)
        post_before_r  <- rbind(post_before_r, post)
        if(opts$nst==1){
            V[t] <- 1-pnorm(opts$theta,V[t],opts$lambda);
        }
        # loop over EM iterations
        for (iter in 1:nIter){
            V_afterUS <- X[t,]%*%W                               # reward prediction
            post <- L*dnorm(r[t],V_afterUS,sqrt(opts$sr))   # unnormalized posterior with reward
            post <- post/sum(post)
            rpe <- repmat((r[t]-V_afterUS)*post,D,1)           # reward prediction error
            x <- repmat(matrix(X[t,]),1,opts$K)
            W <- W + opts$eta[t]*x*rpe            # weight update
            if (psi[t]>0){
                W <- W*(1-repmat(post,D,1))*psi[t]
            }
            post_save <- data.frame(t = rep(t,nrow(post)),iter = rep(iter,nrow(post)), post)
            p_save <- rbind(p_save,post_save)
            W2_save <- data.frame(t = rep(t,nrow(W)),iter = rep(iter,nrow(W)), W)
            w_save <- rbind(w_save,W2_save)
        }
        # save zp
        zp_save <- rbind(zp_save,post)

        # cluster assignment
        k <- which.max(post)                 # maximum a posteriori cluster assignment
        Z[t,k] <- 1
    }
    return(list(opts = opts,
                Dist = Dist,
                V = V,
                Z = Z,
                S = S,
                Zp = zp_save,
                W = w_save,
                P = p_save,
                w = w_before_r,
                p = post_before_r))
}
