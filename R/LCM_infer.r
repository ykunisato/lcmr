# LCM_infer ===========================================================================
LCM_infer <- function(X, opts) {
    # Particle filtering or local maximum a posteriori inference for latent cause model of associative learning.
    #
    # USAGE: results = LCM(X,[opts])
    #
    # INPUTS:
    #   X - [T x D] stimulus inputs, where T is the number of timepoints and D is the number of stimulus features.
    #         The first feature (column 1) is the US, and the rest of the features (column 2 through D) are CSs.
    #   opts (optional) - structure containing various options (see LCM_opts). Missing fields are set to defaults.
    #         If opts.M = 1, then the model computes a local MAP estimate.
    #  OUTPUTS:
    #   opts - options (missing fields set to defaults)
    #   V - [T x 1] US prediction
    #   post - [T x K] latent cause posterior, where post(t,k) is the probability of latent cause k being active on trial t,
    #         after observing the all the features. K (the number of latent causes) is determined adaptively by the model.
    #
    #  This function is developed by Sam Gershman as Matlab code(07/2016) and implemented as R code by Yoshiko Kunisato(08/2019)

    # list of results
    results <- list()

    # set parameters
    if (nargs() < 2){opts<-NULL}
    opts <- LCM_opts(opts)
    M <- opts$M
    a <- opts$a
    b <- opts$b
    results$opts <- opts

    # initialization
    if(opts$c_alpha==0){
        K <- 1
    }else{
        K <- opts$K
    }

    post <-matrix(0, 1, K)
    post[1] <- 1
    # posterior probability of state(M=number of particles, K=number of state)
    post0 <- matrix(0, M, K)
    post0[,1] <- 1
    T <- nrow(X)
    D <- ncol(X)
    N <- array(0,dim=c(M,K,D))  #feature-cause co-occurence counts(particle*state*stim)
    B <- array(0,dim=c(M,K,D))  #feature-cause co-occurence counts(particle*state*stim)
    Nk <- matrix(0, M, K)       #cause counts(particle*state)
    results$post <- cbind(matrix(1,T,1),matrix(0,T,K-1))  #cause assignments (trial*state，value of first row is 1)
    results$V <- matrix(0,T,1)  # US predictions(trials)
    z <- matrix(1,M,1)          #number of particles

    # loop over trials
    for (t in 1:T) {
        #### calculate likelihood
        # Set Mkd in stimulus on (particles*state*stim)
        Mkd <- N
        # if simlus is not presented, insert Mkd in simulus off
        Mkd[,,X[t,]==0] <- B[,,X[t,]==0]
        # calculate likelihood using supple equation 6
        numerator_lik <- Mkd+a
        denominator_lik <- Nk+a+b
        lik <- N
        for (d in 1:D){
            lik[,,d] <- numerator_lik[,,d]/denominator_lik
        }

        # only update posterior if concentration parameter is non-zero
        if(opts$alpha > 0){
            # calculate CRP prior
            prior <- Nk
            for (m in 1:M){
                #add stickiness(if stickiness is higher than 0, number of state leadns 1)
                prior[m,z[m]] <- prior[m,z[m]] + opts$stickiness
                #probability of a new latent cause(Insert alpha in non active state)
                prior[m,which(prior[m,]==0)] <- opts$c_alpha
            }
            ########################
            # posterior conditional on CS only
            # 事前分布とCSの尤度の積　supple 2nd term of equation 11
            post <- prior.*squeeze(prod(lik[,,2:D],3))
            # 事後分布を，事後分布の行の和で割る（CSの確率になる）
            post0 <- bsxfun(@rdivide,post,sum(post,2))

            # posterior conditional on CS and US
            # CSの事後分布とCSとUSの尤度の積（式11の第1項)
            post <- post.*squeeze(lik[,,1])
            # 事後分布を，事後分布の行の和で割る（USの確率になる）
            post <- bsxfun(@rdivide,post,sum(post,2))
            # marginalize over particles(用意した粒子から確率を計算)
            post <- mean(post,1);
        }
        # output of results
        results.post[t,:] = post
    }

    #return(list(opts=opts,M=M,a=a,b=b))
}


# Matlab
# % posterior predictive mean for US
# pUS = squeeze(N(:,:,1)+a)./(Nk+a+b);    %USの尤度
# results.V(t,1) = post0(:)'*pUS(:)./M;   %CSの元での状態の事後分布とUSの尤度を粒子で割る
#
#         % sample new particles
#         x1 = X(t,:)==1; x0 = X(t,:)==0;
#         if M==1
#             [~,z] = max(post);                         % maximum a posteriori
#         else
#             [~,z] = histc(rand(1,M),[0 cumsum(post)]); % multinomial sample
#         end
#         Nk(:,z) = Nk(:,z) + 1;
#         N(:,z,x1) = N(:,z,x1) + 1;
#         B(:,z,x0) = B(:,z,x0) + 1;
#
#     end
#
#     % remove unused particles　これなんだろう。これでK=10じゃなくなっているな
#     ix = mean(results.post)==0;
#     results.post(:,ix) = [];
