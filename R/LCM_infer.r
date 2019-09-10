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
    if(opts$alpha==0){
        K <- 1
    }else{
        K <- opts$K
    }

    post <-matrix(0, 1, K)
    post[1] <- 1
    # posterior probability of state(M=number of particles, K=number of state)
    post0 <- matrix(0, M, K)
    post0[,1] <- 1


    #[T, D] = size(X);
    #N = zeros(M,K,D);                    % feature-cause co-occurence counts(粒子*状態*刺激)
    #B = zeros(M,K,D);                    % feature-cause co-occurence counts(粒子*状態*刺激)
    #Nk = zeros(M,K);                            % cause counts(粒子*状態)
    #results.post = [ones(T,1) zeros(T,K-1)];    % cause assignments (試行数*状態数，1列目は1)
    #results.V = zeros(T,1);                     % US predictions(試行数)
    #z = ones(M,1);                              % 粒子の数

    #opts = LCM_opts(opts);
    M = opts$M;
    a = opts$a;
    b = opts$b;



    return(list(opts=opts,M=M,a=a,b=b))
}

opts <- NULL
