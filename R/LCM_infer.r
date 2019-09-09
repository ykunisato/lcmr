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


    # set parameters
    if (missing(opts)) return(opts<-NULL)

    #opts = LCM_opts(opts);
    M = opts$M;
    a = opts$a;
    b = opts$b;



    return(list(opts=opts,M=M,a=a,b=b))
}

opts <- NULL
