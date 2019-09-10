# LCM_opts ===========================================================================
LCM_opts <- function(opts=opts) {
    # Set options.
    #
    # USAGE: opts = LCM_opts([opts])
    #
    # INPUTS:
    #   opts (optional) - options structure with a subset of fields
    #       specified. All missing or empty fields will be set to defaults. If
    #       opts = [], then the entire structure is set to defaults.
    #
    # OUTPUTS:
    #   opts - fully specified options structure
    #
    # DEFAULTS:
    #   opts.M = 100        (number of particles)
    #   opts.a = 1          (hyperparameter of beta prior: pseudo-count for feature presence)
    #   opts.b = 1          (hyperparameter of beta prior: pseudo-count for feature absence)
    #   opts.alpha = 0      (concentration parameter for Chinese restaurant process prior)
    #   opts.stickiness = 0 (stickiness parameer for Chinese restaurant process prior)
    #   opts.K = 10         (maximum number of latent causes)
    #
    #  This function is developed by Sam Gershman as Matlab code(07/2016) and implemented as R code by Yoshiko Kunisato(08/2019)

    def_opts   <- list()
    def_opts$M <- 100
    def_opts$a <- 1
    def_opts$b <- 1
    def_opts$alpha <-  0
    def_opts$stickiness <- 0
    def_opts$K <- 10

    if (nargs() < 1 || length(opts)==0){
        opts = def_opts;
    }else{
        F <- names(def_opts)
        for(i in 1:length(F)){
            if(eval(parse(text =paste0("length(opts$",F[i],")==0") ))){
                eval(parse(text = paste0("opts$",F[i],"=def_opts$",F[i])))
            }
        }
    }

    # make sure parameters aren't negative
    opts$a = max(opts$a,0);
    opts$b = max(opts$b,0);
    opts$alpha = max(opts$alpha,0);
    opts$stickiness = max(opts$stickiness,0);

    return(opts)
}
