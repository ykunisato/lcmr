# LCM_fit ===========================================================================
LCM_fit <- function(data=data,n_cs=n_cs,opts=opts) {
  # Fit latent cause model to data.
  # USAGE: results = LCM_fit(data,[opts])
  #
  # INPUTS:
  #   data - long format data containing the following variables:
  #         ID: Subject ID
  #         CR: Conditioned Response
  #         US: Unconditioned Stimulus
  #         CS: Conditioned Stimului(CS1,CS2,CS3...)
  #   n_cs - number of CS
  #   opts (optional) - structure defining LCM options (see LCM_opts.m)
  #
  # OUTPUTS:
  #   results - [nSubjects x 1] structure containing the following fields:
  #               .alpha - concentration parameter values
  #               .P - posterior probability distribution over alpha
  #               .lik - log-likelihood for each alpha value
  #               .latents - latent variables for each alpha value (see LCM_infer)
  #               .logBF - log Bayes factor for the alpha>=0 model relative to the alpha=0 model
  #  This function is developed by Sam Gershman as Matlab code(07/2016) and implemented as R code by Yoshiko Kunisato(09/2019)

  # argument
  if (missing(opts)) {
    opts <- list()
  }

  if (missing(n_cs)) {
    warning("Please set the n_cs(number of CS)")
  }

  # number of alpha values to evaluate
  N <- 50
  # set alpha (range=0~10, number is N)
  alpha <- linspace(0,10,N)

  N_participants <- length(unique(data$ID))
  ID_list <- unique(data$ID)
  for (s in 1:N_participants) {
    cat('Participants',s, "\n")
    data_subset <- subset(data, ID==ID_list[s])
    for (i in 1:N) {
      results <- LCM_lik(alpha[i],data_subset,n_cs,opts)

    }
    L <- logsumexp(results(s).lik)
    results(s).alpha <- alpha
    results(s).P <- exp(results(s).lik-L)
    results(s).alpha <- alpha*results(s).P
    # aplha = posterior mean alpha
    results(s).logBF <- L - log(N) - results(s).lik(1)
    results(s).lik(i,1) <-
    results(s).latents(i)<-
  }

  # Matlab
  #for s = 1:length(data)
  #  disp(['Subject ',num2str(s)]);
  #  for i = 1:length(alpha)
  #    [results(s).lik(i,1), results(s).latents(i)] = LCM_lik(alpha(i),data(s),opts);
  #  end
  #  L = logsumexp(results(s).lik);
  #  results(s).alpha = alpha;
  #  results(s).P = exp(results(s).lik-L);
  #  results(s).alpha = alpha*results(s).P;
  #  % aplha = posterior mean alpha
  #  results(s).logBF = L - log(N) - results(s).lik(1);
  #end

    return(list(results=results))
}
