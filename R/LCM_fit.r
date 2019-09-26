# LCM_fit ===========================================================================
LCM_fit <- function(data=data,opts=opts) {
  # Fit latent cause model to data.
  # USAGE: results = LCM_fit(data,[opts])
  #
  # INPUTS:
  #   data - [nSubjects x 1] structure containing the following fields:
  #           .CR - [nTrials x 1] conditioned response
  #           .CS - [nTrials x nCues] conditioned stimului
  #           .US - [nTrials x 1] unconditioned response
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

  # number of alpha values to evaluate
  N = 50
  # set alpha (range=0~10, number is N)
  alpha = linspace(0,10,N)
  if (missing(y)) {
    opts <- list()
    return(list(opts=opts))
  }

  for (s in 1:vector) {

  }

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
