
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lcmr

<!-- badges: start -->

<!-- badges: end -->

The goal of lcmr is to fit latent cause model to conditioning data.

lcmr is an R package of [LCM](https://github.com/sjgershm/LCM) made
by[Sam Gershman](http://gershmanlab.webfactional.com/people/sam.html).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

(Now this is not working because this repository is private)

    # install.packages("devtools")
    devtools::install_github("ykunisato/lcmr")

## How to use the lcmr

### Prepare the data set

The data must be prepared in the long format containing the following
variables (The order and name of variables must be prepared in exactly
the same as following):

  - ID : Put the participant ID in first column
  - CR : Put the Conditioned Response in the second column.
  - US : Put the Unconditioned Stimulus in the third column.
  - CS : Put the Conditioned Stimuli in the third and subsequent column.
    If using multiple CS, set variables name as CS1, CS2, CS3…

### List of functions

  - fit\_lcm( ): Fit latent cause model to conditioning data
  - fit\_lcm\_parallel( ): Fit latent cause model to conditioning data
    using parallel processing
  - infer\_latent\_cause( ): Conduct particle filtering or local maximum
    a posterior inference for latent cause model
  - estimate\_lcm\_a() Estimate alpha and logBF
  - compute\_lcm\_loglik( ): Compute log likelihood of data under the
    latent cause model
  - set\_lcm\_opts( ): Set option of latent cause model

### How to use the LCM\_fit

You specify the following argument:

  - data: Use the data prepared above

  - n\_cs: number of CS

  - opts: (optional) structure defining LCM options (see LCM\_opts)

If you want to define LCM options, you should set it as list format as
following.

    # For example, set the number of particles to 1000 (default values is 100).
    opts <- list(M=1000)

You can make simulation data using the following code.

    # Set number of participants
    number_of_perticiapnts <- 4
    
    # Set hypothetical stimuli
    # Conditioning trail: 18 trial
    # Extinction trial: 12 trial
    # CS: Two CSs(CS1=CS+, CS2=CS-)
    US  <- c(1,0,0,1,1,0,1,0,1,1,0,0,1,0,0,1,1,0, 0,0,0,0,0,0,0,0,0,0,0,0)
    CS1 <- c(1,0,0,1,1,0,1,0,1,1,0,0,1,0,0,1,1,0, 1,1,0,1,0,0,1,0,1,1,0,0)
    CS2 <- c(0,1,1,0,0,1,0,1,0,0,1,1,0,1,1,0,0,1, 0,0,1,0,1,1,0,1,0,0,1,1)
    
    # Make simulation dataset using infer_latent_cause
    participants_alpha <- runif(number_of_perticiapnts, 0, 10)
    data <- NULL
    for (i in 1:number_of_perticiapnts) {
      opts <- list(alpha=participants_alpha[i])
      sim_data <- infer_latent_cause(cbind(US,CS1,CS2),opts)
      sim_df <- data.frame(ID=rep(i,length(US)),CR=sim_data$V,US=US,CS1=CS1,CS2=CS2)
      data <- rbind(data,sim_df)
    }

You can use the LCM\_fit to estimate the post\_mean\_alpha (posterior
mean alpha) and logBF (log Bayes factor for the alpha\>=0 model relative
to the alpha=0 model).

    # If number of CS is 2, set as follows.
    n_cs <- 2
    results <- fit_lcm(data,n_cs,opts)

If you want to use parallel processing, you can use LCM\_pfit().
LCM\_pfit() use the future\_map function from furrr packages for
parallel processing.

    results <- fit_lcm_parallel(data,n_cs,opts)

## Bugs and question

Please report on this repository’s
[issues](https://github.com/ykunisato/lcmr/issues)
