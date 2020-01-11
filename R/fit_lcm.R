#' Fit data to latent cause model
#'
#' \code{fit_lcm} fit latent cause model to conditioning data
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom tidyr unnest_wider
#' @importFrom furrr future_map
#' @importFrom future plan
#' @importFrom pracma linspace
#'
#' @param data long format data containing the following variables
#'        (Order and name is exactly the same as following):
#'
#'        ID  Subject ID
#'
#'        CR  Conditioned Response
#'
#'        US  Unconditioned Stimulus
#'
#'        CS  Conditioned Stimului or Context. If using multiple CS, set variables name as CS1,CS2,CS3...
#'
#'        If you want to use LCM-RW, you have to add time variable(stimulus onset, unit is sec) before US.
#' @param model 1 = latent cause model, 2 = latent cause modulated RW model
#' @param opts (optional) structure defining ASL options
#' @param parameter_range (optional)  range of parameter(a_L, a_U, e_L, e_U)
#' @param parallel (optional)  0 = single, 1 = parallel
#' @param estimation_method (optional)  0 = optim or optimize(lcm), 1 = post mean(only latent cause model)
#'
#' @return return the fit, parameters and plc_vcr.
#' fit is fitting results. parameters is parameters estmated including post_mean_alpha(posterior mean alpha) and
#' logBFlog(Bayes factor for the alpha>=0 model relative to the alpha=0 model).
#' plc_vcr is matrix of latent cause posterior and V & CR predicted.
#'
#' @export
#' @examples
#'
#' # results <- fit_lcm(data, model, opts, parameter_range, parallel, estimation_method)
fit_lcm <- function(data, model, opts, parameter_range, parallel, estimation_method){
    # check argument
    if (missing(data)) {
        stop("Please set the data")
    }
    if (missing(model)) {
        stop("Please set the model")
    }
    if (missing(opts)) {
        opts <- list()
    }
    if (missing(parallel)) {
        parallel <- 0
    }
    if (missing(estimation_method)) {
        estimation_method <- 0
    }
    # set parameters range
    def_parameter_range <- list()
    def_parameter_range$a_L <- 0.0000000000000000000000000000001
    def_parameter_range$a_U <- 10
    def_parameter_range$e_L <- 0.0000000000000000000000000000001
    def_parameter_range$e_U <- 1
    if (missing(parameter_range)) {
        parameter_range = def_parameter_range
    } else {
        F <- names(def_parameter_range)
        for (i in 1:length(F)) {
            if (eval(parse(text = paste0("length(parameter_range$", F[i], ")==0")))) {
                eval(parse(text = paste0("parameter_range$", F[i], "=def_parameter_range$", F[i])))
            }
        }
    }
    # fitting
    if(model == 1){
        if(estimation_method==0){
            if(parallel==0){
                fit  <- data %>%
                    group_by(ID) %>%
                    nest() %>%
                    mutate(fit = map(data, ~optimize(compute_negative_loglike,
                                                     interval = c(parameter_range$a_L, parameter_range$a_U),
                                                     data = ., model = model, opts = opts))) %>%
                    unnest_wider(fit) %>%
                    rename(alpha=minimum,nll=objective)
            }else if(parallel==1){
                # set parallel computing
                plan("multisession")
                # fitting
                fit <- data %>%
                    group_by(ID) %>%
                    nest() %>%
                    mutate(fit = future_map(data, ~optimize(compute_negative_loglike,
                                                            interval = c(parameter_range$a_L, parameter_range$a_U),
                                                            data = ., model = model, opts = opts))) %>%
                    unnest_wider(fit) %>%
                    rename(alpha=minimum,nll=objective)
            }
        }else if(estimation_method==1){
            alpha <- linspace(0, 10, 50)
            if(parallel==0){
                fit <- data %>%
                    group_by(ID) %>%
                    nest() %>%
                    mutate(fit = map(data, ~estimate_by_post_mean(data = ., model, opts, alpha))) %>%
                    unnest_wider(fit) %>%
                    rename(alpha = post_mean_alpha)

            }else if(parallel==1){
                # set parallel computing
                plan("multisession")
                # fitting
                fit <- data %>%
                    group_by(ID) %>%
                    nest() %>%
                    mutate(fit = future_map(data, ~estimate_by_post_mean(data = ., model, opts, alpha))) %>%
                    unnest_wider(fit) %>%
                    rename(alpha = post_mean_alpha)
            }
        }
    }else if(model == 2){
        if(parallel==0){
            fit  <- data %>%
                group_by(ID) %>%
                nest() %>%
                mutate(fit = map(data, ~estimate_by_optim(data = ., model, opts, parameter_range))) %>%
                unnest_wider(fit)
        }else if(parallel==1){
            # set parallel computing
            plan("multisession")
            # fitting
            fit  <- data %>%
                group_by(ID) %>%
                nest() %>%
                mutate(fit = future_map(data, ~estimate_by_optim(data = ., model, opts, parameter_range))) %>%
                unnest_wider(fit)
        }
    }
    # extract matrix of latent cause posterior and V & CR predicted
    b <- NULL
    sd <- NULL
    plc_vcr <- NULL

    for (i in 1:length(fit$ID)) {
        if(model == 1){
            if(is.na(fit$alpha[i])){
                b[i] <- NA
                sd[i] <- NA
            }else{
                estimate <- compute_loglike(fit$alpha[i], fit$data[[i]], model, opts)
                b[i] <- estimate$latents$b
                sd[i] <- estimate$latents$sd
                plc_vcr <-rbind(plc_vcr, data.frame(ID = rep(fit$ID[i],length(estimate$latents$results$V)),
                                                    v =estimate$latents$results$V,
                                                    cr = estimate$latents$CR,
                                                    estimate$latents$results$post))
            }
        }else if(model == 2){
            if(is.na(fit$alpha[i]) || is.nan(fit$eta[i])){
                b[i] <- NA
                sd[i] <- NA
            }else{
                estimate <- compute_loglike(c(fit$alpha[i],fit$eta[i]), fit$data[[i]], model, opts)
                b[i] <- estimate$latents$b
                sd[i] <- estimate$latents$sd
                plc_vcr <-rbind(plc_vcr, data.frame(ID = rep(fit$ID[i],length(estimate$latents$results$V)),
                                                    v =estimate$latents$results$V,
                                                    cr = estimate$latents$CR,
                                                    estimate$latents$results$Z))
            }
        }
    }
    if(model==1){
        parameters <- data.frame(ID = fit$ID,
                                 alpha = fit$alpha,
                                 b = b,
                                 sd = sd)
    }else if(model==2){
        parameters <- data.frame(ID = fit$ID,
                                 alpha = fit$alpha,
                                 eta = fit$eta,
                                 b = b,
                                 sd = sd)
    }
    return(list(fit = fit, parameters = parameters, plc_vcr = plc_vcr))
}





#' Estimate alpha of LCM by post mean
#'
#' \code{estimate_by_post_mean} is function to estimate alpha and logBF
#' @importFrom tidyr nest
#'
#' @param alpha vector of alpha
#' @param data data from fit_asl or fit_asl_parallel
#' @param model 1 = latent cause model, 2 = latent cause modulated RW model
#' @param opts (optional) structure defining ASL options
#'
#' @return df data frame containing post_mean_alpha(posterior mean alpha),
#' logBF(log Bayes factor for the alpha>=0 model relative to the alpha=0 model),
#' loglikelihood and probability each alpha.
estimate_by_post_mean <- function(data, model, opts, alpha) {
    lik <- numeric(length(alpha))
    for (i in 1:length(alpha)) {
        results <- compute_loglike(param=alpha[i], data, model, opts)
        lik[i] <- results$lik
    }
    L <- log(sum(exp(lik)))
    P <- exp(lik - L)
    post_mean_alpha <- alpha %*% P
    logBF <- L - log(length(alpha)) - lik[1]
    df <- data.frame(post_mean_alpha,
                     logBF,
                     data.frame(alpha,lik,P))%>%
        nest(prob_alpha=c(alpha,lik,P))
    return(df)
}




#' Estimate parameters of LCM by optim
#'
#' \code{estimate_by_optim} is function to estimate lcm parameter using optim
#' @importFrom tidyr nest
#'
#' @param data data from fit_asl or fit_asl_parallel
#' @param model 1 = latent cause model, 2 = latent cause modulated RW model
#' @param opts (optional) structure defining ASL options
#' @param parameter_range (optional) range of parameter
#'
#' @return estimated parameters, negative log likelihood
estimate_by_optim <- function(data, model, opts, parameter_range) {
    smallest_nll <- Inf
    param <- NULL
    cat("start estimation using optim... \n")
    for (i in 1:50) {
        #compute_negative_loglike(param, data, model, opts)
        init_param <- c(runif(1,parameter_range$a_L,parameter_range$a_U),
                        runif(1,parameter_range$e_L,parameter_range$e_U))
        tryCatch({
        results <- optim(init_param,
                         compute_negative_loglike,
                         data = data, model = model, opts = opts,
                         method = "L-BFGS-B",
                         lower = c(parameter_range$a_L,parameter_range$e_L),
                         upper = c(parameter_range$a_U,parameter_range$e_U))

        cat(i," ","negative log likelihood: ",results$value)
        cat("  parameter: ",results$par,"\n")
        if(results$value < smallest_nll){
            smallest_nll <- results$value
            param <- results$par
        }
        }, error = function(e) {cat(i," Error in estimation using optim")})

        if(i==20 && length(param)!=0){
            break
        }else if(i==30 && length(param)!=0){
            break
        }else if(i==40 && length(param)!=0){
            break
        }
    }

    if(length(param)==0){
        if(model==1){
            param[1] <- NA
        }else if(model==2){
            param[1] <- NA
            param[2] <- NA
        }
    }

    if(model==1){
        return(list(alpha = param[1], nll = smallest_nll))
    }else if(model==2){
        return(list(alpha = param[1], eta = param[2], nll = smallest_nll))
    }

}
