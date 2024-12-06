## Functions for semi-parametric estimation of the null distribution per sample

generate_fits_given_catalog <- function(catalog,signatures,n_iters=1000,allmut_tolratio=0.003){

  res <- list()

  res$catalog <- as.matrix(catalog)
  res$signatures <- as.matrix(signatures)

  ### Fitting
  res$exposures <- fast_nnls_KLD(res$catalog,res$signatures,n_iters=n_iters)
  res$norm_exposures <- (t(t(res$exposures)/res$metrics$burden))

  res$reconstructed <- res$signatures %*% res$exposures ## residuals
  res$residuals <- res$catalog- res$reconstructed

  res$flexconstr_exposures <- as.matrix(flexconstr_sigfit_multipleSamples(res$signatures,res$catalog,allmut_tolratio = allmut_tolratio))
  res$flexconstr_reconstructed <- res$signatures %*% res$flexconstr_exposures
  res$flexconstr_residuals <- res$catalog - res$flexconstr_reconstructed ## error


  res$metrics$samples <- colnames(res$catalog)
  res$metrics$errorSAD <- colSums(abs(res$flexconstr_residuals))
  res$metrics$residualSAD <- colSums(abs(res$residuals))
  res$metrics$residualSSD <- colSums(res$residuals) ## how much do the residuals vary around 0 ## basically 0

  res$metrics$burden <- colSums(res$catalog)

  res$metrics$norm_errorSAD <- res$metrics$errorSAD/res$metrics$burden
  res$metrics$norm_residualSAD <- res$metrics$residualSAD/res$metrics$burden
  res$metrics$norm_residualSSD <- res$metrics$residualSSD/res$metrics$burden

  ## aggreatate data over breaks by 100bp
  breaks <- seq(50,10000000,by=100)
  midpoints <- breaks+50
  res$metrics$breaks <- midpoints[as.numeric(cut(res$metrics$burden,breaks =breaks))]

  return(res)

}

#### version using andrea's fit because of Numeric Problems in some organs 
generate_fits_given_catalog1 <- function(catalog,signatures,allmut_tolratio=0.003){
  
  res <- list()
  
  res$catalog <- as.matrix(catalog)
  res$signatures <- as.matrix(signatures)
  
  ### Fitting
  res$exposures <- signature.tools.lib::Fit(catalogues = catalog, signatures = signatures, useBootstrap = F, method = "KLD")$exposures
  res$exposures <- res$exposures[,-which(colnames(res$exposures) == "unassigned")]  
  
  res$reconstructed <- res$signatures %*% t(res$exposures) ## residuals
  res$residuals <- res$catalog- res$reconstructed
  
  res$flexconstr_exposures <- as.matrix(flexconstr_sigfit_multipleSamples(res$signatures,res$catalog,allmut_tolratio = allmut_tolratio))
  res$flexconstr_reconstructed <- res$signatures %*% res$flexconstr_exposures
  res$flexconstr_residuals <- res$catalog - res$flexconstr_reconstructed ## error
  
  
  res$metrics$samples <- colnames(res$catalog)
  res$metrics$errorSAD <- colSums(abs(res$flexconstr_residuals))
  res$metrics$residualSAD <- colSums(abs(res$residuals))
  res$metrics$residualSSD <- colSums(res$residuals) ## how much do the residuals vary around 0 ## basically 0
  
  res$metrics$burden <- colSums(res$catalog)
  
  res$metrics$norm_errorSAD <- res$metrics$errorSAD/res$metrics$burden
  res$metrics$norm_residualSAD <- res$metrics$residualSAD/res$metrics$burden
  res$metrics$norm_residualSSD <- res$metrics$residualSSD/res$metrics$burden
  
  res$norm_exposures <- (t(t(res$exposures)/res$metrics$burden))
  
  ## aggreatate data over breaks by 100bp
  breaks <- seq(50,10000000,by=100)
  midpoints <- breaks+50
  res$metrics$breaks <- midpoints[as.numeric(cut(res$metrics$burden,breaks =breaks))]
  
  return(res)
  
}


## subset the fit object by sample, and store useful metrics for downstream calculation
generate_nulls_given_fit <- function(sample,fit_object,n_samples=1000){
  print(sample)
  ## initialize return structures

  res <- list()
  ## store useful per-item metrics
  res$sample <- sample
  res$norm_errorSAD <- fit_object$metrics$norm_errorSAD[res$sample]
  res$norm_residualSAD <- fit_object$metrics$norm_residualSAD[res$sample]
  res$norm_residualSSD <- fit_object$metrics$norm_residualSSD[res$sample]

  res$reconstructed <- fit_object$reconstructed[,res$sample,drop=T]
  res$overall_exposure <- fit_object$metrics$burden[res$sample]

  ## generate associated null samples
  res$null <- rmultinom(n = n_samples,
                        size =res$overall_exposure,
                        prob = res$reconstructed)

  colnames(res$null) <- paste0('Null_',1:ncol(res$null))

  # ##generate some  metrics
  # res$metrics$burden <- colSums(res$realizations) ## the actual exposure post sampling not always perfectly concordant
  # res$metrics$realization_baselineSAD <- colSums(abs(res$X-res$realizations))
  # res$metrics$realization_norm_baselineSAD <- res$metrics$realization_baselineSAD/res$metrics$burden
  # res$metrics$realization_baselineSSD <- colSums((res$X-res$realizations)) ## no abs
  # res$metrics$realization_norm_baselineSSD <- res$metrics$realization_baselineSSD/res$metrics$burden

  return(res)
}

generate_significance_given_null <- function(null_set,signatures,n_iters=1000,allmut_tolratio=0.003){
  print(c(null_set$overall_exposure,ncol(null_set$realizations)))

  res <- list()
  res$sample <- null_set$sample
  res$norm_errorSAD <- null_set$norm_errorSAD
  res$norm_residualSAD <- null_set$norm_residualSAD
  res$norm_residualSSD <- null_set$norm_residualSSD

  ## set the overall exposure
  res$overall_exposure <- null_set$overall_exposure

  ## set the realizations to copy over
  res$null <- as.matrix(null_set$null)
  res$n_samples <- ncol(res$null)



  ## fit the realized catalog (generated by sampling multinomial realizations)
  print('sigfit')

  ## swap in the fast-fit function
  res$null_fit <- fast_nnls_KLD(res$null,signatures,n_iters = n_iters)
  # res$realization_fit <- signature.tools.lib::SignatureFit(cat = res$realizations,
  #                                                      signature_data_matrix = signatures,
  #                                                      method = 'KLD',
  #                                                      verbose = TRUE )

  print("flexconstr")
  res$null_flexconstr_fit <- flexconstr_sigfit_multipleSamples(as.matrix(signatures),
                                                               res$null,
                                                               allmut_tolratio = allmut_tolratio)

  ## reconstruct+ store residuals
  res$null_reconstructed <- as.matrix(signatures) %*% as.matrix(res$null_fit)
  res$null_residuals <- res$null-res$null_reconstructed

  ## flexconstr
  res$null_flexconstr_reconstructed <- as.matrix(signatures) %*% as.matrix(res$null_flexconstr_fit)
  res$null_flexconstr_residuals <- res$null - res$null_flexconstr_reconstructed

  ## calculate emetrics from realizations + residual
  res$metrics$burden <- colSums(res$null)

  ## metrics for residual
  res$metrics$null_residualSAD <- colSums(abs(res$null_residuals))
  res$metrics$null_norm_residualSAD <- res$metrics$null_residualSAD/res$metrics$burden

  res$metrics$null_residualSSD <- colSums(res$null_residuals)
  res$metrics$null_norm_residualSSD <- res$metrics$null_residualSSD/res$metrics$burden

  ## metrics for flexconstr
  res$metrics$null_errorSAD <- colSums(abs(res$null_flexconstr_residuals))
  res$metrics$null_norm_errorSAD <- res$metrics$null_errorSAD/res$metrics$burden

  ## calculate p values

  res$pvals$sample <- res$sample
  res$pvals$norm_residualSAD_normpvalue <- pnorm(res$norm_residualSAD,
                                                 mean(res$metrics$null_norm_residualSAD),
                                                 sd = sd(res$metrics$null_norm_residualSAD),
                                                 lower.tail = F)

  res$pvals$norm_residualSAD_emppvalue <- sum(res$metrics$null_norm_residualSAD>res$norm_residualSAD)/res$n_samples


  res$pvals$norm_errorSAD_normpvalue <- pnorm(res$norm_errorSAD,
                                              mean(res$metrics$null_norm_errorSAD),
                                              sd = sd(res$metrics$null_norm_errorSAD),
                                              lower.tail = F)

  res$pvals$norm_errorSAD_emppvalue <- sum(res$metrics$null_norm_errorSAD>res$norm_errorSAD)/res$n_samples



  return(res)

}





generate_significance_given_null1 <- function(null_set,signatures,n_iters=1000,allmut_tolratio=0.003){
  print(c(null_set$overall_exposure,ncol(null_set$realizations)))
  
  res <- list()
  res$sample <- null_set$sample
  res$norm_errorSAD <- null_set$norm_errorSAD
  res$norm_residualSAD <- null_set$norm_residualSAD
  res$norm_residualSSD <- null_set$norm_residualSSD
  
  ## set the overall exposure
  res$overall_exposure <- null_set$overall_exposure
  
  ## set the realizations to copy over
  res$null <- as.matrix(null_set$null)
  res$n_samples <- ncol(res$null)
  
  
  
  ## fit the realized catalog (generated by sampling multinomial realizations)
  print('sigfit')
  
  ## swap in the fast-fit function
  res$null_fit <- NNLM::nnlm(as.matrix(signatures), 
                             as.matrix(res$null), loss = "mkl", method = "lee", max.iter = n_iters)$coefficients
  
  
  print("flexconstr")
  res$null_flexconstr_fit <- flexconstr_sigfit_multipleSamples(as.matrix(signatures),
                                                               res$null,
                                                               allmut_tolratio = allmut_tolratio)
  
  ## reconstruct+ store residuals
  res$null_reconstructed <- as.matrix(signatures) %*% as.matrix(res$null_fit)
  res$null_residuals <- res$null-res$null_reconstructed
  
  ## flexconstr
  res$null_flexconstr_reconstructed <- as.matrix(signatures) %*% as.matrix(res$null_flexconstr_fit)
  res$null_flexconstr_residuals <- res$null - res$null_flexconstr_reconstructed
  
  ## calculate emetrics from realizations + residual
  res$metrics$burden <- colSums(res$null)
  
  ## metrics for residual
  res$metrics$null_residualSAD <- colSums(abs(res$null_residuals))
  res$metrics$null_norm_residualSAD <- res$metrics$null_residualSAD/res$metrics$burden
  
  res$metrics$null_residualSSD <- colSums(res$null_residuals)
  res$metrics$null_norm_residualSSD <- res$metrics$null_residualSSD/res$metrics$burden
  
  ## metrics for flexconstr
  res$metrics$null_errorSAD <- colSums(abs(res$null_flexconstr_residuals))
  res$metrics$null_norm_errorSAD <- res$metrics$null_errorSAD/res$metrics$burden
  
  ## calculate p values
  
  res$pvals$sample <- res$sample
  res$pvals$norm_residualSAD_normpvalue <- pnorm(res$norm_residualSAD,
                                                 mean(res$metrics$null_norm_residualSAD),
                                                 sd = sd(res$metrics$null_norm_residualSAD),
                                                 lower.tail = F)
  
  res$pvals$norm_residualSAD_emppvalue <- sum(res$metrics$null_norm_residualSAD>res$norm_residualSAD)/res$n_samples
  
  
  res$pvals$norm_errorSAD_normpvalue <- pnorm(res$norm_errorSAD,
                                              mean(res$metrics$null_norm_errorSAD),
                                              sd = sd(res$metrics$null_norm_errorSAD),
                                              lower.tail = F)
  
  res$pvals$norm_errorSAD_emppvalue <- sum(res$metrics$null_norm_errorSAD>res$norm_errorSAD)/res$n_samples
  
  
  
  return(res)
  
}


