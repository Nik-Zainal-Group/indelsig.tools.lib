## Functions for hyperutator filtering as part of clustering / stratification

## calculate likelihood for a mixture model of normals
dnormalmix <- function(x,mixture,log=FALSE) {
  lambda <- mixture$lambda ## get the % partitoins
  k <- length(lambda)
  # Calculate share of likelihood for all data for one component
  like.component <- function(x,component) {
    lambda[component]*dnorm(x,mean=mixture$mu[component],
                            sd=mixture$sigma[component])
  }
  # Create array with likelihood shares from all components over all data
  likes <- sapply(1:k,like.component,x=x)
  #print(likes)
  # Add up contributions from components
  d <- rowSums(likes)
  if (log) {
    d <- log(d)
  }
  return(d)
}

## calculate normal mix likelihood
loglike.normalmix <- function(x,mixture) {
  loglike <- dnormalmix(x,mixture,log=TRUE)
  return(sum(loglike))
}

BIC <- function(loglik,N,k){
  return(-2*loglik+log(N)*k)
}

loglik.normalMix.boot <- function(data,mixture){
  N <- length(data)
  K <- nrow(mixture$lambda)
  B <- ncol(mixture$lambda)

  ll <- array(dim = c(N,K,B))

  for(k in 1:B){
    for(j in 1:K){
      ll[,j,k] <- mixture$lambda[j,k]*dnorm(data,mixture$mu[j,k],mixture$sigma[j,k])
    }
  }

  ret <- colSums(log10(apply(ll,3,rowSums)))
  return(ret)
}



fit_single_norm <- function(data){
  n <- length(data)
  mu<-mean(data) # MLE of mean
  sigma <- sd(data)*sqrt((n-1)/n) # MLE of standard deviation
  ret <- list(
    mu=mu,
    sigma=sigma,
    loglik=sum(dnorm(data,mu,sigma,log=TRUE))
  )

  ret$BIC <- BIC(ret$loglik,length(data),2)

  return(ret)

}

fit_norm_2mix <- function(data,mu){

  mixture <- suppressMessages(normalmixEM(data,k=2,maxit=1000,epsilon=1e-2,mu = mu,maxrestarts = 20,))
  ret <- list(mixture=mixture,
              loglik=loglike.normalmix(data,mixture=mixture))

  ret$BIC <-BIC(ret$loglik,length(data),4)

  return(ret)
}


fit_norm_2mix_boot <- function(data,mixture,B=100){

  boot <- suppressMessages(boot.se(mixture,B = B))


  ret <- list(mixture=mixture,
              loglik=loglik.normalMix.boot(data,mixture=boot))


  ret$BIC <-BIC(ret$loglik,length(data),4)

  return(ret)
}


outliers_IQR <- function(data){
  qs <- quantile(data)
  iqr <- IQR(data)
  upper_lim <- qs['75%']+1.5*iqr
  outliers <- names(data)[which(data >=upper_lim)]
  return(outliers)

}

outliers_IQR_low <- function(data){
  qs <- quantile(data)
  iqr <- IQR(data)
  lower_lim <- qs['25%']-1.5*iqr
  outliers <- names(data)[which(data <= lower_lim)]
  return(outliers)
}
