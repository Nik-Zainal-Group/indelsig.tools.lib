## sig fitting functions lifted from Andrea's codebase
flexconstr_sigfit <- function(P,
                              m,
                              mut_tol=0,
                              allmut_tolratio=0.01){
  G <- -P
  H <- -m
  # test to flexibilise the constraints on the residual part
  allmut <-sum(m)
  # H <- -1.5*m #does not change the problem with zeros ...  main problem with the fit
  # H <- -1.2*m-0.01*allmut # does not improve the fitt
  # H <- -m-0.01*allmut
  # H[H==0]<- -0.1*allmut
  H<- -m-mut_tol*m-allmut_tolratio*allmut

  # G<-NULL
  # H<-NULL

  # add positivity requirement for exposures e >=0

  if (ncol(P)>1){
    coef_nn_e<-diag(1,ncol(P))
    rside_nn_e <- rep(0,ncol(P))
    # summarising both constraints into matrices G and H
    G <- rbind(G,coef_nn_e)
    H <- c(H,rside_nn_e)
  }else{
    G <- c(G,1)
    H <- c(H,0)
  }

  # solving least squares on (Pe-m)^2  with the constraints (-Pe>=-m-mut_tol-allmut_tolratio*) and (e >=0)  => Gx>=h

  e <- limSolve::lsei(A = P, B= m, G = G,
                      H = H, E=NULL, F=NULL, Wx = NULL, Wa = NULL, type = 2, tol = sqrt(.Machine$double.eps),
                      tolrank = NULL, fulloutput = TRUE, verbose = TRUE)

  return(e)

}

flexconstr_sigfit_multipleSamples <- function(P,
                                              M,
                                              mut_tol=0,
                                              allmut_tolratio=0.01){
  E <- list()
  for(i in 1:ncol(M)){
    m <- M[,i,drop=F]
    e <- flexconstr_sigfit(P,m, mut_tol, allmut_tolratio)
    E[[colnames(M)[i]]] <- as.matrix(e$X)
  }
  E <- do.call(cbind,E)
  colnames(E) <- colnames(M)
  return(E)
}



## Fast update-H style fitting
fast_nnls_KLD <- function(X,W,n_iters=1000){
  X <- as.matrix(X)
  W <- as.matrix(W)

  ## initialize at the mean
  H_est<-matrix(mean(X)*runif(ncol(W)*ncol(X)),nrow = ncol(W),ncol=ncol(X))
  colnames(H_est) <- colnames(X)

  ## use the NMF package H update as an approxiamtion to KLD NNLS
  for(i in 1:n_iters){
    H_est <- NMF::nmf_update.KL.h(X,W,h = H_est)
  }

  return(round(H_est))
}


