## cosine comparison functions

## one vs. all cosine comparison using matrix math
calculate_cosine_sim_all_v_one <- function(X,baseline){
  ## linearize baseline
  baseline <- as.matrix(c(baseline),ncol=1)
  X_T <- t(X)

  cos_sim <- (X_T%*%baseline)/(sqrt(rowSums(X_T^2))*sqrt(sum(baseline^2)))
  return(cos_sim)
}


cos_sim <- function (a, b)
{
    return(sum(a * b)/sqrt(sum(a^2) * sum(b^2)))
}

## pairwise (all vs. all ) cosine similarity using fast cosine sim function
calcuate_pairwise_cossim <- function(mat){ ## mat is col=samples, row = channels
  ## Calculate pairwise cosine similarity
  pairwise_cosine_sim <-  apply(expand.grid(1:ncol(mat),1:ncol(mat)),1,function(d){
    cos_sim(mat[,d[1],drop=T],mat[,d[2],drop=T])
  })
  pairwise_cosine_sim <- matrix(pairwise_cosine_sim,ncol = ncol(mat),byrow = F)
  rownames(pairwise_cosine_sim) <- colnames(pairwise_cosine_sim) <- colnames(mat)
  return(pairwise_cosine_sim)
}


## pairwise cosine similarity implementation from Andrea's package
computeCorrelation <- function(x){
  if (ncol(x)==1){
    #if there is only one column, correlation matrix is 1
    return(matrix(1, 1, 1))
  }
  out <- matrix(NA, ncol(x), ncol(x))
  #diagonal is 1
  for(i in 1:ncol(x)){
    out[i,i] <- 1
  }
  colnames(out) <- colnames(x)
  rownames(out) <- colnames(x)
  for(i in 2:ncol(x)){
    for(j in 1:(i-1)){ #up to i-1, diag already set to 1
      #message(i, " ", j)
      out[i,j] <- cos.sim(as.numeric(x[,i]), as.numeric(x[,j]))
      out[j,i] <- out[i,j] #upper triangular is the same
    }
  }
  return(out)
}
#####
