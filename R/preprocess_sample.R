#' Normalize a vector by euclidean length
#'
#' @param v The target vector
#' @return normalized vector
#' @export
normalize_euclidean <- function(v){
  return(v/sqrt(sum((v)^2)))

}

#' Normalize a vector by manhattan length
#'
#' @param v The target vector
#' @return normalized vector
#' @export
normalize_manhattan <- function(v){
  return(v/sum(v))
}


#' Combine low indel burden samples to make synthetic sample with indel number
#' more than a given threshold
#' @param mut_cat An indel catalogue of multiple samples
#' @param ss_threshold Threshold of indel number (default: 500)
#' @return An indel catalogue of multiple samples
#' @export
ss.sample <- function(mut_cat, ss_threshold=500){

  set.seed(123)
  mut_num <- data.frame("Sample"=names(mut_cat),"indel_num"=colSums(mut_cat))

  sample_less250 <- mut_num[mut_num$indel_num<ss_threshold,]
  indel_less250 <- as.data.frame(mut_cat[,sample_less250$Sample])

  nsample_less250 <- dim(indel_less250)[2]

  # if there are only 2 or 1 low indel burden samples, no need to do bootstrapping for them.
  if(nsample_less250>2){

    # bootstraping simulation to decide number of samples to add up to ss_threshold
    bt_mean=0
    i=2
    while(bt_mean<ss_threshold){
      bt_all <- NULL
      for(j in 1:1000){
        bt_j <- sample(sample_less250$indel_num, i, replace = FALSE)
        bt_all <- c(bt_all,sum(bt_j))
      }
      bt_mean <- mean(bt_all)
      i=i+1
    }

    # nsample_combi: the number of samples to add up
    k=1
    nsample_combi <- i-1
    synthetic_samples <- NULL
    #ss_number <- ifelse(ss_number>nsample_less250, ss_number, nsample_less250)
    ss_number <- nsample_less250
    while (k <= ss_number) {
      ssk_sample <- indel_less250[,sample(1:nsample_less250, nsample_combi, replace = FALSE)]
      if(sum(ssk_sample)>=ss_threshold){
        synthetic_samples <- cbind(synthetic_samples, rowSums(ssk_sample))
        k <- k+1
      }
    }
    synthetic_samples <- as.data.frame(synthetic_samples)
    names(synthetic_samples) <- paste0("ss",1:nsample_less250)

    # make new indel catalogues with real samples (indel number >= ss_threshold) + synthetic samples
    mut_cat_new <- cbind(mut_cat[,mut_num[mut_num$indel_num>=ss_threshold,"Sample"]], synthetic_samples)

  }else{
    mut_cat_new <- mut_cat
  }

  return(mut_cat_new)

}



#' Plot indel profile in a 89-channel bar plot, original plots_type_4_m4_89 function
#'
#' @param mut_cat An indel catalogue of multiple samples
#' @param rescale.type types of rescale: "hypermutator", "manhattan", "euclidean". "hypermutator" is to normalize mutation burden of hypermutators to between 2,000-5,000. "manhattan" is 1 norm distance. "euclidean" is 2 norm distance.
#' @return Rescaled indel catalogue to total indel number = 1000
#' @export
rescale.sample <- function(mut_cat,rescale.type){

  if(rescale.type=="hypermutator"){

    mut_num <- data.frame("Sample"=names(mut_cat),"indel_num"=colSums(mut_cat))

    max_num <- max(mut_num$indel_num)

    if(max_num>2000){
      num_lowbound <- 2000
      num_highbound <- 5000

      hyper_sample <- mut_num[mut_num$indel_num>num_lowbound,]

      hyper_sample_min <- min(hyper_sample$indel_num)

      hyper_sample$scale_num <- hyper_sample_min+(hyper_sample$indel_num-hyper_sample_min)*(5000-hyper_sample_min)/(max_num-hyper_sample_min)
      mut_cat_hyper <- mut_cat[,hyper_sample$Sample]
      mut_cat_hyper <- as.data.frame(round(as.matrix(mut_cat_hyper/colSums(mut_cat_hyper)[col(mut_cat_hyper)]) %*% diag(hyper_sample$scale_num)))
      names(mut_cat_hyper) <- hyper_sample$Sample
      mut_cat_shrink <- cbind(mut_cat_hyper, mut_cat[,mut_num[mut_num$indel_num<=num_lowbound,"Sample"]])

    }else{
      mut_cat_shrink <- mut_cat
    }
    return(mut_cat_shrink)
    # write.table(mut_cat_shrink, paste0(tissue_type,"mut_cat_rescale.txt"), sep = "\t", col.names = T, row.names = T, quote = F)

  }

  # manhattan
  if(rescale.type=="manhattan"){
    mut_cat_shrink <- apply(mut_cat, 2, normalize_manhattan)
    return(round(1000*mut_cat_shrink))

  }

  # educlidean
  if(rescale.type=="euclidean"){

    mut_cat_shrink <- apply(mut_cat, 2, normalize_euclidean)
    return(round(1000*mut_cat_shrink))
  }


}


#' Preprocess samples before signature extraction
#'
#' @param mut_cat An indel catalogue of multiple samples
#' @param ss_threshold Threshold of indel number (default: 500)
#' @param rescale.type types of rescale: "manhattan_hb", "euclidean". "manhattan_hb" is to normalize mutation burden of hypermutators to between 2,000-5,000. "euclidean" is 2 norm distance.
#' @return An indel catalogue of multiple samples
#' @export
preprocess.sample <- function(mut_cat, ss_threshold=500, rescale.type="none"){

  preprocess_sample <- ss.sample(mut_cat,ss_threshold)

  if(rescale.type!="none"){
    preprocess_sample <- rescale.sample(preprocess_sample, rescale.type)


  }

  return(preprocess_sample)
  #  write.table(preprocess_sample,paste0(tissue_type,"_ssthreshold",ss_threshold,"_ssnumber",ss_number,"rescale",rescale,"_indcat.txt"), sep = "\t", col.names = T, row.names = T, quote = F)

}
