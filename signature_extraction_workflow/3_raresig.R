#library(indelsiglib)
library(dplyr)
library(tibble)
library(ggplot2)
library(NNLM)
library(grid)
library(gridExtra)


source("fitting.R")
source("cos_sim.R")
source("plot_utils.R")

tissue = tissue
organ = tissue
config = config ## The config is the ID of the extraction
n_chosen = n_chosen ## Number of signatures extracted in the first round of extractions


j <- tissue


storage_dir <- storage_dir
dir.create(storage_dir)

## data_c is the transpose of the catalogue matrix; samples as rows and channels as columns
data_c <- data_c




rownames(data_c) <- data_c$sample
data_c$sample <- NULL
data_c$organ <- NULL
#################################################################################################



catalogs <- as.data.frame(t(data_c))


pgel  <- list(catalog = catalogs, 
              burden = colSums(catalogs),
              name = tissue,
              n=ncol(catalogs))

pgel_all <- list()
pgel_all[[tissue]] <- pgel


pval_df <- read.table(paste0("extractions/",organ,"/fitting_pval/",organ,"_config", config,"_nsig=",n_chosen,"/",organ,"_config_", config,"_nsigs=",n_chosen, "_pvals.tsv"), sep = "\t", header = T)


nsig <- n_chosen
project <- paste0(organ, "_", "config_", config, "_nsig_", nsig)
run_name <- paste0(organ,"_", "config_", config, "_nsig_", nsig)

dir.create(paste0(storage_dir,'/',project,'/'), showWarnings = F)

## Select signficant residual / error samples
pval_df$residual_significant <- p.adjust(pval_df$norm_residualSAD_normpvalue)<0.05
pval_df$error_significant <- p.adjust(pval_df$norm_errorSAD_emppvalue)<0.05

## get the samples significant for refitting


unexplained_samples <- pval_df$samples[pval_df$error_significant]

unexplained_catalog <- pgel_all[[tissue]]$catalog[,unexplained_samples]

## Load the signature file
## sigs is the path to the signature file 
sigs <- sigs

signatures <- read.table(sigs, sep = "\t")

## Fit with flexconstr, and get the residuals
exposures <-  fast_nnls_KLD(unexplained_catalog,signatures,n_iters=1000)

E <- flexconstr_sigfit_multipleSamples(as.matrix(signatures),
                                       as.matrix(unexplained_catalog),allmut_tolratio = 0.003)
R <- unexplained_catalog - as.matrix(signatures) %*% as.matrix(E)

save.image(paste0(storage_dir, "workspace.RData"))

## R is the set of positive residuals
## Catalogues clustering:
## generate distmatrix

## generate pairwise cossim on residuals R
rare_clustering <- list()
rare_clustering$pairwise_cossim <- calcuate_pairwise_cossim(R)
rare_clustering$pairwise_cossim_distmat <- as.dist(1-rare_clustering$pairwise_cossim,diag = TRUE)
## generate clustering result
rare_clustering$hc1_method <- 'average'
rare_clustering$hc1 <- stats::hclust(d = rare_clustering$pairwise_cossim_distmat,method=rare_clustering$hc1_method)

#try a cutree
rare_clustering$k_seq <- 2:(min(20,ncol(R))-1)

rare_clustering$hc1_cuts <- lapply(
  rare_clustering$k_seq,function(i){
    cutree(rare_clustering$hc1,k=i)
  })
## Filter NULL cuts
rare_clustering$hc1_cuts_not_null <- sapply(rare_clustering$hc1_cuts,function(x){length(table(x))})>1

## Adjust k-seq and the clustering results to drop null cases
rare_clustering$hc1_cuts <- rare_clustering$hc1_cuts[rare_clustering$hc1_cuts_not_null]
rare_clustering$k_seq <- rare_clustering$k_seq[rare_clustering$hc1_cuts_not_null]

rare_clustering$hc1_n_clusters <- unlist(lapply(rare_clustering$hc1_cuts,function(x){length(table(x))}))

rare_clustering$hc1_sw <- sapply(rare_clustering$hc1_cuts,function(x){
  mean(cluster::silhouette(x,rare_clustering$pairwise_cossim_distmat)[,'sil_width'])
})

rare_clustering$hc1_stats <- as_tibble(data.frame(
  rare_clustering$k_seq,
  rare_clustering$hc1_sw,
  rare_clustering$hc1_n_clusters
))

rarecl_plt_0 <- rare_clustering$hc1_stats %>%
  reshape2::melt(id.vars='rare_clustering.k_seq') %>%
  ggplot(aes(x=rare_clustering.k_seq,y=value,color=variable))+
  geom_line()+
  facet_wrap(~variable,ncol=2,scales = 'free_y') + scale_x_continuous(breaks = 1:30) + theme_bw() + theme(panel.grid.minor = element_blank())


ggplot2::ggsave(paste0(storage_dir,'/',project,'/',project,'_pval-q0.05_raresigs_',rare_clustering$hc1_method,'_silhouette-width.pdf'),
                plot = rarecl_plt_0,
                width=10,height=5,limitsize = FALSE)## Pick a value of k



library(gridExtra)

for(k in rare_clustering$k_seq){
  library(ggplot2)
  library(gridExtra)
  k_char <- as.character(k)
  message(paste0("Running cluster k="), k_char)
  ## Extract the sub catalogs
  rare_clustering$hc1_cuts_exp[[k_char]]$cluster_catalogs <- lapply(1:k,function(x){
    cl <- (rare_clustering$hc1_cuts[[which(rare_clustering$k_seq==k)]]==x)
    cl <- names(cl)[cl]
    list(catalog = unexplained_catalog[,cl,drop=F],
         exposure = E[,cl,drop=F]
    )
  })
  
  ## Plot the mean profiles for samples
  rare_clustering$hc1_cuts_exp[[k_char]]$mean_profiles <- sapply(rare_clustering$hc1_cuts_exp[[k_char]]$cluster_catalogs,function(x){rowMeans(x$catalog)})
  colnames(rare_clustering$hc1_cuts_exp[[k_char]]$mean_profiles) <- paste0("Cluster ",1:k)
  rare_clustering$hc1_cuts_exp[[k_char]]$sd_profiles <- sapply(rare_clustering$hc1_cuts_exp[[k_char]]$cluster_catalogs,function(x){apply(x$catalog,1,sd)})
  rare_clustering$hc1_cuts_exp[[k_char]]$sd_profiles[is.na(rare_clustering$hc1_cuts_exp[[k_char]]$sd_profiles)] <- 0
  colnames(rare_clustering$hc1_cuts_exp[[k_char]]$sd_profiles) <- paste0("Cluster ",1:k)
  
  
  cl_plt1<- signature_barplots(mat =rare_clustering$hc1_cuts_exp[[k_char]]$mean_profiles ,
                                             err = rare_clustering$hc1_cuts_exp[[k_char]]$sd_profiles ,
                                             text_size = 11,
                                             nrow = ceiling(ncol(rare_clustering$hc1_cuts_exp[[k_char]]$mean_profiles)/5),
                                             ncol = 5 ,
                                             return_list = T,
                                             #return_grob = TRUE,
                                             channel_set = 'ch2')
  ## modify plots to include # of samples
  cl_plt1 <- mapply(rare_clustering$hc1_cuts_exp[[k_char]]$cluster_catalogs,
                    cl_plt1,
                    FUN=function(x,plt){
                      
                      plt$labels$title<-paste0(plt$labels$title,", n=",ncol(x$catalog))
                      return(plt)
                      
                    },SIMPLIFY = F)
  
  cl_plt1 <- arrangeGrob(grobs = cl_plt1,ncol = 5)
  
  ggplot2::ggsave(paste0(storage_dir,'/',project,'/',project,'_pval-q0.05_raresigs_',rare_clustering$hc1_method,'_cut-k=',k_char,'_cluster-means.pdf'),
                  plot = cl_plt1,width=55,height=(ceiling(ncol(rare_clustering$hc1_cuts_exp[[k_char]]$mean_profiles)/5))*4.5,limitsize = FALSE)
  
  
  
  
  
  ## Re-extract by subcluster
  for (subcluster in 1:k){
    print(c(tissue,k,subcluster))
    ## pick a cluster
    subcluster_catalog <- rare_clustering$hc1_cuts_exp[[k_char]]$cluster_catalogs[[subcluster]]$catalog
    subcluster_exposure <- rare_clustering$hc1_cuts_exp[[k_char]]$cluster_catalogs[[subcluster]]$exposure
    
    ## Plot the subcatalog + residuals
    ncol <- 5
    nrow=ceiling(ncol(subcluster_catalog)/ncol)
    height = (nrow)*4.5
    
    ## Plot the cluster residuals
    
    ## Plot the new signatures
    p0 <- signature_barplots(mat = R[,colnames(subcluster_catalog),drop=F],
                                           text_size = 12,
                                           ncol = ncol,
                                           nrow = nrow,
                                           return_grob = TRUE)
    
    ggplot2::ggsave(paste0(storage_dir,'/',project,'/',project,'_pval-q0.05_raresigs_',rare_clustering$hc1_method,'_cut-k=',k_char,'_cl=',subcluster,'_residuals.pdf'),
                    plot = p0,width=55,height=height,limitsize = FALSE)
    
    
    
    
    
    rare_clustering$hc1_cuts_exp[[k_char]]$cluster_catalogs[[subcluster]]$extraction <- list()
    
    ## At max go up to 10 new signatures per subcluster in extraction
    for (n_new_sigs in 1:min(ncol(subcluster_catalog),10)){
      ## set seed for random factorization
      set.seed(12345)

      ## use NNLM with fixed initialization NMF
      ## 100K iterations is sufficient in testing to converge in most cases.
      res.nnmf <- NNLM::nnmf(as.matrix(subcluster_catalog),
                             init = list(W0 = as.matrix(signatures),H1 = as.matrix(subcluster_exposure)),
                             loss = "mkl",
                             method = "lee",
                             k = n_new_sigs,
                             max.iter = 100000,check.k = FALSE)
      ## normalize new signatures
      rareSignatures <- apply(res.nnmf$W[,1:n_new_sigs,drop=F],2,function(x)x/sum(x))
      colnames(rareSignatures) <- paste0("R",1:n_new_sigs)
      
      new_results <- list()
      new_results <- list(n_new_sigs=n_new_sigs,
                          new_signatures=cbind(signatures,rareSignatures))
      
      ## refit the subcluster catalog with the new signatures
      new_results$new_exposures <- fast_nnls_KLD(subcluster_catalog, ## re-fit using new signatures
                                                 new_results$new_signatures,
                                                 n_iters = 1000)
      
      rare_clustering$hc1_cuts_exp[[k_char]]$cluster_catalogs[[subcluster]]$extraction[[n_new_sigs]] <-new_results
      
      ## plot individual signatures
      ncol <- 5
      nrow=ceiling(ncol(new_results$new_signatures)/ncol)
      height = (nrow)*4.5
      
      ## Plot the new signatures
      p1 <- signature_barplots(mat = new_results$new_signatures,
                                             text_size = 12,
                                             ncol = ncol,
                                             nrow = nrow,
                                             return_grob = TRUE)
      
      ggplot2::ggsave(paste0(storage_dir,'/',project,'/',project,'_pval-q0.05_raresigs_',rare_clustering$hc1_method,'_cut-k=',k_char,'_cl=',subcluster,'_newsigs=',n_new_sigs,'.pdf'),
                      plot = p1,width=55,height=height,limitsize = FALSE)
      
      
    }
  }
}


## save the rare_clustering object
save(rare_clustering,
     file = paste0(storage_dir,'/',project,'/',project,'_pval-q0.05_raresigs_',rare_clustering$hc1_method,'_cut-k=1:30.rda')
)
# })





