library(indelsiglib)
library(dplyr)
library(tibble)


source("null_fit_estimation.R")
source("fitting.R")



## the config is the id of the extraction
config <- config

organ <- organ
tissue <- tissue
### n_chosen is the number of signautres extracted from the first common signature extraction
n_chosen = n_chosen

## number of clusters by which the catalogues are divided for the first extraction
k_clustering <- k_clustering


### excluded clusters of the first extraction
get_excluded_clusters <-  c("7_Hyp", "8_Hyp", "4_Hyp")

### Common signatures path
signatures <- signatures

j <- tissue


## data_c is the transpose fo the catalogue matrix; samples as rows and channels as columns 
data_c <- data_c



#data_c <- rbind(GEL_89[GEL_89$organ == tissue,], ICGC_89[ICGC_89$organ == tissue,])
rownames(data_c) <- data_c$sample
data_c$sample <- NULL
data_c$organ <- NULL
#################################################################################################

threshold <- 100
project <- "gel_IC"


catalog <- as.data.frame(t(data_c))

### out directiory for the computations
outdir_pval <- outdir_pval

dir.create(outdir_pval, showWarnings = F, recursive = T)


outdir_intermediate_significance <- paste0(outdir_pval, "intermediate_RDS/")
dir.create(outdir_intermediate_significance, showWarnings=F, recursive =T)



plot_only <- FALSE
## Run P-val fitting for those runs that are available
## Set the seed inside each loop so it is reproducible
set.seed(12356)

nsig_per_run <- n_chosen
#  run_name <- 
#  print(run_name)
tissue <- organ
print(tissue)

signatures <- read.table(signatures, sep = "\t", header = T)

has_first_step_run <- FALSE 

by_param <- 50

###### FIRST STEP 
if(!plot_only){
  if(!has_first_step_run){
      
      counter <- 0 
      message(paste0("Count=", counter))
      fit <- generate_fits_given_catalog(catalog,signatures = signatures, n_iters = 10000, allmut_tolratio = 0.003)
      err_df <- as_tibble(fit$metrics)
      
      saveRDS(list("fit"=fit, "err_df"=err_df),file = paste0(outdir_intermediate_significance, "fit_and_err_df.rds"))

      for(j in seq(1, ncol(catalog), by = by_param)){
        message("#########################\n#########################\n#########################")
        
        message(paste0("Batch starting with ", j))
        
        if(  (j+by_param-1) >= ncol(catalog) ){
          up_sample <- ncol(catalog)
        }else{
          up_sample <- j+(by_param - 1)
        }
        
        if(file.exists(paste0(outdir_intermediate_significance, "Batch_boots_", j, ".rds") )){
          next
        }
        
        bootstraps <- mclapply(fit$metrics$samples[j:up_sample],generate_nulls_given_fit,fit_object=fit,n_samples=20000,mc.cores = 6)
        
        second_fits <- mclapply(bootstraps, function(x){return(generate_significance_given_null(null_set = x, signatures = signatures))}, mc.cores = 6)

        pvals <- as_tibble(do.call(rbind,lapply(second_fits,function(x){as.data.frame(x$pvals)})))
        
        rm(bootstraps)
        rm(second_fits)
        
        gc()
        
        
        message(paste0("Saving RDS file in: ", paste0(outdir_intermediate_significance, "Batch_boots_", j, ".rds")))
        
        counter <- counter + nrow(pvals)
        
        message(paste0("Samples processed:", counter))
        
        saveRDS(object = list("pvals"=pvals), file = paste0(outdir_intermediate_significance, "Batch_boots_", j, ".rds"))
        
        message("RDS file saved")
        
      }
      
      
      #   ## long running calculation generating parametric boostrap samples from the null model of no excess variation
      #   bootstraps <- mclapply(fit$metrics$samples,generate_nulls_given_fit,fit_object=fit,n_samples=20000,mc.cores = 40)
      #   
      #   second_fits <- mclapply(bootstraps, function(x){return(generate_significance_given_null(null_set = x, signatures = signatures))}, mc.cores = 40)
      #   
      
      quit(save = "no")
  }
  
 
  
#### END FIRST STEP  
  
  fit <- readRDS(paste0(outdir_intermediate_significance, "fit_and_err_df.rds"))$fit
  
  err_df <- readRDS(paste0(outdir_intermediate_significance, "fit_and_err_df.rds"))$err_df
  
  rds_pvals <- list.files(outdir_intermediate_significance, pattern = "*.rds", full.names = T)
  rds_pvals <- rds_pvals[!endsWith(x = rds_pvals, suffix = "_df.rds")]
  
  pvals <- sapply(rds_pvals, simplify = F, function(x){return(readRDS(x))})
  
  pvals <- as_tibble(do.call(rbind,lapply(pvals,function(x){as.data.frame(x$pvals)})))
  
  
  #resccued_NA_pvals <- read.table(file = "extractions/Skin/fitting_pval/Skin_config3_nsig=9/rescued_NA_samples.tsv", sep = "\t", header = T)
  
  
  save.image(paste0(outdir_pval,"bootstraps_v2.Rdata"))
  
  
  ## add minimum values since the p value cannot be strictly zero for downstream computation
  pvals$norm_errorSAD_emppvalue[pvals$norm_errorSAD_emppvalue==0] <-  1/20000
  pvals$norm_residualSAD_emppvalue[pvals$norm_residualSAD_emppvalue==0] <- 1/20000
  
  ## Add Q-value corrected values + thresholding
  pvals$residual_qval <-p.adjust(pvals$norm_residualSAD_normpvalue)
  pvals$error_qval <-p.adjust(pvals$norm_errorSAD_emppvalue)
  
  pvals$residual_significant <-pvals$residual_qval<0.05
  pvals$error_significant <-pvals$error_qval<0.05
  
  final_df <- left_join(err_df,pvals,c('samples'='sample'))
  
  outfile <- paste0(outdir_pval,tissue, "_config_", config,'_nsigs=',n_chosen,'_pvals.tsv')
  readr::write_tsv(final_df,outfile)
  
  #
} else{
  ## If only plotting, read the existing DF instead
  print("Plotting Only")
  final_df <- readr::read_tsv(
    paste0(outdir_pval,run_name,'_nsigs=',nsig_per_run,'_pvals.tsv')
  )
  
}


save.image(paste0( outdir_pval, "bootstraps_v2.Rdata"))
#quit(save = "no")

######




get_membership <- get_membership ## object fo the RDS file describing which samples are in which clusters

get_included_clusters <- names(get_membership)[which( (names(get_membership) %in% get_excluded_clusters) == F )]


final_df <- left_join(final_df, y= reshape2::melt(get_membership[get_included_clusters]), by=c("samples"="value"))

save.image(paste0( outdir_pval, "bootstraps_v2.Rdata"))
readr::write_tsv(final_df,paste0(outdir_pval,tissue, "_config_", config,'_nsigs=',n_chosen,'_pvals.tsv'))


colnames(final_df)[which(colnames(final_df) == "L1")] <- "cluster"

final_df[is.na(final_df$cluster), "cluster"] <- "Exclude"

final_df$selected <- ifelse(final_df$samples %in% reshape2::melt(get_membership[get_included_clusters])$value , "Selected", "Unselected")

final_df$cluster_factor <- factor(final_df$cluster,levels = c('Exclude',as.character(1:100),paste0(1:100,'_Hyp')))

readr::write_tsv(final_df,paste0(outdir_pval,tissue, "_config_", config,'_nsigs=',n_chosen,'_pvals.tsv'))



## PLot
f1 <- final_df %>%
  mutate(sig=residual_significant) %>%
  ggplot(aes(x=burden,y=norm_residualSAD))+
  geom_point(data=select(final_df,-cluster_factor,-selected),color='grey',size=0.8)+
  geom_point(aes(color=sig),size=0.8)+
  scale_x_log10()+
  facet_wrap(cluster_factor~selected)+
  theme_classic()+
  labs(x='Burden',y='Normalized Residual',color='q<0.05')+
  ggtitle(paste0(tissue," n=",nsig_per_run,' Norm. Residual'))


f2 <- final_df %>%
  mutate(sig=error_significant) %>%
  ggplot(aes(x=burden,y=norm_errorSAD))+
  geom_point(data=select(final_df,-cluster_factor,-selected),color='grey',size=0.8)+
  geom_point(aes(color=sig),size=0.8)+
  scale_x_log10()+
  facet_wrap(cluster_factor~selected)+
  theme_classic()+
  labs(x='Burden',y='Normalized Error',color='q<0.05')+
  ggtitle(paste0(tissue," n=",nsig_per_run,' Norm. Error'))


p1 <- arrangeGrob(grobs = list(f1,f2),ncol = 2)
outfile_p1  <- paste0(outdir_pval,organ,'_nsigs=',nsig_per_run,'_by-cluster.png')
ggsave(outfile_p1,plot = p1,width = 20,height = 10)


## Plot group
f1_2 <- final_df %>%
  mutate(sig=residual_significant) %>%
  ggplot(aes(x=burden,y=norm_residualSAD))+
  geom_point(aes(color=sig),size=0.8)+
  scale_x_log10()+
  theme_classic()+
  labs(x='Burden',y='Normalized Residual',color='q<0.05')+
  ggtitle(paste0(tissue," n=",nsig_per_run,' Norm. Residual'))


f2_2 <- final_df %>%
  mutate(sig=error_significant) %>%
  ggplot(aes(x=burden,y=norm_errorSAD))+
  geom_point(aes(color=sig),size=0.8)+
  scale_x_log10()+
  theme_classic()+
  labs(x='Burden',y='Normalized Error',color='q<0.05')+
  ggtitle(paste0(tissue," n=",nsig_per_run,' Norm. Error'))


p2 <- arrangeGrob(grobs = list(f1_2,f2_2),ncol = 2)
outfile_p2 <- paste0(outdir_pval,organ,'_nsigs=',nsig_per_run,'_overall.png')
ggsave(outfile_p2,plot = p2,width = 10,height = 4)




