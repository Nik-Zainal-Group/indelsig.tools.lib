#library(indelsiglib)
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(mixtools)
source("clustering.R")
source("plot_utils.R")
source("channel_sets.R")
library(indelsig.tools.lib)


## data_c is the transposed matrix of the catalogs. The scripts starts from this step. The following commented line is code is just to show what the data_c object is generated; it's just for explicative purposes.
#data_c <- as.data.frame(t(catalogs))

#################################################################################################

threshold <- 100
project <- "gel_IC"
## choose your out_dir
outdir <- outdir

tissue <- tissue

pgel_all <- list()
pgel_all[[j]] <- list(catalog=as.data.frame(t(data_c)))
pgel_all[[j]][["burden"]] <- rowSums(data_c)
pgel_all[[j]][["name"]] <- j

set.seed(123456)

outdir_tissue <- paste0(outdir,project,'/',threshold,'/',tissue,'/')

dir.create(outdir_tissue,recursive = T,showWarnings = F)

## tissue
pgel_all[[tissue]]$too_low <- pgel_all[[tissue]]$burden < threshold
pgel_all[[tissue]]$catalog_filtered <- pgel_all[[tissue]]$catalog[,!pgel_all[[tissue]]$too_low]



#print(pgel_all[[tissue]]$catalog_filtered)
pgel_all[[tissue]]$burden_filtered <- colSums(pgel_all[[tissue]]$catalog_filtered)

source("cos_sim.R")
## generate pairwise cossim
pgel_all[[tissue]]$pairwise_cossim <- calcuate_pairwise_cossim(pgel_all[[tissue]]$catalog_filtered)
pgel_all[[tissue]]$pairwise_cossim_distmat <- as.dist(1-pgel_all[[tissue]]$pairwise_cossim,diag = TRUE)

## generate clustering result
clustering <- list()
clustering$hc1_method <- 'complete'
clustering$hc1 <- stats::hclust(d = pgel_all[[tissue]]$pairwise_cossim_distmat,method=clustering$hc1_method)

clustering$cos_sim_seq <-seq(0.1,0.95,0.005)

clustering$hc1_cuts <- lapply(
clustering$cos_sim_seq,function(i){
    cutree(clustering$hc1,h=i)
})

clustering$hc1_cuts_not_null <- sapply(clustering$hc1_cuts,function(x){length(table(x))})>1
clustering$hc1_cuts <- clustering$hc1_cuts[clustering$hc1_cuts_not_null]
clustering$cos_sim_seq <- clustering$cos_sim_seq[clustering$hc1_cuts_not_null]

clustering$hc1_n_clusters <- unlist(lapply(clustering$hc1_cuts,function(x){length(table(x))}))

clustering$hc1_sw <- sapply(clustering$hc1_cuts,function(x){
mean(cluster::silhouette(x,pgel_all[[tissue]]$pairwise_cossim_distmat)[,'sil_width'])
})

clustering$hc1_stats <- as_tibble(data.frame(
clustering$cos_sim_seq,
clustering$hc1_sw,
clustering$hc1_n_clusters
))



## Diagnostic plots
cl_plt_0 <- clustering$hc1_stats %>%
reshape2::melt(id.vars='clustering.cos_sim_seq') %>%
ggplot(aes(x=clustering.cos_sim_seq,y=value,color=variable))+
geom_line()+
facet_wrap(~variable,ncol=2,scales = 'free_y')

ggsave(filename = paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_clustering-summary.png'),plot = cl_plt_0,width = 10,height = 4)



n_max <- min(length(clustering$hc1$labels), 20)
for (k in 5:n_max) {
    print(k)

    clustering$final_cut$cut <- cutree(clustering$hc1,k=k)
    clustering$final_cut$method <- clustering$hc1_method
    clustering$final_cut$clusters <- split(names(clustering$final_cut$cut),f=clustering$final_cut$cut)
    clustering$final_cut$clusters_catalogs <- lapply(clustering$final_cut$clusters,function(x){pgel_all[[tissue]]$catalog_filtered[,x,drop=F]})
    clustering$final_cut$clusters_means_mat <- do.call(cbind,lapply(clustering$final_cut$clusters_catalogs,rowMeans))
    clustering$final_cut$clusters_sd_mat <- do.call(cbind,lapply(clustering$final_cut$clusters_catalogs,function(x){apply(x,1,sd)}))
    clustering$final_cut$clusters_burdens <-lapply(clustering$final_cut$clusters_catalogs,colSums)

    clustering$final_cut$cluster_burdens_df <- tibble::enframe(c(clustering$final_cut$clusters_burdens,recursive=T)) %>%
      mutate(cl=as.numeric(sapply(strsplit(name,'\\.'),'[[',1)))
    clustering$final_cut$cluster_burdens_df <- left_join(clustering$final_cut$cluster_burdens_df, clustering$final_cut$cluster_burdens_df %>% group_by(cl) %>% summarize(n=n()))

 
    cl_plt_1 <- clustering$final_cut$cluster_burdens_df %>%
      ggplot(aes(x=as.factor(as.numeric(cl)),y=value,group=cl,fill=n))+
      geom_violin()+
      geom_jitter()+
      geom_hline(yintercept =(mean(clustering$final_cut$cluster_burdens_df$value)) ,color='red')+
      geom_hline(yintercept =(median(clustering$final_cut$cluster_burdens_df$value)) ,color='green')+
      scale_y_log10()+
      scale_fill_gradient2()+
      theme_classic()+
      labs(x='Cluster Number',y='log10(Burden)',fill='Cluster Size')

    ggsave(filename = paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_k=',k,'_violin.png'),plot = cl_plt_1,width = 10,height = 4)


    cl_plt_2 <- clustering$final_cut$cluster_burdens_df %>%
      ggplot(aes(x=as.factor(as.numeric(cl)),y=value,group=cl,fill=n))+
      geom_boxplot()+
      geom_jitter()+
      geom_hline(yintercept =(mean(clustering$final_cut$cluster_burdens_df$value)) ,color='red')+
      geom_hline(yintercept =(median(clustering$final_cut$cluster_burdens_df$value)) ,color='green')+
      scale_y_log10()+
      scale_fill_gradient2()+
      theme_classic()+
      labs(x='Cluster Number',y='log10(Burden)',fill='Cluster Size')

    ggsave(filename = paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_k=',k,'_boxplot.png'),plot = cl_plt_2,width = 10,height = 4)


    #### mixture modelling
    ## 1 - fit a mixture model to the overall density of the dataset to get an idea of where to initiate clustering
    m1 <- mixtools::normalmixEM(log10(pgel_all[[tissue]]$burden_filtered),lambda = c(0.5,0.5),maxrestarts = 1000)

    png(filename = paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_k=',k,'_total-mixture.png'))
    plot(m1,2) ## fits a clear density
    dev.off()


    ## starting parameter estimates <- m1$mu
    mu_est <- m1$mu
    ## 2 - determine if any of the clusters have two distinct populations, and if so, split them into two distinct clusters
    ## do this by fitting two models, one for a single-normal and one for a mixture of 2 normal

    g <-mapply(names(clustering$final_cut$clusters_burdens),clustering$final_cut$clusters_burdens,FUN=function(cluster_name,data){
      print(length(data))
      data <- log10(data) ## log-normalized data

      if(length(data) >= 10){

        m2_1 <- fit_single_norm((data))

        #boot_m2_2 <- fit_norm_2mix_boot(data,m2_2$mixture,B = 100) ## previously implemented bootstrapping for significance, but not useful anymore
        ## instead, fit multiple times on the same data using random initialization to get the range of model fits

        m2_2 <- lapply(1:100,function(x){

          try({
            fit_norm_2mix(data,mu = m1$mu)
          })

        })
        m2_2 <- m2_2[sapply(m2_2,length)==3]


        ## Pick the representative model most closely matching the median BIC
        m2_2_rep <- m2_2[[which.min(abs(sapply(m2_2,'[[','BIC')-median(sapply(m2_2,'[[','BIC'))))]]
        m2_2$mean_BIC <- mean(sapply(m2_2,'[[','BIC'))
        m2_2$rep <- m2_2_rep

        #plot(m2_2_rep$mixture,whichplots = 2,breaks=100)

        stats <- data.frame(
          m2_1$loglik,
          m2_2$rep$loglik,
          m2_1$BIC,
          m2_2$rep$BIC,
          m2_2$mean_BIC,
          diff(10^m2_2$rep$mixture$mu))

        ## pre-calculate membership in low or high component of the mixture
        low_index <- ifelse(m2_2$rep$mixture$mu[1]<=m2_2$rep$mixture$mu[2],1,2)
        high_index <- ifelse(low_index==1,2,1)

        ## calculate baseline outliers
        outliers <- sort(outliers_IQR(10^data)) ## baseline outliers


        ## if 2-component model fits better than 1-component model
        if(m2_2$mean_BIC < m2_1$BIC){ ## prefer 1 fit model if there are ties
          n_components <- 2
          ## map low and high distributions

          ## classify all those w/ a high index greater than the decision boundary as part of a new cluster
          ## generate new clusters from MM
          MM_cluster <- sort(names(data)[m2_2$rep$mixture$posterior[,high_index]>0.5]) 

          ## get left over old cluster
          non_MM_cluster <- data[!(names(data) %in% MM_cluster)]

          ## get outliers from old_cluster
          non_MM_cluster_outliers <-sort(outliers_IQR(10^non_MM_cluster))

          #IQR_low_MM_cluster <- sort(outliers_IQR_low(10^))

          ## merge old outliers + new_clusters
          refined_outliers <- sort(c(non_MM_cluster_outliers,MM_cluster))
          refined_clusters <- names(data[!(names(data) %in% refined_outliers)])

        } else{
          n_components <- 1
          ## get outliers from data directly
          MM_cluster <- c()
          refined_outliers <- outliers
          refined_clusters <- names(data[!(names(data) %in% refined_outliers)])
        }


        posterior_df <- tibble(samples=names(data),
                               data,
                               posterior=m2_2$rep$mixture$posterior[,high_index])


        posterior_df$outliers<- 'not_outlier'
        posterior_df$mm_outliers<- 'not_MM_outlier'
        posterior_df$refined_outliers <- 'not_refined_outlier'


        posterior_df$outliers[posterior_df$samples %in% outliers] <- 'outlier'
        posterior_df$mm_outliers[posterior_df$samples %in% MM_cluster] <- 'mm_outlier'
        posterior_df$refined_outliers[posterior_df$samples %in% refined_outliers] <- 'refined'


        
        ## plot final overall data distribution
        plots_function = ggplot( data = posterior_df, aes(x=data,y=posterior,color=refined_outliers))+
          labs(x="Log10(counts)",y="Prob Hyp Member",color="Final")+
          scale_y_continuous(limits = c(0,1))+
          geom_point() + facet_wrap(outliers~refined_outliers+mm_outliers,nrow = 1)+
          ggtitle(paste0("Cluster ",cluster_name," Model=",n_components," components"))+
          theme(axis.text = element_text(size = 10))

        ret <- list(refined_clusters=(refined_clusters),
                    refined_outliers=(refined_outliers),
                    stats=stats,
                    n=n_components,
                    m1=m2_1,
                    m2=m2_2_rep,
                    plt=plots_function)

      } else{

        ## otherwise, just collect normal outliers using raw counts instead of log counts
        refined_outliers <- sort(outliers_IQR(10^data))
        refined_clusters <- names(data[!(names(data) %in% refined_outliers)])

        ret <- list(refined_clusters=refined_clusters,refined_outliers=refined_outliers,n=0)

      }

      return(ret)

    },SIMPLIFY = F)

    ## remove all cases not fit by mixture models
    g_filtered <- sapply(g,'[[','n')>0

    ## processing after hypermutator filtering

    ## collect all plts
    plts <- lapply(g,'[[','plt')
    plts <- plts[g_filtered]

    ## save plots
    if(length(plts) > 0){
      outlier_plt_1 <- arrangeGrob(grobs = plts,ncol = 4)

      ggsave(paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_k=',k,'_outliers.png'),
             plot = outlier_plt_1,width = 40,height = 20,limitsize = F)

      ## outlier plot 2
      png(filename = paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_k=',k,'_outliers_mixture-fits.png'),
          width = 2000,height =500*ceiling(sum(g_filtered)/4))
      par(mfrow=c(ceiling(sum(g_filtered)/4),4))
      for(entry in which(g_filtered)){
        plot(g[[entry]]$m2$mixture,2,breaks=100)
        title(paste0('Cluster ',entry),line=+3)
      }
      dev.off()


      ## extract refind clusters and outliers
      refined_clusters <- lapply(g,'[[','refined_clusters')
      refined_outliers <- lapply(g,'[[','refined_outliers')
      ## rename
      names(refined_outliers) <- paste0(names(refined_outliers),'_Hyp')

      refined <- c(refined_clusters,refined_outliers)
      refined <- refined[sapply(refined,length)>0]

      ## save the list
      saveRDS(refined,
           file = paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_k=',k,'_outliers_refined-cluster-members.rds')
      )


      ## extract statistics
      refined_catalogs <- lapply(refined,function(x){pgel_all[[tissue]]$catalog_filtered[,x,drop=F]})
      refined_means_mat <- do.call(cbind,lapply(refined_catalogs,rowMeans))
      refined_means_mat[is.na(refined_means_mat)] <-0
      refined_sd_mat <- do.call(cbind,lapply(refined_catalogs,function(x){apply(x,1,sd)}))
      refined_sd_mat[is.na(refined_sd_mat)] <- 0
      refined_burdens <- lapply(refined_catalogs,colSums)

      refined_burdens_df <- tibble::enframe(c(refined_burdens,recursive=T)) %>%
        mutate(cl=(sapply(strsplit(name,'\\.'),'[[',1)))
      refined_burdens_df <- left_join( refined_burdens_df, refined_burdens_df %>% group_by(cl) %>% summarize(n=n()))

      cl_plt_2 <- refined_burdens_df %>%
        ggplot(aes(x=factor(cl,levels=paste0(rep(1:100,each=2),c("","_Hyp"))),y=value,group=cl,fill=n))+
        geom_boxplot()+
        geom_jitter()+
        geom_hline(yintercept =(mean(refined_burdens_df$value)) ,color='red')+
        geom_hline(yintercept =(median(refined_burdens_df$value)) ,color='green')+
        scale_y_log10()+
        scale_fill_gradient2()+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 50,hjust = 1))+
        labs(x='Cluster Number',y='log10(Burden)',fill='Cluster Size')

      ggsave(filename = paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_k=',k,'_outliers_burden-log.png'),
             width=16,height = 6,plot = cl_plt_2)


      cl_plt_3 <- refined_burdens_df %>%
        ggplot(aes(x=factor(cl,levels=paste0(rep(1:100,each=2),c("","_Hyp"))),y=value,group=cl,fill=n))+
        geom_boxplot()+
        geom_jitter()+
        geom_hline(yintercept =(mean(refined_burdens_df$value)) ,color='red')+
        geom_hline(yintercept =(median(refined_burdens_df$value)) ,color='green')+
        #scale_y_log10()+
        scale_fill_gradient2()+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 50,hjust = 1))+
        labs(x='Cluster Number',y='log10(Burden)',fill='Cluster Size')

      ggsave(filename = paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_k=',k,'_outliers_burden.png'),
             width=16,height = 6,plot = cl_plt_3)

      cl_plt_4<- signature_barplots(mat =refined_means_mat,
                                                  err = refined_sd_mat,
                                                  text_size = 11,
                                                  nrow = ceiling(ncol(refined_means_mat)/5),
                                                  ncol = 5 ,
                                                  return_list = T,
                                                  #return_grob = TRUE,
                                                  channel_set = 'ch2')

      ## modify plots to include # of samples
      cl_plt_4 <- mapply(refined_catalogs,cl_plt_4,FUN=function(cat,plt){

        plt$labels$title<-paste0(plt$labels$title,", n=",ncol(cat))
        return(plt)

      },SIMPLIFY = F)

      cl_plt_4 <- arrangeGrob(grobs = cl_plt_4,ncol = 5)

      ggsave(filename = paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_k=',k,'_outliers_cluster-means.png'),
             width=50,height = ceiling(ncol(refined_means_mat)/5)*5,limitsize = F,plot = cl_plt_4)


      n_per_cluster <- data.frame(n_clusters=sapply(refined_clusters,length),n_outliers=sapply(refined_outliers,length))
      n_per_cluster$cluster <- 1:nrow(n_per_cluster)

      write.table(x = n_per_cluster,file = paste0(outdir_tissue,pgel_all[[tissue]]$name,'_',clustering$final_cut$method,'_k=',k,'_outliers_cluster-counts.txt'))

    }

  }



