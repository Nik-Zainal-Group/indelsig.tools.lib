## sig_extract_utils.R
## These functions are largely for working with extraction results from SignatureExtraction from signature.tools.lib


calculate_mean_sd_signatures <- function(bootstrap_file,
                                         cut_res_file,
                                         channel_set=NULL){
  load(bootstrap_file)
  load(cut_res_file)
  ns <- ns #as.numeric(stringr::str_match(bootstrap_file,'_ns(\\d+)')[2])

  norm_p_boot <- p_boot/matrix(data=rep(apply(p_boot,2,sum),nrow(p_boot)),nrow = nrow(p_boot),byrow = TRUE)

  mean_signatures <- matrix(NA,nrow = nrow(p_boot),ncol = ns)
  sd_signatures <- matrix(NA,nrow = nrow(p_boot),ncol = ns)
  colnames(mean_signatures) <- paste0("ID",1:ns)
  colnames(sd_signatures) <- paste0("ID",1:ns)
  row.names(mean_signatures) <- row.names(p_boot)
  row.names(sd_signatures) <- row.names(p_boot)

  partitions <- cut_res_MC
  for(nsi in 1:ns){
    pboot_dim <- dim(norm_p_boot[,partitions==nsi,drop=FALSE])
    if(pboot_dim[2]==1){
      mean_signatures[,nsi] <- norm_p_boot[,partitions==nsi]
      sd_signatures[,nsi] <- 0
    }else{
      mean_signatures[,nsi] <- apply(norm_p_boot[,partitions==nsi],1,mean)
      sd_signatures[,nsi] <- apply(norm_p_boot[,partitions==nsi],1,sd)
      sd_signatures[is.na(sd_signatures[,nsi]),nsi] <- 0
    }
  }

  if(!is.null(channel_set)){
    ord <- match(ch[[channel_set]]$channels,rownames(mean_signatures))
    mean_signatures <- mean_signatures[ord,]
    sd_signatures <- sd_signatures[ord,]
  }

  return(list(mean=mean_signatures,sd=sd_signatures,n=ns))
}

## uses file_str + nb / project
extract_mean_sd_sigs <- function(file_dir,project,nb,nrepeats,date,channel_set=NULL){


  file_str <- paste0(date,'_',project,'_nrep',nrepeats,'_boots',nb)
  runs <- list.dirs(file_dir,recursive = FALSE,full.names = TRUE) %>% purrr::keep(stringr::str_detect,pattern=file_str)

  ## for each file directory, locate the bootstrap files

  file_str_bootstrap <- paste0('bootstraps_',project,'_ns\\d+_nboots',nb,'.Rdata')
  file_str_cut_res <- paste0('matchedClustering_',project,'_ns\\d+_nboots',nb,'.Rdata')

  message('Loading files')
  sigs<-list()
  for(i in seq_along(runs)){
    bootstrap_file <- sort(list.files(runs[i],recursive=TRUE,pattern=file_str_bootstrap,full.names=TRUE))
    cut_res_file <- sort(list.files(runs[i],recursive=TRUE,pattern=file_str_cut_res,full.names=TRUE))

    if(length(bootstrap_file)==length(cut_res_file)){

      for( j in seq_along(bootstrap_file)){
        message(".",appendLF=FALSE)
        mean_sd_sig <- calculate_mean_sd_signatures(bootstrap_file[j],
                                                    cut_res_file[j],
                                                    channel_set=channel_set)

        sigs[[as.numeric(mean_sd_sig$n)]] <-mean_sd_sig

      }


    }else{
      message(paste0("Run ",runs[i]," is incomplete and missing bootstrap / cut_res files"))
    }

  }
  message('Loaded files')

  return(sigs)

}


## Align two sets of signatures of size n and n+1 using marriage matching, with the odd one out assigned a new name
align_signatures <- function(os,ns){

  cmp <- matrix(0,nrow=ncol(ns),ncol=ncol(os))
  for(j in 1:ncol(ns)){
    for(k in 1:ncol(os)){
      cmp[j,k] <- cos_sim(ns[,j],os[,k])
    }
  }
  gale_shapely_matches <- matchingR::galeShapley.marriageMarket(cmp,t(cmp))

  matches <- c(gale_shapely_matches$engagements) ## dictates what o/s matches to n/s
  new_names <- colnames(os)[matches] ## new names for n/s based on matching to o/s
  new_names[is.na(new_names)] <- paste0('S',nrow(cmp)) ## give newest signature the maximum number
  new_names_order <- order(as.numeric(stringr::str_remove(new_names,'S')))

  ## fix ns:
  colnames(ns) <- new_names
  ns <- ns[,new_names_order]

  return(list(os=os,ns=ns,new_names=new_names,new_names_order=new_names_order))
}

## Apply signature alignment from 1:N signatures extracted
## this serves to keep similar signatures aligned across the entire extraction
align_signature_set <- function(sig_set){
  ## sort baseline entry
  ## Lowest you can go:
  relevant_sigs <- sort(which(!sapply(sig_set,is.null)))

  lowest <- relevant_sigs[1]
  ## order by signal in 1st channel
  lowest_order <- order(sig_set[[lowest]]$mean[1,])

  ### assumes no missing internal signatures <--> need to figure out what the issue is here.
  new_names <- paste0('S',1:lowest)[lowest_order]

  ## update mean
  colnames(sig_set[[lowest]]$mean) <- new_names
  sig_set[[lowest]]$mean <- sig_set[[lowest]]$mean[,lowest_order]

  ## update SD
  colnames(sig_set[[lowest]]$sd) <- new_names
  sig_set[[lowest]]$sd <- sig_set[[lowest]]$sd[,lowest_order]

  for(i in relevant_sigs[-1]){
    print(i)
    align_os_ns <- align_signatures(os=sig_set[[i-1]]$mean,
                                    ns=sig_set[[i]]$mean)
    ## update mean
    sig_set[[i]]$mean <- align_os_ns$ns
    ## update SD
    colnames(sig_set[[i]]$sd) <- align_os_ns$new_names
    sig_set[[i]]$sd <- sig_set[[i]]$sd[,align_os_ns$new_names_order]
  }

  return(sig_set)
}




## Extract metrics from a SignatureExtraction run
## uses file_str
extract_metrics <- function(file_dir='/home/asn32/rds/rds-sn206-nik-zainal/users/asn32/v2/results/extractions/',
                            project,nb,nrepeats,date){
  file_str <- paste0(date,'_',project,'_nrep',nrepeats,'_boots',nb)

  pot_files <- list.dirs(file_dir,recursive = FALSE,full.names = TRUE)
  metrics <- pot_files %>% purrr::keep(stringr::str_detect,pattern=file_str) %>%
    purrr::map(list.files,recursive=T,pattern='Sigs_OverallMetrics.*.tsv',full.names=T) %>%
    purrr::discard(~length(.)==0) %>%
    purrr::map(readr::read_tsv) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(nsig) %>%
    dplyr::select(nsig,
                  ave.RMSE.orig,ave.RMSE,
                  ave.SilWid.MC,max.MaxBCCS.MC,min.MinWCCS.MC,
                  mmcs_MC,cophenetic.corr.hclust) %>%
    dplyr::filter(nsig!=1)

  metrics[,3] <- metrics[,3]/as.numeric(metrics[1,3])
  metrics[,2] <- metrics[,2]/as.numeric(metrics[1,2])
  metrics <- reshape2::melt(metrics,id.vars=c('nsig'))
  return(metrics)
}


## Plot signature extraction metrics from a SignatureExtraction run
plot_metrics <- function(file_dir='/home/asn32/rds/rds-sn206-nik-zainal/users/asn32/v2/results/extractions/',
                         project,nb,nrepeats,date){

  file_str <- paste0(date,'_',project,'_nrep',nrepeats,'_boots',nb)

  metrics <- extract_metrics(file_dir = file_dir,
                             project = project,
                             nb = nb,
                             nrepeats = nrepeats,
                             date=date)


  m <- ggplot2::ggplot(metrics,ggplot2::aes(x=nsig,y=value,color=variable)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = c("#e6194b","#3cb44b","#000000","#ffe119","#0082c8","#f58231","#911eb4"))+
    ggplot2::labs(x='Number of Signatures',y='Score') +
    ggplot2::ggtitle(file_str) +
    ggplot2::scale_x_continuous(breaks = seq(min(metrics$nsig),max(metrics$nsig),by = 1)) +
    coord_cartesian(ylim=c(0,1.0))+
    labs(color='Metric')+
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=0,size=10,colour = 'black'),
                   axis.text.y= ggplot2::element_text(size=10,colour = "black"),
                   axis.title =  ggplot2::element_text(size=15),
                   plot.title =  ggplot2::element_text(size=10),
                   panel.grid = ggplot2::element_blank(),
                   panel.background =  ggplot2::element_rect(fill = "white"),
                   panel.border =  ggplot2::element_rect(colour = "black", fill=NA),
                   legend.position = 'right')

  return(list(m=m,metrics=metrics))


}



#' Function to compile run information from sigfitlib
#' @export
compile_run <- function(file_dir,project,nb,nrepeats,date,channel_set,outdir,ncol=5,...){
  #browser()
  ## generate project string
  file_str <- paste0(date,'_',project,'_nrep',nrepeats,'_boots',nb)
  outfile_dir <- paste0(outdir,'/',file_str,'/')


  ## create the directory to store the results
  if(!dir.exists(outfile_dir)){
    dir.create(outfile_dir)
  }

  ## extract signatures + align them
  sigs <- extract_mean_sd_sigs(file_dir=file_dir,
                               project= project,
                               nb= nb,
                               nrepeats=nrepeats,
                               date=date,channel_set = channel_set
  )

  sigs <- align_signature_set(sigs)
  ## save sigs as RDS file
  ## extract run metrics ...
  base::save(sigs,file = paste0(outfile_dir,'/',file_str,'_signatures.rda'))

  ## plot individual signatures
  nrow=ceiling(max(unlist(sapply(sigs,'[[','n')))/ncol)
  height = (nrow)*4.5

  relevant_sigs <- sort(which(!sapply(sigs,is.null)))
  for(i in relevant_sigs){
    p <- signature_barplots(mat = sigs[[i]]$mean,
                       err = sigs[[i]]$sd,
                       text_size = 12,
                       ncol = ncol,
                       nrow = nrow,
                       return_grob = TRUE)

    ggplot2::ggsave(paste0(outfile_dir,file_str,'_signatures_ns',sigs[[i]]$n,'.pdf'),
                    plot = p,width=55,height=height,limitsize = FALSE)

    dev.off()
  }

  ## Extract metrics
  mret <- plot_metrics(file_dir=file_dir,
                    project= project,
                    nb= nb,
                    nrepeats=nrepeats,
                    date=date)

  #save(mret[['metrics']],file=paste0(outfile_dir,file_str,'_overall_metrics.rda'))

  ggplot2::ggsave(paste0(outfile_dir,file_str,'_overall_metrics.pdf'),
                  plot = mret$m,width = 8,height = 4)

  return(invisible())

}

#' Function to compile run information from sigfitlib
#' @export
compile_all_runs <- function(extraction_dir,outdir,channel_set='ch2',ncores){
  all_dirs <- list.dirs(extraction_dir,full.names = TRUE,recursive = FALSE)
  all_projects <- stringr::str_replace(all_dirs,'_ns\\d+-\\d+$','')

  unique_projects <- unique(all_projects)

  unique_projects_df <- data.frame(do.call(rbind,stringr::str_match_all(basename(unique_projects),'^(\\d+)_(.+)_nrep(\\d+)_boots(\\d+)')))
  print(unique_projects_df)
  colnames(unique_projects_df) <-c('full','date','project','nrepeats','nb')
  unique_projects_df$path <- unique_projects


  r <- parallel::mclapply(X = 1:nrow(unique_projects_df),function(i){
    print(unique_projects_df[i,])

    compile_run(
      outdir = outdir,
      file_dir = extraction_dir,
      project = unique_projects_df$project[i],
      date=unique_projects_df$date[i],
      nb = unique_projects_df$nb[i],
      nrepeats = unique_projects_df$nrepeats[i],
      channel_set = channel_set
    )
  },mc.cores = ncores)

  return(r)

}





#' Select a specific signature from a sigfitlib collection
#'
#' @param sigs The sigfitlib collection
#' @param n Number of signatures
#' @param mean Extract the mean result
#' @param sd Extract the standard deviation result
#' @param return_mat Whether to return
#' @return Returns a data.frame containing this information
#' @export
#'
#' @examples
#' ''
select_signature <- function(sigs,n,mean=TRUE,sd=FALSE,return_mat=F){
  select_headers <- c('mean','sd')[c(mean,sd)]
  if(length(select_headers)==1){
    ret <- (sigs[which(sapply(sigs,FUN = '[','n')==n)][[1]][[select_headers]])
    if(return_mat){

      ret <- as.matrix(ret[,-c(1:2),drop=F])
    }

  }else{
    ret <- (sigs[which(sapply(sigs,FUN = '[','n')==n)][[1]][select_headers])

    if(return_mat){
      ret <- lapply(ret,function(x){
        as.matrix(x[,-c(1:2),drop=F])
      })
    }
  }

  return(ret)

}

