#' Calculate cosine similarity between two vectors
#'
#' @param v1 The first vector
#' @param v2 The second vector
#' @return The cosine similarity between two vectors
#' @export
cos_similarity <- function(v1,v2){
  v1 <- v1/sum(v1)
  v2 <- v2/sum(v2)
  v1v2 <- sum(v1*v2)
  v1_length <- sqrt(sum(v1*v1))
  v2_length <- sqrt(sum(v2*v2))
  return(v1v2/v1_length/v2_length)
}

#' Calculate cosine similarity between two signature/profile sets
#'
#' @param sigset1 The first signature set
#' @param sigset2 The second signature set
#' @param h Hight of the plot
#' @param w Width of the plot
#' @param text_size Size of text
#' @param ifplot Plot the output or not, default is True
#' @param outputname Output file name of the plot
#' @return A text file including all the cosine similarities between signatures from two data sets
#' @export
cal_cossim_2sigsets <- function(sigset1,sigset2,h,w,text_size,ifplot=T,outputname){

 # muttype <- colnames(sigset2)[1]
  sig_all <- merge(sigset2, sigset1, by="row.names")
  sigset2_new <- data.frame(sig_all[,2:(dim(sigset2)[2]+1)])

  sigset1_new <- sig_all[,(dim(sigset2)[2]+2):dim(sig_all)[2]]
  simi_matrix <- NULL
  for(i in 1:dim(sigset2_new)[2]){
    print(i)
    simi_m <- apply(sigset1_new,2,function(x) abs(cos_similarity(sigset2_new[,i],x)))
    simi_matrix <- rbind(simi_matrix, simi_m)
  }
  simi_matrix <- data.frame(simi_matrix)
  simi_matrix$sigset2 <- colnames(sigset2)
  simi_matrix_melt <- reshape2::melt(simi_matrix,c("sigset2"))
  names(simi_matrix_melt) <- c("sigset2","sigset1","similarity")
  cosmig_pos <- simi_matrix$sigset2
  write.table(simi_matrix,paste0(outputname,".txt"),sep = "\t",row.names = F, col.names = T, quote = F)

  if(ifplot==T){
    simi_matrix_melt$similarity <- round(simi_matrix_melt$similarity,2)
    pdf(file=paste0(outputname,".pdf"), onefile=TRUE,width = w,height = h)
    g <-ggplot2::ggplot(simi_matrix_melt, ggplot2::aes(y=factor(sigset1), x=factor(sigset2))) + geom_tile(ggplot2::aes(fill=similarity),colour="white")
    g <- g+ggplot2::xlab("set2")+ggplot2::ylab("set1")+ggplot2::scale_fill_gradient2(high="red", low="white",limits=c(0, 1)) #scale_fill_gradient2(low ="blue",mid = "white",high="red",space="Lab")
    g <- g+ggplot2::scale_x_discrete(limits = cosmig_pos)
    g <- g+ggplot2::geom_text(ggplot2::aes(label=paste(similarity)),size=text_size)
    g <- g+ggplot2::theme(axis.text.x=ggplot2::element_text(size=10, angle=90, vjust=0.9,colour="black"),
                 axis.text.y=ggplot2::element_text(size=10,colour = "black"),
                 axis.title.x = ggplot2::element_text(size=15),
                 axis.title.y = ggplot2::element_text(size=15),
                 plot.title = ggplot2::element_text(size=10),
                 panel.grid.minor.x=ggplot2::element_blank(),
                 panel.grid.major.x=ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "white"),
                 panel.border = ggplot2::element_rect(colour = "black", fill=NA))

    print(g)
    grDevices::dev.off()

  }
  # find the n non-overlap pairs with highest cosine similarity
  sigpairs <- NULL
  simi_matrix_melt_temp <- simi_matrix_melt
  while (dim(simi_matrix_melt_temp)[1]>0) {
    max_pair <- simi_matrix_melt_temp[simi_matrix_melt_temp$similarity==max(simi_matrix_melt_temp$similarity),]
    sigpairs <- rbind(sigpairs,max_pair)
    simi_matrix_melt_temp <- simi_matrix_melt_temp[!(simi_matrix_melt_temp$sigset2 %in% sigpairs$sigset2 | simi_matrix_melt_temp$sigset1 %in% sigpairs$sigset1),]
  }
  sigpairs <- sigpairs[order(sigpairs$similarity, decreasing = T),]
  write.table(sigpairs,paste0(outputname,"_similarpair.txt"),sep = "\t",row.names = F, col.names = T, quote = F)

  if(ifplot==T){
    sigpairs$similarity <- round(sigpairs$similarity,2)
    sigpairs$y2 <- seq(dim(sigpairs)[1], 1, by=-1)
    pdf(file=paste0(outputname,"_pair.pdf"), onefile=TRUE,width = w,height = h)
    p <-ggplot2::ggplot(sigpairs) + ggplot2::geom_segment(ggplot2::aes(x=1, xend=2, y=y2, yend=y2), size=.75, show.legend=F) +
      ggplot2::geom_vline(xintercept=1, linetype="dashed", size=.1) +
      ggplot2::geom_vline(xintercept=2, linetype="dashed", size=.1) + xlim(.5, 2.5) + ylim(0,(1.1*dim(sigpairs)[1]))
    p <- p + ggplot2::geom_text(label=sigpairs$sigset1, y=sigpairs$y2, x=rep(1, NROW(sigpairs)), hjust=1.1, size=3.5)
    p <- p + ggplot2::geom_text(label=sigpairs$sigset2, y=sigpairs$y2, x=rep(2, NROW(sigpairs)), hjust=-0.1, size=3.5)
    p <- p + ggplot2::geom_text(label=sigpairs$similarity, y=(sigpairs$y2+.05*dim(sigpairs)[1]), x=rep(1.5, NROW(sigpairs)), hjust=1.1, size=3.5)
    p <- p + ggplot2::geom_text(label="Set 1", x=1, y=1.1*dim(sigpairs)[1], hjust=1.2, size=5)  # title
    p <- p + ggplot2::geom_text(label="Set 2", x=2, y=1.1*dim(sigpairs)[1], hjust=-0.1, size=5)  # title
    p <- p+ ggplot2::theme_void()
    print(p)
    grDevices::dev.off()

  }

  #


  return(simi_matrix_melt)
}

