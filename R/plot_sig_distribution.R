#' Plot percentage of signature in each sample for a given cohort
#'
#' @param sample_sig_count A data frame shows the number of mutations of signatures in each sample
#' @param SampleCol Sample column name
#' @param h Hight of the plot
#' @param w Width of the plot
#' @param text_size Size of text
#' @param outputname Output file name of the plot
#' @return A bar plot of signature distribution (percentage) in each sample
#' @export
plot_sig_percentage <- function(sample_sig_count, SampleCol, h,w,text_size,outputname){

  sample_sig_per_melt <- reshape2::melt(sample_sig_per,id.vars=SampleCol)
  names(sample_sig_per_melt) <- c("Sample", "Sig", "Count")
  brks <- c(0, 0.25, 0.5, 0.75, 1)
  grDevices::pdf(file=paste0(outputname,".pdf"), onefile=TRUE,width=w,height=h)
  p <- ggplot2::ggplot(data=sample_sig_per_melt, ggplot2::aes(x=Sample, y=Count,fill=Sig))+ ggplot2::geom_bar(stat="identity", position = "fill")+ggplot2::xlab("Sample")+ggplot2::ylab("Percentage")
  p <- p+scale_y_continuous(breaks = brks, labels = scales::percent(brks))
  p <- p+ggplot2::ggtitle(outputname)
#  p <- p+ggplot2::scale_fill_manual(values=indel_mypalette_fill)
  p <- p+ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                        axis.text.y=ggplot2::element_text(size=10,colour = "black"),
                        axis.title.x = ggplot2::element_text(size=15),
                        axis.title.y = ggplot2::element_text(size=15),
                        plot.title = ggplot2::element_text(size=14),
                        panel.grid.minor.x=ggplot2::element_blank(),
                        panel.grid.major.x=ggplot2::element_blank(),
                        panel.grid.major.y = ggplot2::element_blank(),
                        panel.grid.minor.y = ggplot2::element_blank(),
                        panel.background = ggplot2::element_rect(fill = "white"),
                        panel.border = ggplot2::element_rect(colour = "black", fill=NA))
  print(p)
  grDevices::dev.off()
}

#' Plot Count of signature in each sample for a given cohort
#'
#' @param sample_sig_count A data frame shows the number of mutations of signatures in each sample
#' @param SampleCol Sample column name
#' @param h Hight of the plot
#' @param w Width of the plot
#' @param text_size Size of text
#' @param outputname Output file name of the plot
#' @return A bar plot of signature distribution (Count) in each sample
#' @export
plot_sig_count <- function(sample_sig_count, SampleCol, h,w,text_size,outputname){

  sample_sig_per_melt <- reshape2::melt(sample_sig_per,id.vars=SampleCol)
  names(sample_sig_per_melt) <- c("Sample", "Sig", "Count")
  grDevices::pdf(file=paste0(outputname,".pdf"), onefile=TRUE,width=w,height=h)
  p <- ggplot2::ggplot(data=sample_sig_per_melt, ggplot2::aes(x=Sample, y=Count,fill=Sig))+ ggplot2::geom_bar(stat="identity")+ggplot2::xlab("Sample")+ggplot2::ylab("Count")
  #  p <- p+scale_y_continuous(limits=c(0,40),breaks=(seq(0,40,10)))
  p <- p+ ggplot2::ggtitle(outputname)
  p <- p+ggplot2::scale_fill_manual(values=indel_mypalette_fill)+ggplot2::coord_cartesian(ylim=c(0,unique(blocks$ymax)), expand = FALSE)
  p <- p+ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                        axis.text.y=ggplot2::element_text(size=10,colour = "black"),
                        axis.title.x = ggplot2::element_text(size=15),
                        axis.title.y = ggplot2::element_text(size=15),
                        plot.title = ggplot2::element_text(size=14),
                        panel.grid.minor.x=ggplot2::element_blank(),
                        panel.grid.major.x=ggplot2::element_blank(),
                        panel.grid.major.y = ggplot2::element_blank(),
                        panel.grid.minor.y = ggplot2::element_blank(),
                        panel.background = ggplot2::element_rect(fill = "white"),
                        panel.border = ggplot2::element_rect(colour = "black", fill=NA))

  print(p)
  grDevices::dev.off()
}
