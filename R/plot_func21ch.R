#' Plot indel profile in a 21-channel bar plot for single sample
#'
#' @param muts_basis A indel catalogue of a single sample
#' @param text_size Size of text
#' @param plot_title Title of the plot
#' @return A 21-channel indel profile plot
#' @export
gen_plot_catalouge21_single <- function(muts_basis,text_size,plot_title){
  indel_template_type_3 <- data.frame("IndelType"=c("[InsC]NonRep","[InsC]ShortRep_leq4","[InsC]LongRep_g4","[InsT]NonRep","[InsT]ShortRep_leq4","[InsT]LongRep_g4","Ins_NonRep","Ins_nMer_ShortRep_leq4","Ins_nMer_LongRep_g4",
                                                    "[DelC]NonRep","[DelC]ShortRep_leq4","[DelC]LongRep_g4","[DelT]NonRep","[DelT]ShortRep_leq4","[DelT]LongRep_g4","Del_NonRep","Del_nMer_ShortRep_leq4","Del_nMer_LongRep_g4","Del_Spaced_short_leq5","Del_Spaced_long_g5",
                                                    "Complex"),
                                      "Indel"=c("Ins(C)","Ins(C)","Ins(C)","Ins(T)","Ins(T)","Ins(T)","Ins(2,)","Ins(2,)","Ins(2,)",
                                                 "Del(C)","Del(C)","Del(C)","Del(T)","Del(T)","Del(T)","Del(2,):M(1,)","Del(2,):M(1,)","Del(2,):R(0,9)","Del(2,):R(0,9)","Del(2,):R(0,9)",
                                                 "Complex")
                                      )

  muts_basis_melt <- reshape2::melt(muts_basis,"IndelType")

  muts_basis_melt <- merge(indel_template_type_3, muts_basis_melt,by="IndelType",all.x=T)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType","Indel","Sample","freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)


  # indel_mypalette_fill <- c("skyblue","orange", "blue","tomato","greenyellow","pink","grey","purple","deeppink","black")
 # indel_mypalette_fill <- c("#06C2F4", # [-C]
  #                           "#FF8642", # [-T]
  #                           "royalblue", #"#007996"  [+C]
  #                           "#C0362C", # [+T]
  #                           "#000000", # FEABB9 Complex
  #                           "#B1DDA1", # Del_nMer
  #                          "#C3B7AC", # Del_NonRep
  #                          "#CF97D7", # Del_Spaced
  #                          "limegreen", #"#668D3C"  Ins_nMer
  #                          "#816C5B")  # Ins_NonRep
  indel_mypalette_fill <- c("#000000", # FEABB9 Complex
                            "#762A83", # Del(2,):M(1,)
                            "#EE3377", # Del(2,):R(0,9) #EE6677
                            "#004488", # Del(C)
                            "#997700", # Del(T)
                            "#EE99AA", #"#668D3C"  Ins(2,)
                            "#6699CC", #"#007996"  Ins(C)
                            "#EECC66") # Ins(T)

  indel_positions <- indel_template_type_3$IndelType
  entry <- table(indel_template_type_3$Indel)
 # order_entry <- c("[+C]", "[+T]", "Ins_nMer", "Ins_NonRep", "[-C]", "[-T]", "Del_nMer", "Del_NonRep", "Del_Spaced", "Complex")
  order_entry <- c("Ins(C)", "Ins(T)", "Ins(2,)", "Del(C)", "Del(T)", "Del(2,):R(0,9)", "Del(2,):M(1,)", "Complex")

  entry <- entry[order_entry]
  blocks <- data.frame(Type=unique(indel_template_type_3$Indel),
                       fill=indel_mypalette_fill,
                       xmin=c(0,cumsum(entry)[-length(entry)])+0.5,
                       xmax=cumsum(entry)+0.5)
  blocks$ymin <- max(muts_basis_melt$freq)*1.08#
  blocks$ymax <- max(muts_basis_melt$freq)*1.2
#  blocks$labels <-c("+C", "+T", "+M", "+N", "-C", "-T", "-M", "-N", "-Mh", "X")
#  blocks$cl <-c("white", "white", "white", "white", "black", "black", "black", "black", "black", "white")

  blocks$labels <-c("1bp C", "1bp T", ">=2bp", "1bp C", "1bp T", ">=2bp", "Mh", "X")
  blocks$cl <-c("black", "black", "black", "white", "white", "white", "white",  "white")


  p <- ggplot2::ggplot(data=muts_basis_melt, ggplot2::aes(x=IndelType, y=freq,fill=Indel))+ ggplot2::geom_bar(stat="identity",position="dodge", width=.7)+ggplot2::xlab("Indel Types")+ggplot2::ylab("Count")
  #  p <- p+scale_y_continuous(limits=c(0,40),breaks=(seq(0,40,10)))
  p <- p+ggplot2::scale_x_discrete(limits = indel_positions)+ ggplot2::ggtitle(plot_title)
  p <- p+ggplot2::scale_fill_manual(values=indel_mypalette_fill)+ggplot2::coord_cartesian(ylim=c(0,unique(blocks$ymax)), expand = FALSE)
  p <- p+ggplot2::theme_classic()+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, size=5,colour = "black",hjust=1),
                                                 axis.text.y=ggplot2::element_text(size=10,colour = "black"),
                                                 #    axis.line.y=element_blank(),
                                                 legend.position = "none",
                                                 axis.title.x = ggplot2::element_text(size=15),
                                                 axis.title.y = ggplot2::element_text(size=15))
  ## Add the overhead blocks

  p <- p+ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type),inherit.aes = F)+
    # geom_text(data=blocks,aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels),size=text_size,inherit.aes = F,colour="white")
    ggplot2::geom_text(data=blocks,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)+ggplot2::scale_colour_manual(values=c("black", "white"))

  return(p)



}


#' Plot indel profile in a 21-channel bar plot, original plots_type_4_m4_89 function
#'
#' @param muts_basis A indel catalogue of multiple samples
#' @param colnum Number of columns
#' @param h Hight of the plot
#' @param w Width of the plot
#' @param text_size Size of text
#' @param outputname Output file name of the plot
#' @return A plot including 21-channel indel profile of multiple samples
#' @import gridExtra
#' @export
plots_indelprofile_21ch<- function(muts_basis,colnum, h,w,text_size,outputname){

  muts_basis2 <- muts_basis[,names(muts_basis) != "IndelType"]
  p_all <- list()
  for(i in 1:dim(muts_basis2)[2]){

    p <- gen_plot_catalouge21_single(data.frame("Sample"=muts_basis2[,i],"IndelType"=rownames(muts_basis2)), text_size,names(muts_basis2)[i])
    p_all[[length(p_all)+1]] <- p

  }

  filename <- paste0(outputname, ".pdf")
  grDevices::pdf(file=filename, onefile=TRUE,width=w,height=h)

  do.call("grid.arrange", c(p_all, ncol = colnum))


  grDevices::dev.off()

}
