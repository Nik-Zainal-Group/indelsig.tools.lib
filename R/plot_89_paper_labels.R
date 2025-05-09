gen_plot_catalouge89_single_paper <- function (muts_basis, text_size, plot_title, plot_title_size = 8.5) 
{
  indel_type_4_figurelabel <- structure(list(IndelType = c("A[Ins(C):R0]A", 
                                                           "A[Ins(C):R0]T", "Ins(C):R(0,3)", "Ins(C):R(4,6)", "Ins(C):R(7,9)", 
                                                           "A[Ins(T):R(0,4)]A", "A[Ins(T):R(0,4)]C", "A[Ins(T):R(0,4)]G", 
                                                           "C[Ins(T):R(0,4)]A", "C[Ins(T):R(0,4)]C", "C[Ins(T):R(0,4)]G", 
                                                           "G[Ins(T):R(0,4)]A", "G[Ins(T):R(0,4)]C", "G[Ins(T):R(0,4)]G", 
                                                           "A[Ins(T):R(5,7)]A", "A[Ins(T):R(5,7)]C", "A[Ins(T):R(5,7)]G", 
                                                           "C[Ins(T):R(5,7)]A", "C[Ins(T):R(5,7)]C", "C[Ins(T):R(5,7)]G", 
                                                           "G[Ins(T):R(5,7)]A", "G[Ins(T):R(5,7)]C", "G[Ins(T):R(5,7)]G", 
                                                           "A[Ins(T):R(8,9)]A", "A[Ins(T):R(8,9)]C", "A[Ins(T):R(8,9)]G", 
                                                           "C[Ins(T):R(8,9)]A", "C[Ins(T):R(8,9)]C", "C[Ins(T):R(8,9)]G", 
                                                           "G[Ins(T):R(8,9)]A", "G[Ins(T):R(8,9)]C", "G[Ins(T):R(8,9)]G", 
                                                           "Ins(2,4):R0", "Ins(5,):R0", "Ins(2,4):R1", "Ins(5,):R1", 
                                                           "Ins(2,):R(2,4)", "Ins(2,):R(5,9)", "[Del(C):R1]A", "[Del(C):R1]T", 
                                                           "[Del(C):R2]A", "[Del(C):R2]T", "[Del(C):R3]A", "[Del(C):R3]T", 
                                                           "[Del(C):R(4,5)]A", "[Del(C):R(4,5)]T", "[Del(C):R(1,5)]G", 
                                                           "Del(C):R(6,9)", "A[Del(T):R(1,4)]A", "A[Del(T):R(1,4)]C", 
                                                           "A[Del(T):R(1,4)]G", "C[Del(T):R(1,4)]A", "C[Del(T):R(1,4)]C", 
                                                           "C[Del(T):R(1,4)]G", "G[Del(T):R(1,4)]A", "G[Del(T):R(1,4)]C", 
                                                           "G[Del(T):R(1,4)]G", "A[Del(T):R(5,7)]A", "A[Del(T):R(5,7)]C", 
                                                           "A[Del(T):R(5,7)]G", "C[Del(T):R(5,7)]A", "C[Del(T):R(5,7)]C", 
                                                           "C[Del(T):R(5,7)]G", "G[Del(T):R(5,7)]A", "G[Del(T):R(5,7)]C", 
                                                           "G[Del(T):R(5,7)]G", "A[Del(T):R(8,9)]A", "A[Del(T):R(8,9)]C", 
                                                           "A[Del(T):R(8,9)]G", "C[Del(T):R(8,9)]A", "C[Del(T):R(8,9)]C", 
                                                           "C[Del(T):R(8,9)]G", "G[Del(T):R(8,9)]A", "G[Del(T):R(8,9)]C", 
                                                           "G[Del(T):R(8,9)]G", "Del(2,4):R1", "Del(5,):R1", "Del(2,8):U(1,2):R(2,4)", 
                                                           "Del(2,):U(1,2):R(5,9)", "Del(3,):U(3,):R2", "Del(3,):U(3,):R(3,9)", 
                                                           "Del(2,5):M1", "Del(3,5):M2", "Del(4,5):M(3,4)", "Del(6,):M1", 
                                                           "Del(6,):M2", "Del(6,):M3", "Del(6,):M(4,)", "Complex"), 
                                             Indel = c("Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                                       "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                                       "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                                       "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                                       "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                                       "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                                       "Ins(T)", "Ins(T)", "Ins(2,)", "Ins(2,)", "Ins(2,)", 
                                                       "Ins(2,)", "Ins(2,)", "Ins(2,)", "Del(C)", "Del(C)", 
                                                       "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                                       "Del(C)", "Del(C)", "Del(C)", "Del(T)", "Del(T)", 
                                                       "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                                       "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                                       "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                                       "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                                       "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                                       "Del(2,):R(0,9)", "Del(2,):R(0,9)", "Del(2,):R(0,9)", 
                                                       "Del(2,):R(0,9)", "Del(2,):R(0,9)", "Del(2,):R(0,9)", 
                                                       "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
                                                       "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
                                                       "Del(2,):M(1,)", "Complex"), Indel3 = c("Insertion", 
                                                                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                                                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                                                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                                                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                                                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                                                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                                                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                                                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                                                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                                                                               "Insertion", "Deletion", "Deletion", "Deletion", 
                                                                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                                                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                                                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                                                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                                                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                                                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                                                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                                                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                                                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                                                                               "Deletion", "Deletion", "Complex"), Figlabel = c("A[C0]A", 
                                                                                                                                                "A[C0]T", "C(0,3)", "C(4,6)", "C(7,9)", "A[T(0,4)]A", 
                                                                                                                                                "A[T(0,4)]C", "A[T(0,4)]G", "C[T(0,4)]A", "C[T(0,4)]C", 
                                                                                                                                                "C[T(0,4)]G", "G[T(0,4)]A", "G[T(0,4)]C", "G[T(0,4)]G", 
                                                                                                                                                "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)[G", "C[T(5,7)[A", 
                                                                                                                                                "C[T(5,7)[C", "C[T(5,7)[G", "G[T(5,7)[A", "G[T(5,7)[C", 
                                                                                                                                                "G[T(5,7)]G", "A[T(8,9)]A", "A[T(8,9)]C", "A[T(8,9)]G", 
                                                                                                                                                "C[T(8,9)]A", "C[T(8,9)]C", "C[T(8,9)]G", "G[T(8,9)]A", 
                                                                                                                                                "G[T(8,9)]C", "G[T(8,9)]G", "L(2,4):R0", "L(5, ):R0", 
                                                                                                                                                "L(2,4):R1", "L(5, ):R1", "L(2, ):R(2,4)", "L(2, ):R(5,9)", 
                                                                                                                                                "[C1]A", "[C1]T", "[C2]A", "[C2]T", "[C3]A", "[C3]T", 
                                                                                                                                                "[C(4,5)]A", "[C(4,5)]T", "[C(1,5)]G", "C(6,9)", 
                                                                                                                                                "A[T(1,4)]A", "A[T(1,4)]C", "A[T(1,4)]G", "C[T(1,4)]A", 
                                                                                                                                                "C[T(1,4)]C", "C[T(1,4)]G", "G[T(1,4)]A", "G[T(1,4)]C", 
                                                                                                                                                "G[T(1,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)]G", 
                                                                                                                                                "C[T(5,7)]A", "C[T(5,7)]C", "C[T(5,7)]G", "G[T(5,7)]A", 
                                                                                                                                                "G[T(5,7)]C", "G[T(5,7)]G", "A[T(8,9)]A", "A[T(8,9)]C", 
                                                                                                                                                "A[T(8,9)]G", "C[T(8,9)]A", "C[T(8,9)]C", "C[T(8,9)]G", 
                                                                                                                                                "G[T(8,9)]A", "G[T(8,9)]C", "G[T(8,9)]G", "L(2,4):R1", 
                                                                                                                                                "L(5, ):R1", "L(2,8):U(1,2):R(2,4)", "L(2, ):U(1,2):R(5,9)", 
                                                                                                                                                "L(3, ):U(3,):R2", "L(3, ):U(3,):R(3,9)", "L(2,5):M1", 
                                                                                                                                                "L(3,5):M2", "L(4,5):M(3,4)", "L(6, ):M1", "L(6, ):M2", 
                                                                                                                                                "L(6, ):M3", "L(6, ):M(4, )", "Complex")), class = "data.frame", 
                                        row.names = c(NA, -89L))
  
  
  
  muts_basis_melt <- reshape2::melt(muts_basis, "IndelType")
  muts_basis_melt <- merge(indel_type_4_figurelabel, muts_basis_melt, 
                           by = "IndelType", all.x = T)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType", "Indel", "Indel3", 
                              "Figlabel", "Sample", "freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)
  indel_mypalette_fill <- c("#000000", "#762A83", "#EE6677", 
                            "#004488", "#997700", "#EE99AA", "#6699CC", "#EECC66")
  indel_positions <- indel_type_4_figurelabel$IndelType
  indel_positions_labels <- indel_type_4_figurelabel$Figlabel
  entry <- table(indel_type_4_figurelabel$Indel)
  order_entry <- c("Ins(C)", "Ins(T)", "Ins(2,)", "Del(C)", 
                   "Del(T)", "Del(2,):R(0,9)", "Del(2,):M(1,)", "Complex")
  entry <- entry[order_entry]
  blocks <- data.frame(Type = unique(indel_type_4_figurelabel$Indel), 
                       fill = indel_mypalette_fill, xmin = c(0, cumsum(entry)[-length(entry)]) + 
                         0.5, xmax = cumsum(entry) + 0.5)
  blocks$ymin <- -max(muts_basis_melt$freq) * 0.06
  blocks$ymax <- 0
  blocks$labels <- c("1bp C", "1bp T", ">=2bp", "1bp C", "1bp T", 
                     ">=2bp", "Mh", "X")
  blocks$cl <- c("black", "black", "black", "white", "white", 
                 "white", "white", "white")
  indel_mypalette_fill3 <- c("#000000", "#888888", "#DDDDDD")
  entry3 <- table(indel_type_4_figurelabel$Indel3)
  order_entry3 <- c("Insertion", "Deletion", "Complex")
  entry3 <- entry3[order_entry3]
  blocks3 <- data.frame(Type = unique(indel_type_4_figurelabel$Indel3), 
                        fill = indel_mypalette_fill3, xmin = c(0, cumsum(entry3)[-length(entry3)]) + 
                          0.5, xmax = cumsum(entry3) + 0.5)
  blocks3$ymin <- max(muts_basis_melt$freq) * 1.08
  blocks3$ymax <- max(muts_basis_melt$freq) * 1.14
  blocks3$labels <- c("Insertion", "Deletion", "X")
  blocks3$cl <- c("black", "white", "white")
  indel_mypalette_fill_all <- c("#000000", "#762A83", "#EE3377", 
                                "#004488", "#997700", "#888888", "#EE99AA", "#6699CC", 
                                "#EECC66", "#DDDDDD")
  p <- ggplot2::ggplot(data = muts_basis_melt, ggplot2::aes(x = IndelType, 
                                                            y = freq, fill = Indel)) + ggplot2::geom_bar(stat = "identity", 
                                                                                                         position = "dodge", width = 0.7)
  p <- p + ggplot2::scale_x_discrete(limits = indel_positions, 
                                     labels = indel_positions_labels, expand = c(0,0))
  p <- p + ggplot2::scale_fill_manual(values = indel_mypalette_fill_all) + 
    ggplot2::scale_y_continuous(limits = c(unique(blocks$ymin), 
                                           unique(blocks3$ymax)), labels = scales::number_format(accuracy = 0.01), 
                                expand = c(0, 0))
  p <- p + ggplot2::theme_classic() + ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                                                     axis.text.y = ggplot2::element_text(size = 10, colour = "black"), 
                                                     axis.ticks.x = ggplot2::element_blank(), axis.line.x = ggplot2::element_blank(), 
                                                     legend.position = "none", axis.title.y = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(),  plot.title = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), panel.border = element_rect(fill = NA, colour = "black"))
  p <- p + ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin = xmin, 
                                                          ymin = ymin, xmax = xmax, ymax = ymax, fill = Type), colour = "white", 
                              inherit.aes = F, size = 0.1) 
  p <- p + ggplot2::geom_rect(data = blocks3, ggplot2::aes(xmin = xmin, 
                                                           ymin = ymin, xmax = xmax, ymax = ymax, fill = Type), colour = "white", 
                              inherit.aes = F, size = 0.1) +  ggplot2::annotate("label", label = plot_title, x=0.5, y = max(muts_basis_melt$freq)  , vjust = 0.7, hjust = 0, size = plot_title_size, fill = "transparent", col = "black", fontface = "bold", label.size = 0)
  return(p)
}





plot_indels_rsigs_label_multiple <-  function (muts_basis, colnum, h, w, text_size, print_Xlabel = T,paper_version=F, 
                                               outputname, toadd=NULL, overall_title=NULL, print_pdf=T, print_png=F, annotation=NULL, ann_size=7, mlp=0) 
{
  cnames <- names(muts_basis)
  muts_basis2 <- muts_basis
  if (length(cnames) > 1) {
    p_all <- list()
    for (i in 1:dim(muts_basis2)[2]) {
      plottitle <- paste(names(muts_basis2)[i], ifelse(!is.null(toadd), toadd[i], ""))
      if(!paper_version){
        if (print_Xlabel == T) {
          
          p <- gen_plot_catalouge89_single(data.frame(Sample = muts_basis2[,i], IndelType = rownames(muts_basis2)), text_size,plottitle)
          
        }else{
          p <- gen_plot_catalouge89_single_noXlabel(data.frame(Sample = muts_basis2[,i], IndelType = rownames(muts_basis2)), text_size,plottitle)
        }
      }else{
        p <- gen_plot_catalouge89_single_paper(data.frame(Sample = muts_basis2[,i], IndelType = rownames(muts_basis2)), text_size,plottitle)
      }
      if(!is.null(annotation)){
        if(nrow(annotation[[i]]) == 1){
          p <- p + ggplot2::annotate("label", label = annotation[[i]][1, "label"], x=88.8, y = max(as.numeric(muts_basis2[,i])), vjust = 0.7, hjust = 1, size = ann_size, fill = annotation[[i]][1, "fill"], col = annotation[[i]][1, "colour"], fontface = "bold", label.r = unit(10, "pt"), alpha = 0.8, label.size = 0.25)
        }else{
          y_calc = max(as.numeric(muts_basis2[,i])) * seq(1, by = -mlp, length.out = nrow(annotation[[i]]))
          for(j in 1:nrow(annotation[[i]])){
            
            p <- p + ggplot2::annotate("label", label = annotation[[i]][j, "label"], x=88.8, y = y_calc[j]  , vjust = 0.7, hjust = 1, size = ann_size, fill = annotation[[i]][j, "fill"], col = annotation[[i]][j, "colour"], fontface = "bold", label.r = unit(10, "pt"), alpha = 0.8, label.size = 0.25)
            
          }  
        }
        
      }
      p_all[[length(p_all) + 1]] <- p
    }
    if (is.null(outputname) == F) {
      
      
      
      if(print_pdf){
        filename <- paste0(outputname, ".pdf")
        grDevices::pdf(file = filename, onefile = TRUE, width = w, 
                       height = h)
        do.call("grid.arrange", c(p_all, ncol = colnum, top = overall_title))
        grDevices::dev.off()
      }
      
      if(print_png){
        filename <- paste0(outputname, ".png")
        grDevices::png(file = filename, width = w, 
                       height = h, units = "in", res = 120)
        do.call("grid.arrange", c(p_all, ncol = colnum, top = overall_title))
        grDevices::dev.off()
      }
      
      
    }
    else {
      return(do.call("grid.arrange", c(p_all, ncol = colnum, top = overall_title)))
    }
  }
  else {
    plottitle <- paste(names(muts_basis2)[1], ifelse(!is.null(toadd), toadd[i], ""))
    p_all <- list()
    if(!paper_version){
      if (print_Xlabel == T) {
        p <- gen_plot_catalouge89_single(data.frame(Sample = muts_basis2[,1], IndelType = rownames(muts_basis2)), text_size, plottitle)
      }
      else {
        p <- gen_plot_catalouge89_single_noXlabel(data.frame(Sample = muts_basis2[,1], IndelType = rownames(muts_basis2)), text_size,plottitle)
      }
    }else{
      
      p <- gen_plot_catalouge89_single_paper(data.frame(Sample = muts_basis2[,1], IndelType = rownames(muts_basis2)), text_size,plottitle)
      
    }
    if(!is.null(annotation)){
      if(nrow(annotation[[1]]) == 1){
        p <- p + ggplot2::annotate("label", label = annotation[[1]][1, "label"], x=88.8, y = max(as.numeric(muts_basis2[,1])), vjust = 0.7, hjust = 1, label.size= 0.25, size = ann_size, col = annotation[[1]][1, "colour"],fill= annotation[[1]][1, "fill"], fontface = "bold", label.r = unit(10, "pt"), alpha = 0.8)
      }else{
        
        y_calc = max(as.numeric(muts_basis2[,1])) * seq(1, by = -mlp, length.out = nrow(annotation[[1]]))
        
        for(j in 1:nrow(annotation[[1]])){
          
          p <- p + ggplot2::annotate("label", label = annotation[[1]][j, "label"], x=88.8, y = y_calc[j]  , vjust = 0.7, hjust = 1, size = ann_size, fill = annotation[[1]][j, "fill"], col = annotation[[1]][j, "colour"], fontface = "bold", label.r = unit(10, "pt"), alpha = 0.8, label.size=0.25)
          
        }  
        
        
      }
      
      
      
    }
    p_all[[length(p_all) + 1]] <- p
    if (is.null(outputname) == F) {
      
      
      if(print_pdf){
        filename <- paste0(outputname, ".pdf")
        grDevices::pdf(file = filename, onefile = TRUE, width = w, 
                       height = h)
        if(!is.null(overall_title)){
          do.call("grid.arrange", c(p_all, ncol = colnum, top = overall_title))
        }else{
          do.call("grid.arrange", c(p_all, ncol = colnum))
        }
        grDevices::dev.off()
        
        
      }    
      
      
      
      if(print_png){
        filename <- paste0(outputname, ".png")
        grDevices::png(file = filename, width = w, 
                       height = h, units = "in", res = 120)
        if(!is.null(overall_title)){
          do.call("grid.arrange", c(p_all, ncol = colnum, top = overall_title))
        }else{
          do.call("grid.arrange", c(p_all, ncol = colnum))
        }
        grDevices::dev.off()
        
        
      }
      
      
      
    }
    else {
      if(!is.null(overall_title)){
        
        return(do.call("grid.arrange", c(p_all, ncol = colnum,top = overall_title)))
      
      }else{
        
        return(do.call("grid.arrange", c(p_all, ncol = colnum)))
      }
      
      return(do.call("grid.arrange", c(p_all, ncol = colnum,top = overall_title)))
    }
  }
} 
