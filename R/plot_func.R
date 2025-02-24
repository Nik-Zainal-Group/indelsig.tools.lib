
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Old function to plot indel profile/signature in a 89-channel bar plot for single sample
#'
#' @param muts_basis A indel catalogue of a single sample
#' @param text_size Size of text
#' @param plot_title Title of the plot
#' @return A 89-channel indel profile plot
#' @export
gen_plot_catalouge89_single_old<- function(muts_basis,text_size,plot_title){
  #mean_parentmuts <- sum(muts_basis[,1:(dim(muts_basis)[2]-1)])/(dim(muts_basis)[2]-1)
  indel_template_type_4 <- data.frame("IndelType"=c("A|[+C]Rep=0|A","A|[+C]Rep=0|T","[+C]Rep_leq3","[+C]Rep_456","[+C]Rep_789",


                                                    "A|[+T]Rep_leq4|A","A|[+T]Rep_leq4|C","A|[+T]Rep_leq4|G","C|[+T]Rep_leq4|A","C|[+T]Rep_leq4|C","C|[+T]Rep_leq4|G","G|[+T]Rep_leq4|A","G|[+T]Rep_leq4|C","G|[+T]Rep_leq4|G",
                                                    "A|[+T]Rep_567|A","A|[+T]Rep_567|C","A|[+T]Rep_567|G","C|[+T]Rep_567|A","C|[+T]Rep_567|C","C|[+T]Rep_567|G","G|[+T]Rep_567|A","G|[+T]Rep_567|C","G|[+T]Rep_567|G",
                                                    "A|[+T]Rep_89|A","A|[+T]Rep_89|C","A|[+T]Rep_89|G","C|[+T]Rep_89|A","C|[+T]Rep_89|C","C|[+T]Rep_89|G","G|[+T]Rep_89|A","G|[+T]Rep_89|C","G|[+T]Rep_89|G",

                                                    "Ins_NonRep_R0_L234","Ins_NonRep_R0_L5","Ins_NonRep_R1_L234","Ins_NonRep_R1_L5",

                                                    "Ins_nMer_R234","Ins_nMer_R5",

                                                    "[-C]Rep=1|A","[-C]Rep=1|T",
                                                    "[-C]Rep=2|A","[-C]Rep=2|T",
                                                    "[-C]Rep=3|A","[-C]Rep=3|T",
                                                    "[-C]Rep_45|A","[-C]Rep_45|T",
                                                    "[-C]Rep_leq5|G",
                                                    "[-C]Rep_6",

                                                    "A|[-T]Rep_leq4|A","A|[-T]Rep_leq4|C","A|[-T]Rep_leq4|G","C|[-T]Rep_leq4|A","C|[-T]Rep_leq4|C","C|[-T]Rep_leq4|G","G|[-T]Rep_leq4|A","G|[-T]Rep_leq4|C","G|[-T]Rep_leq4|G",
                                                    "A|[-T]Rep_567|A","A|[-T]Rep_567|C","A|[-T]Rep_567|G","C|[-T]Rep_567|A","C|[-T]Rep_567|C","C|[-T]Rep_567|G","G|[-T]Rep_567|A","G|[-T]Rep_567|C","G|[-T]Rep_567|G",
                                                    "A|[-T]Rep_89|A","A|[-T]Rep_89|C","A|[-T]Rep_89|G","C|[-T]Rep_89|A","C|[-T]Rep_89|C","C|[-T]Rep_89|G","G|[-T]Rep_89|A","G|[-T]Rep_89|C","G|[-T]Rep_89|G",

                                                    "Del_NonRep_L234","Del_NonRep_L5",
                                                    "Del_nMer_U12_R234","Del_nMer_U12_R5","Del_nMer_U3_R2","Del_nMer_U3_R3",
                                                    "Del_Spaced_short_leq5_mh1","Del_Spaced_short_leq5_mh2","Del_Spaced_short_leq5_mh3",
                                                    "Del_Spaced_long_g5_mh1","Del_Spaced_long_g5_mh2", "Del_Spaced_long_g5_mh3","Del_Spaced_long_g5_mh4",
                                                    "Complex"),

                                      "Indel"=c("[+C]","[+C]","[+C]","[+C]","[+C]",


                                                "[+T]","[+T]","[+T]","[+T]","[+T]","[+T]","[+T]","[+T]","[+T]",
                                                "[+T]","[+T]","[+T]","[+T]","[+T]","[+T]","[+T]","[+T]","[+T]",
                                                "[+T]","[+T]","[+T]","[+T]","[+T]","[+T]","[+T]","[+T]","[+T]",

                                                "Ins_NonRep","Ins_NonRep","Ins_NonRep","Ins_NonRep",
                                                "Ins_nMer","Ins_nMer",

                                                "[-C]","[-C]","[-C]","[-C]","[-C]","[-C]","[-C]","[-C]",
                                                "[-C]","[-C]",


                                                "[-T]","[-T]","[-T]","[-T]","[-T]","[-T]","[-T]","[-T]","[-T]",
                                                "[-T]","[-T]","[-T]","[-T]","[-T]","[-T]","[-T]","[-T]","[-T]",
                                                "[-T]","[-T]","[-T]","[-T]","[-T]","[-T]","[-T]","[-T]","[-T]",


                                                "Del_NonRep","Del_NonRep",
                                                "Del_nMer","Del_nMer","Del_nMer","Del_nMer",
                                                "Del_Spaced","Del_Spaced","Del_Spaced","Del_Spaced","Del_Spaced","Del_Spaced","Del_Spaced",
                                                "Complex")
  )

  muts_basis_melt <- reshape2::melt(muts_basis,"IndelType")

  muts_basis_melt <- merge(indel_template_type_4, muts_basis_melt,by="IndelType",all.x=T)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType","Indel","Sample","freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)


  # indel_mypalette_fill <- c("skyblue","orange", "blue","tomato","greenyellow","pink","grey","purple","deeppink","black")
  indel_mypalette_fill <- c("#06C2F4", # [-C]
                            "#FF8642", # [-T]
                            "#007996", # [+C]
                            "#C0362C", # [+T]
                            "#FEABB9", # Complex
                            "#B1DDA1", # Del_nMer
                            "#C3B7AC", # Del_NonRep
                            "#91278F", # Del_Spaced
                            "#668D3C", # Ins_nMer
                            "#816C5B")   # Ins_NonRep

  indel_positions <- indel_template_type_4$IndelType
  entry <- table(indel_template_type_4$Indel)
  order_entry <- c("[+C]", "[+T]", "Ins_NonRep", "Ins_nMer", "[-C]", "[-T]", "Del_NonRep", "Del_nMer", "Del_Spaced", "Complex")
  entry <- entry[order_entry]
  blocks <- data.frame(Type=unique(indel_template_type_4$Indel),
                       fill=indel_mypalette_fill,
                       xmin=c(0,cumsum(entry)[-length(entry)])+0.5,
                       xmax=cumsum(entry)+0.5)
  blocks$ymin <- max(muts_basis_melt$freq)*1.08
  blocks$ymax <- max(muts_basis_melt$freq)*1.2
  blocks$labels <-c("+C", "+T", "+N", "+M", "-C", "-T", "-N", "-M", "-Mh", "X")


  p <- ggplot2::ggplot(data=muts_basis_melt, ggplot2::aes(x=IndelType, y=freq,fill=Indel))+ ggplot2::geom_bar(stat="identity",position="dodge", width=.7)+ggplot2::xlab("Indel Types")+ggplot2::ylab("Count")
  p <- p+ggplot2::scale_x_discrete(limits = indel_positions)+ ggplot2::scale_y_continuous(labels=scales::percent)+ggplot2::ggtitle(plot_title)
  p <- p+ggplot2::scale_fill_manual(values=indel_mypalette_fill)
  p <- p+ggplot2::theme_classic()+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, size=5,colour = "black",hjust=1),
                               axis.text.y=ggplot2::element_text(size=10,colour = "black"),
                               axis.line.y=ggplot2::element_blank(),
                               legend.position = "none",
                               axis.title.x = ggplot2::element_text(size=15),
                               axis.title.y = ggplot2::element_text(size=15))
  ## Add the overhead blocks

  p <- p+ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type),inherit.aes = F)+
    ggplot2::geom_text(data=blocks,ggplot2::aes(x=(xmax+xmin)/2,y=ymax*1.1,label=labels),size=text_size,inherit.aes = F)

  return(p)


  # return(muts_basis)
}

#' Plot indel signature in a 89-channel bar plot for single sample
#'
#' @param muts_basis A indel catalogue of a single sample
#' @param text_size Size of text
#' @param plot_title Title of the plot
#' @return A 89-channel indel signature plot
#' @export
gen_plot_catalouge89_single_percentage<- function(muts_basis,text_size,plot_title){
  #mean_parentmuts <- sum(muts_basis[,1:(dim(muts_basis)[2]-1)])/(dim(muts_basis)[2]-1)
  indel_type_4_figurelabel <- structure(list(IndelType = c("A[Ins(C):R0]A", "A[Ins(C):R0]T", 
"Ins(C):R(0,3)", "Ins(C):R(4,6)", "Ins(C):R(7,9)", "A[Ins(T):R(0,4)]A", 
"A[Ins(T):R(0,4)]C", "A[Ins(T):R(0,4)]G", "C[Ins(T):R(0,4)]A", 
"C[Ins(T):R(0,4)]C", "C[Ins(T):R(0,4)]G", "G[Ins(T):R(0,4)]A", 
"G[Ins(T):R(0,4)]C", "G[Ins(T):R(0,4)]G", "A[Ins(T):R(5,7)]A", 
"A[Ins(T):R(5,7)]C", "A[Ins(T):R(5,7)]G", "C[Ins(T):R(5,7)]A", 
"C[Ins(T):R(5,7)]C", "C[Ins(T):R(5,7)]G", "G[Ins(T):R(5,7)]A", 
"G[Ins(T):R(5,7)]C", "G[Ins(T):R(5,7)]G", "A[Ins(T):R(8,9)]A", 
"A[Ins(T):R(8,9)]C", "A[Ins(T):R(8,9)]G", "C[Ins(T):R(8,9)]A", 
"C[Ins(T):R(8,9)]C", "C[Ins(T):R(8,9)]G", "G[Ins(T):R(8,9)]A", 
"G[Ins(T):R(8,9)]C", "G[Ins(T):R(8,9)]G", "Ins(2,4):R0", "Ins(5,):R0", 
"Ins(2,4):R1", "Ins(5,):R1", "Ins(2,):R(2,4)", "Ins(2,):R(5,9)", 
"[Del(C):R1]A", "[Del(C):R1]T", "[Del(C):R2]A", "[Del(C):R2]T", 
"[Del(C):R3]A", "[Del(C):R3]T", "[Del(C):R(4,5)]A", "[Del(C):R(4,5)]T", 
"[Del(C):R(1,5)]G", "Del(C):R(6,9)", "A[Del(T):R(1,4)]A", "A[Del(T):R(1,4)]C", 
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
"Del(6,):M2", "Del(6,):M3", "Del(6,):M(4,)", "Complex"), Indel = c("Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(2,)", "Ins(2,)", "Ins(2,)", 
"Ins(2,)", "Ins(2,)", "Ins(2,)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(2,):R(0,9)", 
"Del(2,):R(0,9)", "Del(2,):R(0,9)", "Del(2,):R(0,9)", "Del(2,):R(0,9)", 
"Del(2,):R(0,9)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Complex"), Indel3 = c("Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Complex"), Figlabel = c("A[C0]A", "A[C0]T", 
"C(0,3)", "C(4,6)", "C(7,9)", "A[T(0,4)]A", "A[T(0,4)]C", "A[T(0,4)]G", 
"C[T(0,4)]A", "C[T(0,4)]C", "C[T(0,4)]G", "G[T(0,4)]A", "G[T(0,4)]C", 
"G[T(0,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)[G", "C[T(5,7)[A", 
"C[T(5,7)[C", "C[T(5,7)[G", "G[T(5,7)[A", "G[T(5,7)[C", "G[T(5,7)]G", 
"A[T(8,9)]A", "A[T(8,9)]C", "A[T(8,9)]G", "C[T(8,9)]A", "C[T(8,9)]C", 
"C[T(8,9)]G", "G[T(8,9)]A", "G[T(8,9)]C", "G[T(8,9)]G", "L(2,4):R0", 
"L(5, ):R0", "L(2,4):R1", "L(5, ):R1", "L(2, ):R(2,4)", "L(2, ):R(5,9)", 
"[C1]A", "[C1]T", "[C2]A", "[C2]T", "[C3]A", "[C3]T", "[C(4,5)]A", 
"[C(4,5)]T", "[C(1,5)]G", "C(6,9)", "A[T(1,4)]A", "A[T(1,4)]C", 
"A[T(1,4)]G", "C[T(1,4)]A", "C[T(1,4)]C", "C[T(1,4)]G", "G[T(1,4)]A", 
"G[T(1,4)]C", "G[T(1,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)]G", 
"C[T(5,7)]A", "C[T(5,7)]C", "C[T(5,7)]G", "G[T(5,7)]A", "G[T(5,7)]C", 
"G[T(5,7)]G", "A[T(8,9)]A", "A[T(8,9)]C", "A[T(8,9)]G", "C[T(8,9)]A", 
"C[T(8,9)]C", "C[T(8,9)]G", "G[T(8,9)]A", "G[T(8,9)]C", "G[T(8,9)]G", 
"L(2,4):R1", "L(5, ):R1", "L(2,8):U(1,2):R(2,4)", "L(2, ):U(1,2):R(5,9)", 
"L(3, ):U(3,):R2", "L(3, ):U(3,):R(3,9)", "L(2,5):M1", "L(3,5):M2", 
"L(4,5):M(3,4)", "L(6, ):M1", "L(6, ):M2", "L(6, ):M3", "L(6, ):M(4, )", 
"Complex")), class = "data.frame", row.names = c(NA, -89L))




  muts_basis_melt <- reshape2::melt(muts_basis,"IndelType")

  muts_basis_melt <- merge(indel_type_4_figurelabel, muts_basis_melt,by="IndelType",all.x=T)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType","Indel","Indel3","Figlabel","Sample","freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)


  # indel_mypalette_fill <- c("skyblue","orange", "blue","tomato","greenyellow","pink","grey","purple","deeppink","black")
  indel_mypalette_fill <- c("#000000", # FEABB9 Complex
                            "#762A83", # Del(2,):M(1,)
                            "#EE3377", # Del(2,):R(0,9) #EE6677
                            "#004488", # Del(C)
                            "#997700", # Del(T)
                            "#EE99AA", #"#668D3C"  Ins(2,)
                            "#6699CC", #"#007996"  Ins(C)
                            "#EECC66") # Ins(T)

  indel_positions <- indel_type_4_figurelabel$IndelType
  indel_positions_labels <- indel_type_4_figurelabel$Figlabel

#  indel_positions <- indel_template_type_4$IndelType
  entry <- table(indel_type_4_figurelabel$Indel)
  order_entry <- c("Ins(C)", "Ins(T)", "Ins(2,)", "Del(C)", "Del(T)", "Del(2,):R(0,9)", "Del(2,):M(1,)", "Complex")
  entry <- entry[order_entry]

  blocks <- data.frame(Type=unique(indel_type_4_figurelabel$Indel),
                       fill=indel_mypalette_fill,
                       xmin=c(0,cumsum(entry)[-length(entry)])+0.5,
                       xmax=cumsum(entry)+0.5)
  blocks$ymin <- max(muts_basis_melt$freq)*1.08#
  blocks$ymax <- max(muts_basis_melt$freq)*1.2
  blocks$labels <-c("1bp C", "1bp T", ">=2bp", "1bp C", "1bp T", ">=2bp", "Mh", "X")
  blocks$cl <-c("black", "black", "black", "white", "white", "white", "white",  "white")

  # grey blocks for insertion or deletion or complex
  indel_mypalette_fill3 <- c("#000000", # FEABB9 Complex
                             "#888888", # Deletion
                             "#DDDDDD") # Insertion

  entry3 <- table(indel_type_4_figurelabel$Indel3)
  order_entry3 <- c("Insertion", "Deletion", "Complex")
  entry3 <- entry3[order_entry3]
  blocks3 <- data.frame(Type=unique(indel_type_4_figurelabel$Indel3),
                        fill=indel_mypalette_fill3,
                        xmin=c(0,cumsum(entry3)[-length(entry3)])+0.5,
                        xmax=cumsum(entry3)+0.5)
  blocks3$ymin <- max(muts_basis_melt$freq)*1.2#
  blocks3$ymax <- max(muts_basis_melt$freq)*1.32
  blocks3$labels <-c("Insertion", "Deletion", "X")
  blocks3$cl <-c("black", "white",  "white")

  indel_mypalette_fill_all <- c("#000000", # FEABB9 Complex

                                "#762A83", # Del(2,):M(1,)
                                "#EE3377", # Del(2,):R(0,9) #EE6677
                                "#004488", # Del(C)
                                "#997700", # Del(T)
                                "#888888", # Deletion

                                "#EE99AA", #"#668D3C"  Ins(2,)
                                "#6699CC", #"#007996"  Ins(C)
                                "#EECC66", # Ins(T)
                                "#DDDDDD") # Insertion


  p <- ggplot2::ggplot(data=muts_basis_melt, ggplot2::aes(x=IndelType, y=freq,fill=Indel))+ ggplot2::geom_bar(stat="identity",position="dodge", width=.7)+ggplot2::xlab("Indel Types")+ggplot2::ylab("Percentage")
  #  p <- p+scale_y_continuous(limits=c(0,40),breaks=(seq(0,40,10)))
  p <- p+ggplot2::scale_x_discrete(limits = indel_positions)+ ggplot2::scale_y_continuous(labels=scales::percent)+ggplot2::ggtitle(plot_title)
  p <- p+ggplot2::scale_fill_manual(values=indel_mypalette_fill_all)+ggplot2::coord_cartesian(ylim=c(0,unique(blocks3$ymax)), expand = FALSE)
  p <- p+ggplot2::theme_classic()+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, size=5,colour = "black",hjust=1),
                               axis.text.y=ggplot2::element_text(size=10,colour = "black"),
                               #    axis.line.y=element_blank(),
                               legend.position = "none",
                               axis.title.x = ggplot2::element_text(size=15),
                               axis.title.y = ggplot2::element_text(size=15))

  ## Add the overhead blocks for insertion or deletion or complex
  p <- p+ggplot2::geom_rect(data = blocks3, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type,colour = "white"),inherit.aes = F)+
    # geom_text(data=blocks,aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels),size=text_size,inherit.aes = F,colour="white")
    ggplot2::geom_text(data=blocks3,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)+ggplot2::scale_colour_manual(values=c("black", "white"))

  ## Add the overhead blocks for indel bases
  p <- p+ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type,colour = "white"),inherit.aes = F)+
    ggplot2::geom_text(data=blocks,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)

  return(p)


  # return(muts_basis)
}

#' Plot indel profile in a 89-channel bar plot for single sample
#' Show all x labels
#' @param muts_basis A indel catalogue of a single sample
#' @param text_size Size of text
#' @param plot_title Title of the plot
#' @return A 89-channel indel profile plot
#' @export
gen_plot_catalouge89_single<- function(muts_basis,text_size,plot_title){
  #mean_parentmuts <- sum(muts_basis[,1:(dim(muts_basis)[2]-1)])/(dim(muts_basis)[2]-1)


  indel_type_4_figurelabel <- structure(list(IndelType = c("A[Ins(C):R0]A", "A[Ins(C):R0]T", 
"Ins(C):R(0,3)", "Ins(C):R(4,6)", "Ins(C):R(7,9)", "A[Ins(T):R(0,4)]A", 
"A[Ins(T):R(0,4)]C", "A[Ins(T):R(0,4)]G", "C[Ins(T):R(0,4)]A", 
"C[Ins(T):R(0,4)]C", "C[Ins(T):R(0,4)]G", "G[Ins(T):R(0,4)]A", 
"G[Ins(T):R(0,4)]C", "G[Ins(T):R(0,4)]G", "A[Ins(T):R(5,7)]A", 
"A[Ins(T):R(5,7)]C", "A[Ins(T):R(5,7)]G", "C[Ins(T):R(5,7)]A", 
"C[Ins(T):R(5,7)]C", "C[Ins(T):R(5,7)]G", "G[Ins(T):R(5,7)]A", 
"G[Ins(T):R(5,7)]C", "G[Ins(T):R(5,7)]G", "A[Ins(T):R(8,9)]A", 
"A[Ins(T):R(8,9)]C", "A[Ins(T):R(8,9)]G", "C[Ins(T):R(8,9)]A", 
"C[Ins(T):R(8,9)]C", "C[Ins(T):R(8,9)]G", "G[Ins(T):R(8,9)]A", 
"G[Ins(T):R(8,9)]C", "G[Ins(T):R(8,9)]G", "Ins(2,4):R0", "Ins(5,):R0", 
"Ins(2,4):R1", "Ins(5,):R1", "Ins(2,):R(2,4)", "Ins(2,):R(5,9)", 
"[Del(C):R1]A", "[Del(C):R1]T", "[Del(C):R2]A", "[Del(C):R2]T", 
"[Del(C):R3]A", "[Del(C):R3]T", "[Del(C):R(4,5)]A", "[Del(C):R(4,5)]T", 
"[Del(C):R(1,5)]G", "Del(C):R(6,9)", "A[Del(T):R(1,4)]A", "A[Del(T):R(1,4)]C", 
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
"Del(6,):M2", "Del(6,):M3", "Del(6,):M(4,)", "Complex"), Indel = c("Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(2,)", "Ins(2,)", "Ins(2,)", 
"Ins(2,)", "Ins(2,)", "Ins(2,)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(2,):R(0,9)", 
"Del(2,):R(0,9)", "Del(2,):R(0,9)", "Del(2,):R(0,9)", "Del(2,):R(0,9)", 
"Del(2,):R(0,9)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Complex"), Indel3 = c("Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Complex"), Figlabel = c("A[C0]A", "A[C0]T", 
"C(0,3)", "C(4,6)", "C(7,9)", "A[T(0,4)]A", "A[T(0,4)]C", "A[T(0,4)]G", 
"C[T(0,4)]A", "C[T(0,4)]C", "C[T(0,4)]G", "G[T(0,4)]A", "G[T(0,4)]C", 
"G[T(0,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)[G", "C[T(5,7)[A", 
"C[T(5,7)[C", "C[T(5,7)[G", "G[T(5,7)[A", "G[T(5,7)[C", "G[T(5,7)]G", 
"A[T(8,9)]A", "A[T(8,9)]C", "A[T(8,9)]G", "C[T(8,9)]A", "C[T(8,9)]C", 
"C[T(8,9)]G", "G[T(8,9)]A", "G[T(8,9)]C", "G[T(8,9)]G", "L(2,4):R0", 
"L(5, ):R0", "L(2,4):R1", "L(5, ):R1", "L(2, ):R(2,4)", "L(2, ):R(5,9)", 
"[C1]A", "[C1]T", "[C2]A", "[C2]T", "[C3]A", "[C3]T", "[C(4,5)]A", 
"[C(4,5)]T", "[C(1,5)]G", "C(6,9)", "A[T(1,4)]A", "A[T(1,4)]C", 
"A[T(1,4)]G", "C[T(1,4)]A", "C[T(1,4)]C", "C[T(1,4)]G", "G[T(1,4)]A", 
"G[T(1,4)]C", "G[T(1,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)]G", 
"C[T(5,7)]A", "C[T(5,7)]C", "C[T(5,7)]G", "G[T(5,7)]A", "G[T(5,7)]C", 
"G[T(5,7)]G", "A[T(8,9)]A", "A[T(8,9)]C", "A[T(8,9)]G", "C[T(8,9)]A", 
"C[T(8,9)]C", "C[T(8,9)]G", "G[T(8,9)]A", "G[T(8,9)]C", "G[T(8,9)]G", 
"L(2,4):R1", "L(5, ):R1", "L(2,8):U(1,2):R(2,4)", "L(2, ):U(1,2):R(5,9)", 
"L(3, ):U(3,):R2", "L(3, ):U(3,):R(3,9)", "L(2,5):M1", "L(3,5):M2", 
"L(4,5):M(3,4)", "L(6, ):M1", "L(6, ):M2", "L(6, ):M3", "L(6, ):M(4, )", 
"Complex")), class = "data.frame", row.names = c(NA, -89L))
  
  muts_basis_melt <- reshape2::melt(muts_basis,"IndelType")

  muts_basis_melt <- merge(indel_type_4_figurelabel, muts_basis_melt,by="IndelType",all.x=T)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType","Indel","Indel3","Figlabel","Sample","freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)


  # indel_mypalette_fill <- c("skyblue","orange", "blue","tomato","greenyellow","pink","grey","purple","deeppink","black")
#  indel_mypalette_fill <- c("#06C2F4", # [-C]
  #                            "#FF8642", # [-T]
  #                            "royalblue", #"#007996"  [+C]
  #                          "#C0362C", # [+T]
  #                          "#000000", # FEABB9 Complex
  #                          "#B1DDA1", # Del_nMer
  #                      #    "#C3B7AC", # Del_NonRep
  #                          "#CF97D7", # Del_Spaced
  #                          "limegreen") #"#668D3C"  Ins_nMer
  #                      #    "#816C5B")  # Ins_NonRep

#  indel_mypalette_fill <- c("#000000", # FEABB9 Complex
#                            "#CF97D7", # Del:L(2,):M(1,)
#                            "#B2E289", # Del:L(2,):R(0,)
#                            "#06C2F4", # Del:L1:C
#                            "#FF8642", # Del:L1:T
#                            "#4B8C61", #"#668D3C"  Ins:L(2,)
#                            "royalblue", #"#007996"  Ins:L1:C
#                            "#C0362C") # Ins:L1:T

  indel_mypalette_fill <- c("#000000", # FEABB9 Complex
                            "#762A83", # Del(2,):M(1,)
                            "#EE3377", # Del(2,):R(0,9) #EE6677
                            "#004488", # Del(C)
                            "#997700", # Del(T)
                            "#EE99AA", #"#668D3C"  Ins(2,)
                            "#6699CC", #"#007996"  Ins(C)
                            "#EECC66") # Ins(T)

  indel_positions <- indel_type_4_figurelabel$IndelType
  indel_positions_labels <- indel_type_4_figurelabel$Figlabel

  # color blocks for indel bases

  entry <- table(indel_type_4_figurelabel$Indel)
  order_entry <- c("Ins(C)", "Ins(T)", "Ins(2,)", "Del(C)", "Del(T)", "Del(2,):R(0,9)", "Del(2,):M(1,)", "Complex")
  entry <- entry[order_entry]
  blocks <- data.frame(Type=unique(indel_type_4_figurelabel$Indel),
                       fill=indel_mypalette_fill,
                       xmin=c(0,cumsum(entry)[-length(entry)])+0.5,
                       xmax=cumsum(entry)+0.5)
  blocks$ymin <- max(muts_basis_melt$freq)*1.08#
  blocks$ymax <- max(muts_basis_melt$freq)*1.2
  blocks$labels <-c("1bp C", "1bp T", ">=2bp", "1bp C", "1bp T", ">=2bp", "Mh", "X")
  blocks$cl <-c("black", "black", "black", "white", "white", "white", "white",  "white")

  # grey blocks for insertion or deletion or complex
  indel_mypalette_fill3 <- c("#000000", # FEABB9 Complex
                            "#888888", # Deletion
                            "#DDDDDD") # Insertion

  entry3 <- table(indel_type_4_figurelabel$Indel3)
  order_entry3 <- c("Insertion", "Deletion", "Complex")
  entry3 <- entry3[order_entry3]
  blocks3 <- data.frame(Type=unique(indel_type_4_figurelabel$Indel3),
                       fill=indel_mypalette_fill3,
                       xmin=c(0,cumsum(entry3)[-length(entry3)])+0.5,
                       xmax=cumsum(entry3)+0.5)
  blocks3$ymin <- max(muts_basis_melt$freq)*1.2#
  blocks3$ymax <- max(muts_basis_melt$freq)*1.32
  blocks3$labels <-c("Insertion", "Deletion", "X")
  blocks3$cl <-c("black", "white",  "white")

  indel_mypalette_fill_all <- c("#000000", # FEABB9 Complex

                            "#762A83", # Del(2,):M(1,)
                            "#EE3377", # Del(2,):R(0,9) #EE6677
                            "#004488", # Del(C)
                            "#997700", # Del(T)
                            "#888888", # Deletion

                            "#EE99AA", #"#668D3C"  Ins(2,)
                            "#6699CC", #"#007996"  Ins(C)
                            "#EECC66", # Ins(T)
                            "#DDDDDD") # Insertion


  p <- ggplot2::ggplot(data=muts_basis_melt, ggplot2::aes(x=IndelType, y=freq,fill=Indel))+ ggplot2::geom_bar(stat="identity",position="dodge", width=.7)+ggplot2::xlab("Indel Types")+ggplot2::ylab("Count")
  #  p <- p+scale_y_continuous(limits=c(0,40),breaks=(seq(0,40,10)))
  p <- p+ggplot2::scale_x_discrete(limits = indel_positions,labels = indel_positions_labels)+ ggplot2::ggtitle(plot_title)
  p <- p+ggplot2::scale_fill_manual(values=indel_mypalette_fill_all)+ggplot2::coord_cartesian(ylim=c(0,unique(blocks3$ymax)), expand = FALSE)
  p <- p+ggplot2::theme_classic()+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, size=5,colour = "black",hjust=1),
                                                 axis.text.y=ggplot2::element_text(size=10,colour = "black"),
                                                 #    axis.line.y=element_blank(),
                                                 legend.position = "none",
                                                 axis.title.x = ggplot2::element_text(size=15),
                                                 axis.title.y = ggplot2::element_text(size=15))

  ## Add the overhead blocks for insertion or deletion or complex
  p <- p+ggplot2::geom_rect(data = blocks3, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type,colour = "white"),inherit.aes = F)+
    # geom_text(data=blocks,aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels),size=text_size,inherit.aes = F,colour="white")
    ggplot2::geom_text(data=blocks3,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)+ggplot2::scale_colour_manual(values=c("black", "white"))

  ## Add the overhead blocks for indel bases
  p <- p+ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type,colour = "white"),inherit.aes = F)+
    ggplot2::geom_text(data=blocks,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)

  return(p)


  # return(muts_basis)
}

#' Plot indel profile in a 89-channel bar plot for single sample
#' Not show X labels
#' @param muts_basis A indel catalogue of a single sample
#' @param text_size Size of text
#' @param plot_title Title of the plot
#' @return A 89-channel indel profile plot
#' @export
gen_plot_catalouge89_single_noXlabel<- function(muts_basis,text_size,plot_title){
  #mean_parentmuts <- sum(muts_basis[,1:(dim(muts_basis)[2]-1)])/(dim(muts_basis)[2]-1)

indel_type_4_figurelabel <- structure(list(IndelType = c("A[Ins(C):R0]A", "A[Ins(C):R0]T", 
"Ins(C):R(0,3)", "Ins(C):R(4,6)", "Ins(C):R(7,9)", "A[Ins(T):R(0,4)]A", 
"A[Ins(T):R(0,4)]C", "A[Ins(T):R(0,4)]G", "C[Ins(T):R(0,4)]A", 
"C[Ins(T):R(0,4)]C", "C[Ins(T):R(0,4)]G", "G[Ins(T):R(0,4)]A", 
"G[Ins(T):R(0,4)]C", "G[Ins(T):R(0,4)]G", "A[Ins(T):R(5,7)]A", 
"A[Ins(T):R(5,7)]C", "A[Ins(T):R(5,7)]G", "C[Ins(T):R(5,7)]A", 
"C[Ins(T):R(5,7)]C", "C[Ins(T):R(5,7)]G", "G[Ins(T):R(5,7)]A", 
"G[Ins(T):R(5,7)]C", "G[Ins(T):R(5,7)]G", "A[Ins(T):R(8,9)]A", 
"A[Ins(T):R(8,9)]C", "A[Ins(T):R(8,9)]G", "C[Ins(T):R(8,9)]A", 
"C[Ins(T):R(8,9)]C", "C[Ins(T):R(8,9)]G", "G[Ins(T):R(8,9)]A", 
"G[Ins(T):R(8,9)]C", "G[Ins(T):R(8,9)]G", "Ins(2,4):R0", "Ins(5,):R0", 
"Ins(2,4):R1", "Ins(5,):R1", "Ins(2,):R(2,4)", "Ins(2,):R(5,9)", 
"[Del(C):R1]A", "[Del(C):R1]T", "[Del(C):R2]A", "[Del(C):R2]T", 
"[Del(C):R3]A", "[Del(C):R3]T", "[Del(C):R(4,5)]A", "[Del(C):R(4,5)]T", 
"[Del(C):R(1,5)]G", "Del(C):R(6,9)", "A[Del(T):R(1,4)]A", "A[Del(T):R(1,4)]C", 
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
"Del(6,):M2", "Del(6,):M3", "Del(6,):M(4,)", "Complex"), Indel = c("Ins(C)", 
"Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
"Ins(T)", "Ins(T)", "Ins(T)", "Ins(2,)", "Ins(2,)", "Ins(2,)", 
"Ins(2,)", "Ins(2,)", "Ins(2,)", "Del(C)", "Del(C)", "Del(C)", 
"Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
"Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(2,):R(0,9)", 
"Del(2,):R(0,9)", "Del(2,):R(0,9)", "Del(2,):R(0,9)", "Del(2,):R(0,9)", 
"Del(2,):R(0,9)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
"Complex"), Indel3 = c("Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Insertion", "Insertion", "Insertion", "Insertion", "Insertion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
"Deletion", "Deletion", "Complex"), Figlabel = c("A[C0]A", "A[C0]T", 
"C(0,3)", "C(4,6)", "C(7,9)", "A[T(0,4)]A", "A[T(0,4)]C", "A[T(0,4)]G", 
"C[T(0,4)]A", "C[T(0,4)]C", "C[T(0,4)]G", "G[T(0,4)]A", "G[T(0,4)]C", 
"G[T(0,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)[G", "C[T(5,7)[A", 
"C[T(5,7)[C", "C[T(5,7)[G", "G[T(5,7)[A", "G[T(5,7)[C", "G[T(5,7)]G", 
"A[T(8,9)]A", "A[T(8,9)]C", "A[T(8,9)]G", "C[T(8,9)]A", "C[T(8,9)]C", 
"C[T(8,9)]G", "G[T(8,9)]A", "G[T(8,9)]C", "G[T(8,9)]G", "L(2,4):R0", 
"L(5, ):R0", "L(2,4):R1", "L(5, ):R1", "L(2, ):R(2,4)", "L(2, ):R(5,9)", 
"[C1]A", "[C1]T", "[C2]A", "[C2]T", "[C3]A", "[C3]T", "[C(4,5)]A", 
"[C(4,5)]T", "[C(1,5)]G", "C(6,9)", "A[T(1,4)]A", "A[T(1,4)]C", 
"A[T(1,4)]G", "C[T(1,4)]A", "C[T(1,4)]C", "C[T(1,4)]G", "G[T(1,4)]A", 
"G[T(1,4)]C", "G[T(1,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)]G", 
"C[T(5,7)]A", "C[T(5,7)]C", "C[T(5,7)]G", "G[T(5,7)]A", "G[T(5,7)]C", 
"G[T(5,7)]G", "A[T(8,9)]A", "A[T(8,9)]C", "A[T(8,9)]G", "C[T(8,9)]A", 
"C[T(8,9)]C", "C[T(8,9)]G", "G[T(8,9)]A", "G[T(8,9)]C", "G[T(8,9)]G", 
"L(2,4):R1", "L(5, ):R1", "L(2,8):U(1,2):R(2,4)", "L(2, ):U(1,2):R(5,9)", 
"L(3, ):U(3,):R2", "L(3, ):U(3,):R(3,9)", "L(2,5):M1", "L(3,5):M2", 
"L(4,5):M(3,4)", "L(6, ):M1", "L(6, ):M2", "L(6, ):M3", "L(6, ):M(4, )", 
"Complex")), class = "data.frame", row.names = c(NA, -89L))


  muts_basis_melt <- reshape2::melt(muts_basis,"IndelType")

  muts_basis_melt <- merge(indel_type_4_figurelabel, muts_basis_melt,by="IndelType",all.x=T)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType","Indel","Indel3","Figlabel","Sample","freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)



  indel_mypalette_fill <- c("#000000", # FEABB9 Complex
                            "#762A83", # Del(2,):M(1,)
                            "#EE6677", # Del(2,):R(0,9)
                            "#004488", # Del(C)
                            "#997700", # Del(T)
                            "#EE99AA", #"#668D3C"  Ins(2,)
                            "#6699CC", #"#007996"  Ins(C)
                            "#EECC66") # Ins(T)

  indel_positions <- indel_type_4_figurelabel$IndelType
  indel_positions_labels <- indel_type_4_figurelabel$Figlabel
  entry <- table(indel_type_4_figurelabel$Indel)
  order_entry <- c("Ins(C)", "Ins(T)", "Ins(2,)", "Del(C)", "Del(T)", "Del(2,):R(0,9)", "Del(2,):M(1,)", "Complex")
  entry <- entry[order_entry]
  blocks <- data.frame(Type=unique(indel_type_4_figurelabel$Indel),
                       fill=indel_mypalette_fill,
                       xmin=c(0,cumsum(entry)[-length(entry)])+0.5,
                       xmax=cumsum(entry)+0.5)
  blocks$ymin <- -max(muts_basis_melt$freq)*0.12#
  blocks$ymax <- 0
  blocks$labels <-c("1bp C", "1bp T", ">=2bp", "1bp C", "1bp T", ">=2bp", "Mh", "X")
  blocks$cl <-c("black", "black", "black", "white", "white", "white", "white",  "white")

  # grey blocks for insertion or deletion or complex
  indel_mypalette_fill3 <- c("#000000", # FEABB9 Complex
                             "#888888", # Deletion
                             "#DDDDDD") # Insertion

  entry3 <- table(indel_type_4_figurelabel$Indel3)
  order_entry3 <- c("Insertion", "Deletion", "Complex")
  entry3 <- entry3[order_entry3]
  blocks3 <- data.frame(Type=unique(indel_type_4_figurelabel$Indel3),
                        fill=indel_mypalette_fill3,
                        xmin=c(0,cumsum(entry3)[-length(entry3)])+0.5,
                        xmax=cumsum(entry3)+0.5)
  blocks3$ymin <- max(muts_basis_melt$freq)*1.08#
  blocks3$ymax <- max(muts_basis_melt$freq)*1.2
  blocks3$labels <-c("Insertion", "Deletion", "X")
  blocks3$cl <-c("black", "white",  "white")

  indel_mypalette_fill_all <- c("#000000", # FEABB9 Complex

                                "#762A83", # Del(2,):M(1,)
                                "#EE3377", # Del(2,):R(0,9) #EE6677
                                "#004488", # Del(C)
                                "#997700", # Del(T)
                                "#888888", # Deletion

                                "#EE99AA", #"#668D3C"  Ins(2,)
                                "#6699CC", #"#007996"  Ins(C)
                                "#EECC66", # Ins(T)
                                "#DDDDDD") # Insertion

  p <- ggplot2::ggplot(data=muts_basis_melt, ggplot2::aes(x=IndelType, y=freq,fill=Indel))+ ggplot2::geom_bar(stat="identity",position="dodge", width=.7)+ggplot2::xlab("Indel Types")+ggplot2::ylab("Count")
  #  p <- p+scale_y_continuous(limits=c(0,40),breaks=(seq(0,40,10)))
  p <- p+ggplot2::scale_x_discrete(limits = indel_positions,labels = indel_positions_labels)+ ggplot2::ggtitle(plot_title)
  p <- p+ggplot2::scale_fill_manual(values=indel_mypalette_fill_all)+ggplot2::coord_cartesian(ylim=c(unique(blocks$ymin),unique(blocks3$ymax)), expand = F)
  p <- p+ggplot2::theme_classic()+ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                                                 axis.text.y=ggplot2::element_text(size=10,colour = "black"),
                                                 axis.ticks.x=ggplot2::element_blank(),
                                                 axis.line.x=ggplot2::element_blank(),
                                                 legend.position = "none",
                                                 #axis.title.x = ggplot2::element_text(size=15),
                                                 axis.title.y = ggplot2::element_text(size=15))

  ## Add the overhead blocks
  p <- p+ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type,colour = "white"),inherit.aes = F)+
    # geom_text(data=blocks,aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels),size=text_size,inherit.aes = F,colour="white")
    ggplot2::geom_text(data=blocks,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)+ggplot2::scale_colour_manual(values=c("black", "white"))

  ## Add the overhead blocks for insertion or deletion or complex
  p <- p+ggplot2::geom_rect(data = blocks3, ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Type,colour = "white"),inherit.aes = F)+
    # geom_text(data=blocks,aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels),size=text_size,inherit.aes = F,colour="white")
    ggplot2::geom_text(data=blocks3,ggplot2::aes(x=(xmax+xmin)/2,y=(ymax+ymin)/2,label=labels, colour=cl),size=text_size,fontface="bold",inherit.aes = F)

  return(p)


  # return(muts_basis)
}



#' Plot indel profile in a 89-channel bar plot, original plots_type_4_m4_89 function
#'
#' @param muts_basis A indel catalogue of multiple samples
#' @param colnum Number of columns
#' @param h Hight of the plot
#' @param w Width of the plot
#' @param text_size Size of text
#' @param print_Xlabel print X axis label or not
#' @param outputname Output file name of the plot
#' @return A plot including 89-channel indel profile of multiple samples
#' @import gridExtra
#' @export
plots_indelprofile_89ch<- function(muts_basis,colnum, h,w,text_size,print_Xlabel=T,, print_pdf = T, print_png=F, outputname){

  cnames <- names(muts_basis)
#  cnames <- cnames[cnames !="IndelType"]
#  muts_basis2 <- muts_basis[,names(muts_basis) != "IndelType"]
  muts_basis2 <- muts_basis

  if(length(cnames)>1){
    p_all <- list()
    for(i in 1:dim(muts_basis2)[2]){

      plottitle <- paste0(names(muts_basis2)[i], ", N =", sum(muts_basis2[,i]))
      if(print_Xlabel==T){
        p <- gen_plot_catalouge89_single(data.frame("Sample"=muts_basis2[,i],"IndelType"=rownames(muts_basis2)), text_size,plottitle)

      }else{
        p <- gen_plot_catalouge89_single_noXlabel(data.frame("Sample"=muts_basis2[,i],"IndelType"=rownames(muts_basis2)), text_size,plottitle)

      }
      p_all[[length(p_all)+1]] <- p

    }

    if(is.null(outputname) == F){
      
      if(print_pdf){
	      filename <- paste0(outputname, ".pdf")
	      grDevices::pdf(file=filename, onefile=TRUE,width=w,height=h)

	      do.call("grid.arrange", c(p_all, ncol = colnum))
	      grDevices::dev.off()
      }    
    
      if(print_png){
	      filename <- paste0(outputname, ".png")
	      grDevices::png(file=filename,width=w,height=h)

	      do.call("grid.arrange", c(p_all, ncol = colnum))
	      grDevices::dev.off()
      }    
    
    
    }else{


      return(do.call("grid.arrange", c(p_all, ncol = colnum)))

    }
  }else{
    plottitle <- paste0(names(muts_basis2)[1], ", N =", sum(muts_basis2[,1]))
    p_all <- list()
    if(print_Xlabel==T){
      p <- gen_plot_catalouge89_single(data.frame("Sample"=muts_basis2[,1],"IndelType"=rownames(muts_basis2)), text_size,plottitle)

    }else{
      p <- gen_plot_catalouge89_single_noXlabel(data.frame("Sample"=muts_basis2[,1],"IndelType"=rownames(muts_basis2)), text_size,plottitle)

    }
    p_all[[length(p_all)+1]] <- p
    
    
    
    
    if(is.null(outputname)==F){
    
	    if(print_pdf){
    		filename <- paste0(outputname, ".pdf")
    		grDevices::pdf(file=filename, onefile=TRUE,width=w,height=h)

    		do.call("grid.arrange", c(p_all, ncol = colnum))
    		grDevices::dev.off()
    	 }
    	    
    	    if(print_png){
    		filename <- paste0(outputname, ".png")
    		grDevices::png(file=filename,width=w,height=h)

    		do.call("grid.arrange", c(p_all, ncol = colnum))
    		grDevices::dev.off()
    	    }   
    
    
    
    
    
    }else{

      return(do.call("grid.arrange", c(p_all, ncol = colnum)))

    }

  }

}



#' Plot indel profile in a 89-channel bar plot, original plots_type_4_m4_89 function
#' Not show X labels
#' @param muts_basis A indel catalogue of multiple samples
#' @param colnum Number of columns
#' @param h Hight of the plot
#' @param w Width of the plot
#' @param text_size Size of text
#' @param outputname Output file name of the plot
#' @return A plot including 89-channel indel profile of multiple samples
#' @import gridExtra
#' @export
plots_indelprofile_89ch_noXlabel<- function(muts_basis,colnum, h,w,text_size,outputname){

  cnames <- names(muts_basis)
  cnames <- cnames[cnames !="IndelType"]
  muts_basis2 <- muts_basis[,names(muts_basis) != "IndelType"]

  if(length(cnames)>1){
    p_all <- list()
    for(i in 1:dim(muts_basis2)[2]){

      plottitle <- paste0(names(muts_basis2)[i], ", N =", sum(muts_basis2[,i]))
      p <- gen_plot_catalouge89_single_noXlabel(data.frame("Sample"=muts_basis2[,i],"IndelType"=rownames(muts_basis2)), text_size,plottitle)
      p_all[[length(p_all)+1]] <- p

    }

    filename <- paste0(outputname, ".pdf")
    grDevices::pdf(file=filename, onefile=TRUE,width=w,height=h)

    do.call("grid.arrange", c(p_all, ncol = colnum))
    grDevices::dev.off()

  }else{
    p <- gen_plot_catalouge89_single_noXlabel(data.frame("Sample"=muts_basis[,cnames],"IndelType"=rownames(muts_basis)), text_size,cnames)
    filename <- paste0(outputname, ".pdf")
    grDevices::pdf(file=filename, onefile=TRUE,width=10,height=5)

    do.call("grid.arrange", c(p, ncol = 1))
    grDevices::dev.off()


  }

}





#' Fixed size plot indel signatures in a 89-channel bar plot, original plots_type_4_m4_89 function
#'
#' @param muts_basis A indel catalogue of multiple samples
#' @param rownum Number of rowumns
#' @param h Hight of the plot
#' @param w Width of the plot
#' @param outputname Output file name of the plot
#' @return A plot including 89-channel indel signatures of multiple signatures
#' @import gridExtra
#' @export
plots_indelsig_89ch<- function(muts_basis,rownum=5, h=15,w=40,outputname){

  muts_basis2 <- muts_basis[,names(muts_basis) != "IndelType"]
  p_all <- list()
  for(i in 1:dim(muts_basis2)[2]){

    p <- gen_plot_catalouge89_single_percentage(data.frame("Sample"=muts_basis2[,i],"IndelType"=rownames(muts_basis2)), 3,names(muts_basis2)[i])
    p_all[[length(p_all)+1]] <- p

  }


  filename <- paste0(outputname, ".pdf")
  grDevices::pdf(file=filename, onefile=TRUE,width=w,height=h)

  do.call("grid.arrange", c(p_all, ncol = 6, nrow=rownum))


  grDevices::dev.off()

}


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
