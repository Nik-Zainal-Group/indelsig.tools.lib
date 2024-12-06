InDel_sigs_order <- c("A[Ins(C):R0]A", "A[Ins(C):R0]T", "Ins(C):R(0,3)", "Ins(C):R(4,6)",
                      "Ins(C):R(7,9)", "A[Ins(T):R(0,4)]A", "A[Ins(T):R(0,4)]C", "A[Ins(T):R(0,4)]G",
                      "C[Ins(T):R(0,4)]A", "C[Ins(T):R(0,4)]C", "C[Ins(T):R(0,4)]G",
                      "G[Ins(T):R(0,4)]A", "G[Ins(T):R(0,4)]C", "G[Ins(T):R(0,4)]G",
                      "A[Ins(T):R(5,7)]A", "A[Ins(T):R(5,7)]C", "A[Ins(T):R(5,7)]G",
                      "C[Ins(T):R(5,7)]A", "C[Ins(T):R(5,7)]C", "C[Ins(T):R(5,7)]G",
                      "G[Ins(T):R(5,7)]A", "G[Ins(T):R(5,7)]C", "G[Ins(T):R(5,7)]G",
                      "A[Ins(T):R(8,9)]A", "A[Ins(T):R(8,9)]C", "A[Ins(T):R(8,9)]G",
                      "C[Ins(T):R(8,9)]A", "C[Ins(T):R(8,9)]C", "C[Ins(T):R(8,9)]G",
                      "G[Ins(T):R(8,9)]A", "G[Ins(T):R(8,9)]C", "G[Ins(T):R(8,9)]G",
                      "Ins(2,4):R0", "Ins(5,):R0", "Ins(2,4):R1", "Ins(5,):R1", "Ins(2,):R(2,4)",
                      "Ins(2,):R(5,9)", "[Del(C):R1]A", "[Del(C):R1]T", "[Del(C):R2]A",
                      "[Del(C):R2]T", "[Del(C):R3]A", "[Del(C):R3]T", "[Del(C):R(4,5)]A",
                      "[Del(C):R(4,5)]T", "[Del(C):R(1,5)]G", "Del(C):R(6,9)", "A[Del(T):R(1,4)]A",
                      "A[Del(T):R(1,4)]C", "A[Del(T):R(1,4)]G", "C[Del(T):R(1,4)]A",
                      "C[Del(T):R(1,4)]C", "C[Del(T):R(1,4)]G", "G[Del(T):R(1,4)]A",
                      "G[Del(T):R(1,4)]C", "G[Del(T):R(1,4)]G", "A[Del(T):R(5,7)]A",
                      "A[Del(T):R(5,7)]C", "A[Del(T):R(5,7)]G", "C[Del(T):R(5,7)]A",
                      "C[Del(T):R(5,7)]C", "C[Del(T):R(5,7)]G", "G[Del(T):R(5,7)]A",
                      "G[Del(T):R(5,7)]C", "G[Del(T):R(5,7)]G", "A[Del(T):R(8,9)]A",
                      "A[Del(T):R(8,9)]C", "A[Del(T):R(8,9)]G", "C[Del(T):R(8,9)]A",
                      "C[Del(T):R(8,9)]C", "C[Del(T):R(8,9)]G", "G[Del(T):R(8,9)]A",
                      "G[Del(T):R(8,9)]C", "G[Del(T):R(8,9)]G", "Del(2,4):R1", "Del(5,):R1",
                      "Del(2,8):U(1,2):R(2,4)", "Del(2,):U(1,2):R(5,9)", "Del(3,):U(3,):R2",
                      "Del(3,):U(3,):R(3,9)", "Del(2,5):M1", "Del(3,5):M2", "Del(4,5):M(3,4)",
                      "Del(6,):M1", "Del(6,):M2", "Del(6,):M3", "Del(6,):M(4,)", "Complex"
)



#' Generate indel catalogue in 89 channels
#'
#' @param muts_list A indel list
#' @param sample_col Sample column name
#' @return A 89 channel indel catalogue
#' @export
gen_catalogue89 <- function(muts_list, sample_col){
  indel_catalogue <- data.frame(table(muts_list[,sample_col],muts_list$type_4))
  names(indel_catalogue) <- c("Sample","IndelType","freq")
  indel_catalogue <- reshape2::dcast(indel_catalogue,IndelType~Sample,value.var="freq")


  indel_catalogue <- merge(indel_template_type_4,indel_catalogue,by="IndelType",all.x=T)
  indel_catalogue[is.na(indel_catalogue)] <- 0
  rownames(indel_catalogue) <- indel_catalogue[,"IndelType"]
  return(indel_catalogue[InDel_sigs_order,-c(1:2),drop=FALSE])
}


#' Generate indel catalogue in 21 channels
#'
#' @param muts_list A indel list
#' @param sample_col Sample column name
#' @return A 21 channel indel catalogue
#' @export
gen_catalogue21 <- function(muts_list, sample_col){
  indel_catalogue <- data.frame(table(muts_list[,sample_col],muts_list$type_3))
  names(indel_catalogue) <- c("Sample","type_3","freq")
  indel_catalogue <- reshape2::dcast(indel_catalogue,type_3~Sample,value.var="freq")

  indel_template_type_3 <- data.frame("type_3"=c("[InsC]NonRep","[InsC]ShortRep_leq4","[InsC]LongRep_g4","[InsT]NonRep","[InsT]ShortRep_leq4","[InsT]LongRep_g4","Ins_NonRep","Ins_nMer_ShortRep_leq4","Ins_nMer_LongRep_g4",
                                                 "[DelC]NonRep","[DelC]ShortRep_leq4","[DelC]LongRep_g4","[DelT]NonRep","[DelT]ShortRep_leq4","[DelT]LongRep_g4","Del_NonRep","Del_nMer_ShortRep_leq4","Del_nMer_LongRep_g4","Del_Spaced_short_leq5","Del_Spaced_long_g5",
                                                 "Complex"))
  indel_catalogue <- merge(indel_template_type_3,indel_catalogue,by="type_3",all.x=T)
  indel_catalogue[is.na(indel_catalogue)] <- 0
  rownames(indel_catalogue) <- indel_catalogue[,"type_3"]
  return(indel_catalogue[,-1])

}



#' Generate indel catalogue in full channels
#'
#' @param muts_list A indel list
#' @param sample_col Sample column name
#' @return A full channel indel catalogue
#' @export
gen_fullcatalogue<- function(muts_list, sample_col){
  indel_catalogue <- data.frame(table(muts_list[,sample_col],muts_list$type_4))
  names(indel_catalogue) <- c("Sample","IndelType","freq")
  indel_catalogue <- reshape2::dcast(indel_catalogue,IndelType~Sample,value.var="freq")


  indel_catalogue <- merge(indel_template_type_4_full,indel_catalogue,by="IndelType",all.x=T)
  indel_catalogue[is.na(indel_catalogue)] <- 0
  rownames(indel_catalogue) <- indel_catalogue[,"IndelType"]
  return(indel_catalogue[,-c(1:2),drop=FALSE])
}
