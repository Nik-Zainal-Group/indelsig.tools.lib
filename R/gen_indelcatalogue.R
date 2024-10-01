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
  return(indel_catalogue[,-c(1:2),drop=FALSE])
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
