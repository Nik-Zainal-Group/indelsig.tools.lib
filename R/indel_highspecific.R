#' For signature analysis, indels have to be as clean as possible.
#' This function will: 1) remove single indels that have original_reps >=10
#' 2) remove nMer indels that have original_reps >=10
#' 3) small indels: length <=100
#' @param indel.classified A classified indel list.
#' @return A filtered indel list
#' @export
indel_highspecific <- function(indel.classified){
  s <- indel.classified
  # remove single indels that have original_reps >=10
  s <- s[!(s$indel.length==1 & s$indel.type%in% c("D","I") & s$original_reps>=10), ]
  # remove nMer indels that have original_reps >=10
  s <- s[!(s$type_2%in% c("Ins_nMer","Del_nMer") & s$original_reps>=10), ]
  #
  s <- s[!(s$REF%in%c("AA","TT","CC","GG") | s$ALT%in%c("AA","TT","CC","GG")), ]
  # remove long indels
  s <- s[s$indel.length<=100,]

  return(s)

}
