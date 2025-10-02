#' Format a vector into a comma seperated string and cat it to the commandline
#'
#' @param v The vector to format
#' @param cat TT/F, whether to cat output to interpreter
#' @return formatted vector
#' @export
#'
#' @examples
#' format_vector(c(1,2,3,4))
format_vector <- function(v,cat=TRUE){
  v_formatted <- (paste0('c("',paste0(v,collapse='","'),'")'))
#format_vector(unname(unlist(lapply(split(st4$type_4,cut(1:length(st4$type_4),4)),'c',c('\n')))))
  if(cat){
    cat(v_formatted)
    return(invisible())
  }else{
    return(v_formatted)
  }
}
