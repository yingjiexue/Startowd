#'fmtdata
#'
#' @name fmtdata
#'
#' @description  the function is to format a summary data
#'
#' @param x A data frame
#' @param align a align way of table text
#' @return The default method returns table
#'
#' @import flextable
#' @import officer
#' @import magrittr
#' @import utils
#' @import stringi
#' @export
#'
#' @examples
#' \dontrun{
#' fname<-system.file("extdata","summary.html",package = "startowd",mustWork = TRUE)
#' table<-gettable(fname)
#' ft<-fatdat(table)
#' ft
#' }
#'
#'


fatdat<-function(x,align="center"){
  len<-length(unique(names(x)))
  if(len==1){
    names(x)<-unlist(x[1,])
    x<-x[-1,]
  }
  ft<-flextable(x)
  nr<-nrow(ft$body$dataset)
  nc<-ncol(ft$body$dataset)
  ft<-border_remove(ft)
  def_par <- fp_par(text.align = align)
  ft<-style(ft,j=2:nc,pr_p=def_par,part = "header")
  ft<-style(ft,1:nr,2:nc,pr_p=def_par)
  def_cell <- fp_cell(border.top  = fp_border(color = "black",width = 1),
                      border.bottom = fp_border(color = "black",width = 1))
  ft<-style(ft,pr_c=def_cell,part = "header")
  def_bott<-fp_cell(border.bottom = fp_border(color = "black",width = 1))
  ft<-style(ft,nr,1:nc,pr_c=def_bott)
  ft<-set_table_properties(ft, layout = "autofit")
  return(ft)
}
