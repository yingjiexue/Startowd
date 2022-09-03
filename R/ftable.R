#'fmmodel
#'
#' @name fmmodel
#'
#' @description  the function is to format a model table style from html file  produced by stargazer package
#'
#' @param tab A data frame
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
#' fname<-system.file("extdata","table.html",package = "startowd",mustWork = TRUE)
#' table<-gettable(fname)
#' print(table)
#' ft<-fmmodel(table)
#' ft
#' }
#'


fmmodel<-function(tab){
  newtb<-tab%>%head(-1)
  names(newtb)<-as.character(1:ncol(newtb))
  sposit<-which(newtb=="(1)",arr.ind = T)[1,1]-1
  obs<-which(newtb=="Observations",arr.ind = T)[1,1]
  ft<-flextable(newtb)
  for(rowns in 1:sposit){
    namva<-unique(t(newtb[rowns,2:ncol(newtb)])%>%as.vector())
    for(cols in 1:length(namva)){
      mtop<-max(which(newtb==namva[cols],arr.ind = T)[,2])
      mtao<-min(which(newtb==namva[cols],arr.ind = T)[,2])
      ft<-ft%>%merge_at(i=rowns,j=mtao:mtop)
    }
  }
  ft<-border_remove(ft)
  def_par <- fp_par(text.align = "center")
  ft<-style(ft,1:nrow(newtb),2:ncol(newtb),pr_p=def_par)
  def_cell <- fp_cell(border.top  = fp_border(color = stri_escape_unicode("black"),width = 1))
  ft<-style(ft,c(1,sposit+2,obs),1:ncol(newtb),pr_c=def_cell)
  def_bott <- fp_cell(border.bottom = fp_border(color = stri_escape_unicode("black"),width = 1))
  ft<-style(ft,nrow(newtb),1:ncol(newtb),pr_c=def_bott)
  ft<-delete_part(ft,part="header")
  ft<-add_footer_lines(ft,stri_encode("\u6ce8\uff1a\u62ec\u53f7\u5185\u4e3a\u56de\u5f52\u7cfb\u6570\u5bf9\u5e94\u7684t\u7edf\u8ba1\u91cf\u3002\u5176\u4e2d***\u3001**\u548c*\u5206\u522b\u4ee3\u8868\u7cfb\u6570\u57281%\u30015%\u548c10%\u7684\u7f6e\u4fe1\u6c34\u5e73\u4e0b\u663e\u8457\u3002"))
  return(ft)
}
