#'fmmodel
#'
#' @name fmmodel
#'
#' @description  the function is to format a model table style from html file  produced by stargazer package
#'
#' @param tab A data frame
#' @param ftsz the size of font in the table
#' @param ftname the name of font in the table
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


fmmodel<-function(tab,ftsz=9,ftname="Time New Roma"){
  if(as.vector(tab[nrow(tab),1]=="Note:")){
    newtb<-tab%>%head(-1)
  }else{
    newtb<-tab
  }

  names(newtb)<-as.character(1:ncol(newtb))
  sposit<-which(newtb=="(1)",arr.ind = T)[1,1]-1
  obs<-which(newtb=="Observations",arr.ind = T)[1,1]
  ft<-flextable(newtb)
  for(rowns in 1:sposit){
    namva<-unique(t(newtb[rowns,2:ncol(newtb)])%>%as.vector())
    for(cols in 1:length(namva)){
      ids<-which(newtb==namva[cols],arr.ind = T)[,2]
      ids<-ids[ids>1]
      l<-length(ids)-1
      if(l==0){
        next
      }
      grup<-NULL
      ismerg<-NULL
      cout=1
      for(num in 1:l){
        if(ids[num+1]-ids[num]==1){
          grup[num]=grup[num+1]=cout
          ismerg[num]=ismerg[num+1]=1
        }else{
          grup[num]=cout
          grup[num+1]=cout+1
          ismerg[num]=ismerg[num+1]=0
          cout=cout+1
        }

      }
      recod<-data.frame(grup,ismerg,ids)[ismerg==1,]
      if(nrow(recod!=0)){
        for(idex in unique(recod$grup)){
          mtop<-max(recod$ids[recod$grup==idex])
          mtao<-min(recod$ids[recod$grup==idex])
          ft<-ft%>%merge_at(i=rowns,j=mtao:mtop)
        }
      }
    }
  }
  ft<-border_remove(ft)
  ft<-delete_part(ft,part="header")
  ft <- set_table_properties(ft, layout = "autofit")
  def_par <- fp_par(text.align = "center")
  ft<-style(ft,1:nrow(newtb),2:ncol(newtb),pr_p=def_par)
  def_cell<-fp_border(color = "black",width = 1)
  ft<-surround(ft,c(1,obs),1:ncol(newtb),border.top=def_cell)
  ft<-surround(ft,c(sposit+1,nrow(newtb)),1:ncol(newtb),border.bottom =def_cell)
  ft<-add_footer_lines(ft,stri_encode("\u6ce8\uff1a\u62ec\u53f7\u5185\u4e3a\u56de\u5f52\u7cfb\u6570\u5bf9\u5e94\u7684t\u7edf\u8ba1\u91cf\u3002\u5176\u4e2d***\u3001**\u548c*\u5206\u522b\u4ee3\u8868\u7cfb\u6570\u57281%\u30015%\u548c10%\u7684\u7f6e\u4fe1\u6c34\u5e73\u4e0b\u663e\u8457\u3002"))
  ft<-font(ft,fontname=stri_escape_unicode(ftname),part="all")
  ft<-font(ft,fontname="\u5b8b\u4f53",part="footer")
  ft<-fontsize(ft,size=ftsz,part = "all")
  return(ft)
}
