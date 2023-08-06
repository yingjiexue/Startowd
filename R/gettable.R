#'gettable
#'
#' @name gettable
#'
#' @description  the function is to read a table from html file
#'
#' @param filename A html file created by stargazer package
#' @return The default method returns a data frame
#'
#' @import rvest
#' @importFrom  stringr str_detect
#' @import utils
#' @import magrittr
#' @export
#'
#' @examples
#'
#' \dontrun{
#' fname<-system.file("extdata","table.html",package = "startowd",mustWork = TRUE)
#' table<-gettable(fname)
#' }
#'

gettable<-function(filename){
  if(!str_detect(filename,'.html|.htm')){
    return(stri_encode("\u6587\u4ef6\u7c7b\u578b\u9519\u8bef\uff0c\u8bf7\u8f93\u5165htm\u6216html\u683c\u5f0f\u7684\u6587\u4ef6"))
  }else{
    gettab<-read_html(filename,encoding = "UTF-8")%>%
      html_node("table")%>%
      html_table(fill = T,header = T)
    tab<-gettab[apply(gettab,MARGIN = 1,function(charc) sum(is.na(charc)|charc=='')!=ncol(gettab)),]
    wed<-gettab|>as.vector()|>unlist()|>paste0(collapse = "")|>str_detect("\\*")
    if(wed){
      minrow=min(which(matrix(str_detect(as.matrix(tab),"\\*"),ncol = ncol(tab)),arr.ind = TRUE)[,1])
      tab[minrow:nrow(tab),2:ncol(tab)]<-apply(tab[minrow:nrow(tab),2:ncol(tab)], 2, gsub,pattern="\\s+", replacement="")
      tab<-data.frame(tab)
    }

  }

  return(tab)
}
