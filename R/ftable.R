#'fmmodel
#'
#' @name fmmodel
#'
#' @description  the function is to format a model table style from html file  produced by stargazer package
#'
#' @param tab A data frame
#' @param ftsz the size of font in the table
#' @param tabname add table name to header
#' @param langu is a mark which means your paper enviroment is chinese or English, the default is "C", if you want to create a table in English, you can set parameter langu="E".
#' @param notehead is a note before the table header,the default is none.
#' @param notefoot is table footer, the default is none.
#' @param ftsz is a integer, you can set you table font size by it. the default is 9.
#' @param ftname is table font name.  the default is  "Time New Roma".
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


fmmodel<-function(tab,langu="C",tabname="",notehead="",notefoot="",ftsz=9,ftname="Time New Roma"){
  #set_flextable_defaults(padding = 4)
  if(as.vector(tab[nrow(tab),1]=="Note:")){
    newtb<-tab%>%head(-1)
  }else{
    newtb<-tab
  }

  sposit<-which(newtb=="(1)",arr.ind = T)[1,1]-1
  obs<-which(newtb=="Observations",arr.ind = T)[1,1]
  bodyc<-newtb[(sposit+2):nrow(newtb),]
  names(bodyc)<-as.character(c(" ",newtb[sposit+1,2:ncol(newtb)]))

  ft<-flextable(bodyc)

  if(sposit>1|(langu!="C"&tabname!="")){
    sph<-sposit+1
    for(adhd in sposit:1){
      ft<-add_header_row(ft,values = as.vector(t(newtb[adhd,])),
                         colwidths = rep(1,ncol(newtb)))
    }
  }else{
    sph<-sposit+2
    for(adhd in sposit:0){
      if(adhd==sposit){
        vhead<-rep('',ncol(newtb))
      }else{
        vhead<-as.vector(t(newtb[adhd+1,]))
      }
      ft<-add_header_row(ft,values = vhead,
                         colwidths = rep(1,ncol(newtb)))
    }
  }




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
          ft<-ft%>%merge_at(i=rowns,j=mtao:mtop,part = "header")
        }
      }
    }
  }

  ft <- set_table_properties(ft, layout = "autofit")

  if(langu=="C"){
    ft<-border_remove(ft)
    def_par <- fp_par(text.align = "center")
    ft<-style(ft,1:sph,2:ncol(bodyc),pr_p=def_par,part = "header")
    ft<-style(ft,1:nrow(bodyc),2:ncol(bodyc),pr_p=def_par,part = "body")
    def_cell<-fp_border(color = "black",width = 1)
    ft<-surround(ft,1,1:ncol(newtb),border.top=def_cell,part = "header")
    ft<-surround(ft,sph,1:ncol(newtb),border.bottom =def_cell,part = "header")
    ft<-padding(ft, padding  = 0, part = "header")
    ft <- height(ft, height = 0, part = "header")
    ft <- hrule(ft, rule = "exact", part = "header")
    ft<-surround(ft,c(obs-sposit-2,nrow(bodyc)),1:ncol(bodyc),border.bottom =def_cell,part = "body")
    ft<-add_footer_lines(ft,stri_encode("\u6ce8\uff1a\u62ec\u53f7\u5185\u4e3a\u56de\u5f52\u7cfb\u6570\u5bf9\u5e94\u7684t\u7edf\u8ba1\u91cf\u3002\u5176\u4e2d***\u3001**\u548c*\u5206\u522b\u4ee3\u8868\u7cfb\u6570\u57281%\u30015%\u548c10%\u7684\u7f6e\u4fe1\u6c34\u5e73\u4e0b\u663e\u8457\u3002"))
    ft<-font(ft,fontname=stri_escape_unicode(ftname),part="all")
    ft<-font(ft,fontname="\u5b8b\u4f53",part="footer")
    ft<-fontsize(ft,size=ftsz,part = "all")
    return(ft)
  }else{

    if(tabname==""&notehead==""){
      ft<-border_remove(ft)
      def_par <- fp_par(text.align = "center")
      ft<-style(ft,1:sph,2:ncol(bodyc),pr_p=def_par,part = "header")
      ft<-style(ft,1:nrow(bodyc),2:ncol(bodyc),pr_p=def_par,part = "body")
      def_cell<-fp_border(color = "black",width = 1)
      ft<-surround(ft,1,1:ncol(newtb),border.top=def_cell,part = "header")
      ft<-surround(ft,sph,1:ncol(newtb),border.bottom =def_cell,part = "header")
      ft<-padding(ft, padding  = 0, part = "header")
      ft <- height(ft, height = 0, part = "header")
      ft <- hrule(ft, rule = "exact", part = "header")
      ft<-surround(ft,c(obs-sposit-2,nrow(bodyc)),1:ncol(bodyc),border.bottom =def_cell,part = "body")
      ft<-add_footer_lines(ft,paste("Note:",notefoot,"t-statistics are given in parentheses. \u002a\u002a\u002a, \u002a\u002a and \u002a represent statistical significance at the 1%, 5% and 10% level, respectively."))
    }else{
      if(notehead==""|tabname==""){
        heads<-paste0(notehead,tabname)
        ft<-add_header_lines(ft,values =heads )
        ft<-border_remove(ft)
        def_parf <- fp_par(text.align = "justify")
        ft<-style(ft,1,2:ncol(bodyc),pr_p=def_parf,part = "header")
        def_par <- fp_par(text.align = "center")
        ft<-style(ft,2:sph,2:ncol(bodyc),pr_p=def_par,part = "header")
        ft<-style(ft,1:nrow(bodyc),2:ncol(bodyc),pr_p=def_par,part = "body")
        def_cell<-fp_border(color = "black",width = 1)
        ft<-surround(ft,2,1:ncol(newtb),border.top=def_cell,part = "header")
        ft<-surround(ft,sph+1,1:ncol(newtb),border.bottom =def_cell,part = "header")
        ft<-padding(ft, padding  = 0, part = "header")
        ft <- height(ft, height = 0, part = "header")
        ft <- hrule(ft, rule = "exact", part = "header")
        ft<-surround(ft,c(obs-sposit-2,nrow(bodyc)),1:ncol(bodyc),border.bottom =def_cell,part = "body")
        ft<-add_footer_lines(ft,paste("Note:",notefoot,"t-statistics are given in parentheses. \u002a\u002a\u002a, \u002a\u002a and \u002a represent statistical significance at the 1%, 5% and 10% level, respectively."))
      }else{
        heads<-c(notehead,tabname)
        ft<-add_header_lines(ft,values =heads )
        ft<-border_remove(ft)
        def_parf <- fp_par(text.align = "left")
        ft<-style(ft,1,2:ncol(bodyc),pr_p=def_parf,part = "header")
        def_parf <- fp_par(text.align = "justify")
        ft<-style(ft,2,2:ncol(bodyc),pr_p=def_parf,part = "header")
        def_par <- fp_par(text.align = "center")
        ft<-style(ft,3:sph,2:ncol(bodyc),pr_p=def_par,part = "header")
        ft<-style(ft,1:nrow(bodyc),2:ncol(bodyc),pr_p=def_par,part = "body")
        def_cell<-fp_border(color = "black",width = 1)
        ft<-surround(ft,3,1:ncol(newtb),border.top=def_cell,part = "header")
        ft<-surround(ft,sph+2,1:ncol(newtb),border.bottom =def_cell,part = "header")
        ft<-padding(ft, padding  = 0, part = "header")
        ft <- height(ft, height = 0, part = "header")
        ft <- hrule(ft, rule = "exact", part = "header")
        ft<-surround(ft,c(obs-sposit-2,nrow(bodyc)),1:ncol(bodyc),border.bottom =def_cell,part = "body")
        ft<-add_footer_lines(ft,paste("Note:",notefoot,"t-statistics are given in parentheses. \u002a\u002a\u002a, \u002a\u002a and \u002a represent statistical significance at the 1%, 5% and 10% level, respectively."))
      }

    }
    if(notefoot==""){
      ft<-style(ft,1,2:ncol(bodyc),pr_p=fp_par(text.align = "left"),part = "footer")
    }else{
      ft<-style(ft,1,2:ncol(bodyc),pr_p=fp_par(text.align = "justify"),part = "footer")
    }
    ft<-font(ft,fontname=ftname,part="all")
    ft<-fontsize(ft,size=ftsz,part = "all")
    return(ft)
  }
}
