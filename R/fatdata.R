#'fmtdata
#'
#' @name fmtdata
#'
#' @description  the function is to format a summary data
#'
#' @param x A data frame
#' @param tabname add table name to header
#' @param langu is a mark which means your paper enviroment is chinese or English, the default is "C", if you want to create a table in English, you can set parameter langu="E".
#' @param notehead is a note before the table header,the default is none.
#' @param notefoot is table footer, the default is none.
#' @param ftsz the size of font in the table(default:9pt)
#' @param ftname the name of font in the table(default:Time New Roma)
#' @param align a align way of the table text(default:center)
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


fatdat<-function(x,langu="C",tabname="",notehead="",notefoot="",ftsz=9,ftname="Time New Roma",align="center"){
  len<-length(unique(names(x)))
  if(len==1){
    names(x)<-unlist(x[1,])
    x<-x[-1,]
  }
  ft<-flextable(x)
  nr<-nrow(ft$body$dataset)
  nc<-ncol(ft$body$dataset)
  if(langu=="C"){
    ft<-border_remove(ft)
    def_par <- fp_par(text.align = align)
    ft<-style(ft,j=2:nc,pr_p=def_par,part = "header")
    ft<-style(ft,1:nr,2:nc,pr_p=def_par)
    def_cell <- fp_cell(border.top  = fp_border(color = "black",width = 1),
                        border.bottom = fp_border(color = "black",width = 1))
    ft<-style(ft,pr_c=def_cell,part = "header")

  }else{
    if(tabname==""&notehead==""){
      ft<-border_remove(ft)
      def_par <- fp_par(text.align = align)
      ft<-style(ft,i=1,j=2:nc,pr_p=def_par,part = "header")
      ft<-style(ft,1:nr,2:nc,pr_p=def_par)
      def_cell<-fp_border(color = "black",width = 1)
      ft<-surround(ft,1,1:nc,border.top=def_cell,part = "header")
      ft<-surround(ft,1,1:nc,border.bottom =def_cell,part = "header")
    }else{
      if(tabname==""){
        ft<-add_header_lines(ft,values =notehead )
        ns=2
      }else{
        if(notehead==""){
          ft<-add_header_lines(ft,values =tabname )
          ns=2
        }else{
          ft<-add_header_lines(ft,values =tabname )
          ft<-add_header_lines(ft,values =notehead )
          ns=3
        }
      }
      ft<-border_remove(ft)
      def_par <- fp_par(text.align = align)
      ft<-style(ft,i=ns,j=1:nc,pr_p=fp_par(text.align = "justify"),part = "header")
      ft<-style(ft,1:nr,2:nc,pr_p=def_par)
      def_cell<-fp_border(color = "black",width = 1)
      ft<-surround(ft,ns,1:nc,border.top=def_cell,part = "header")
      ft<-surround(ft,ns,1:nc,border.bottom =def_cell,part = "header")
      }
  }

  if(notefoot!=""){
    ft<-add_footer_lines(ft,notefoot)
    ft<-style(ft,1,1:nc,pr_p=fp_par(text.align = "justify"),part = "footer")
  }

  def_bott<-fp_cell(border.bottom = fp_border(color = "black",width = 1))
  ft<-style(ft,nr,1:nc,pr_c=def_bott)
  ft<-set_table_properties(ft, layout = "autofit")
  ft<-font(ft,fontname=stri_escape_unicode(ftname),part="all")
  ft<-fontsize(ft,size=ftsz,part = "all")
  return(ft)

}
