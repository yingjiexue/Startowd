
# startowd

<!-- badges: start -->
<!-- badges: end -->

The goal of startowd is to modify html table style From stargazer 

## Why do I creat this package

Due to stargazer package don't support to output a table into Microsoft Word documents(e.g.,doc or docx), 
when we want to output summary table into Word document like stargazer package, Only save output into an .htm or .html file by stargazer. handly Open the resulting file in your web browser, Copy and paste the table from the web browser to your Microsoft Word document.

In order to output well-formatted tables into .docx file, I build startoword package which can read a .html table from stargazer and make it become a flextable object with function flextable. You can output this flextable
object into .docx file by any style you like.

## Installation

You can install the development version of startowd from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yingjiexue/startowd")
```

### Example

Read table form .html file outputed by stargazer

``` r
library(startowd)
## basic example code
fname<-system.file("extdata","table.html",package = "startowd",mustWork = TRUE)
table<-gettable(fname)
```

Make the model result table become a flextable object with function flextable

``` r
library(startowd)
fname<-system.file("extdata","table.html",package = "startowd",mustWork = TRUE)
table<-gettable(fname)
print(table)
ft<-fmmodel(table)
ft
```


Make the data summary result table become a flextable object with function flextable

``` r
library(startowd)
fname<-system.file("extdata","summary.html",package = "startowd",mustWork = TRUE)
table<-gettable(fname)
ft<-fatdat(table)
ft
```
