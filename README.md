
# Startowd

<!-- badges: start -->
<!-- badges: end -->

The goal of Startowd is to Modify Html Table Style From Stargazer 

## Why do I creat this package

Due to Stargazer package don't suport to output a table into Microsoft Word documents(e.g.,doc or docx), 
when we want to output summary table into Word document like Stargazer package, Only save output into an .htm or .html file by Stargazer. handly Open the resulting file in your web browser, Copy and paste the table from the web browser to your Microsoft Word document.

In order to output well-formatted tables into .docx file, I build Startoword package which can read a .html table from Stargazer and make it become a flextable object with function flextable. You can output this flextable
object into .docx file by any style you like.

## Installation

You can install the development version of Startowd from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yingjiexue/Startowd")
```

### Example

Read table form .html file outputed by Stargazer

``` r
library(Startowd)
## basic example code
fname<-system.file("extdata","table.html",package = "Startowd",mustWork = TRUE)
table<-gettable(fname)
```

Make the model result table become a flextable object with function flextable

``` r
library(Startowd)
fname<-system.file("extdata","table.html",package = "Startowd",mustWork = TRUE)
table<-gettable(fname)
print(table)
ft<-fmmodel(table)
ft
```


Make the data summary result table become a flextable object with function flextable

``` r
library(Startowd)
fname<-system.file("extdata","summary.html",package = "Startowd",mustWork = TRUE)
table<-gettable(fname)
ft<-fatdat(table)
ft
```
