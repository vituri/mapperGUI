# mapper-GUI

Try the online version first:

https://gvituri.shinyapps.io/mapper-GUI/

If it doesn't work, install the following packages (some of them are not in cran yet)

list_of_packages = c("igraph","TDAmapper", "shiny", "plotly", "shinythemes", "shinycssloaders", "GGally", "ggplot2", "grnn", "ggraph", "TDAstats", "rgl", "cccd", "Matrix", "dplyr")

lapply(list_of_packages, 
       function(x) if(!require(x,character.only = TRUE)) install.packages(x))

and then run the command:

runGitHub( "mapper-GUI", "vituri")

