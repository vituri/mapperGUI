# mapper-GUI

list_of_packages = c("igraph","TDAmapper", "shiny", "plotly", "shinythemes", "shinycssloaders", "GGally", "ggplot2", "grnn", "ggraph", "TDAstats", "rgl", "cccd", "Matrix", "dplyr")

lapply(list_of_packages, 
       function(x) if(!require(x,character.only = TRUE)) install.packages(x))

runGitHub( "mapper-GUI", "vituri")

