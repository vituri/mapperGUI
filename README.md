# mapper-GUI

This is a Graphical User Interface for the Mapper algorithm, used in Topological Data Analysis. It is still in its initial phase; later I will add explanations about the filters, clusterings, distances and so on. 

To read about Mapper, check the paper "Topological Methods for the Analysis of High Dimensional
Data Sets and 3D Object Recognition": https://research.math.osu.edu/tgda/mapperPBG.pdf
To read about some application, check this nice article by Matt Piekenbrock, creator of the R Mapper package that I will use in my code in the next update: https://peekxc.github.io/Mapper/articles/ShapeRecognition.html

-------------------------------------------------

Try the online version first:

https://gvituri.shinyapps.io/mapper-GUI/

If it doesn't work, install the following packages (some of them are not in cran yet)

list_of_packages = c("igraph","TDAmapper", "shiny", "plotly", "shinythemes", "shinycssloaders", "GGally", "ggplot2", "grnn", "ggraph", "TDAstats", "rgl", "cccd", "Matrix", "dplyr")

lapply(list_of_packages, 
       function(x) if(!require(x,character.only = TRUE)) install.packages(x))

and then run the command:

runGitHub( "mapper-GUI", "vituri")

