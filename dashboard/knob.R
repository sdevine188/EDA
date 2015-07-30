
# shiny showcase
# https://www.rstudio.com/products/shiny/shiny-user-showcase/

# htmlwidgets
# https://github.com/ramnathv/htmlwidgets/blob/master/README.md

# https://github.com/htmlwidgets/knob

library(devtools)
install_github('rstudio/htmltools')
install_github('ramnathv/htmlwidgets')
install_github('htmlwidgets/knob')

library(knob)
knob(value = 20, min = 0, max = 100, 
     angleArc = 250, angleOffset = -125, 
     fgColor = "#66CC66")