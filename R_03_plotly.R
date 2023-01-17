###################################################
### 01
###################################################

# Using iris dataset create a scatterplot showing dependency between Sepal.Length
# and Sepal.Width. Color should depend on the species of the individual, and
# the size of a point on the area (Petal.Length x Petal.Width).
# Save the obtained graph to html file.

library(tidyverse)
library(plotly)

?iris
head(iris)

(iris_chart <- plot_ly(iris) %>%
    add_trace(
      x = ~Sepal.Length,
      y = ~Sepal.Width,
      type = 'scatter',
      mode = 'markers',
      color = ~factor(Species),
      size = ~(Petal.Length * Petal.Width),
      hoverinfo = 'text',
      text = ~paste0(
        '<b>', Species,'</b>',
        '\nSepal Length: ', Sepal.Length, ' cm',
        '\nSepal Width: ', Sepal.Width, ' cm',
        '\nPetal Length: ', Petal.Length, ' cm',
        '\nPetal Width: ', Petal.Width, ' cm')
    ) %>%
    layout(
      xaxis = list(title = 'Sepal Length [cm]'), 
      yaxis = list(title = 'Sepal Width [cm]')
    ))

getwd()
# setwd('C:/Users/User/OneDrive/Edu/R/Podyplomowe R/R')
htmlwidgets::saveWidget(iris_chart, 'iris_chart.html')



###################################################
### 02
###################################################

# Using the mtcars dataset, prepare a pie chart showing car count
# of a given number of cylinders and type of engine.

library(wesanderson)
wes_palettes
my_col <- wes_palette('Cavalcanti1', type = 'discrete', n = 5)

?mtcars

(cars_chart <- mtcars %>%
    tibble::rownames_to_column('model') %>%
    select(model,cyl,vs) %>%
    mutate(n = 1,
           vs = case_when(vs == '0' ~ 'V-shaped',
                          vs == '1' ~ 'Straight')) %>%
    unite(engine, c(cyl, vs), sep = " | ", remove = TRUE) %>%
    group_by(engine) %>%
    summarise(cars = paste(model,
                           collapse=' \n'),
              n = n()) %>%
    
    plot_ly() %>%
    add_trace(
      labels = ~engine,
      marker = list(colors = my_col),
      values = ~n,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'text',
      text = ~paste(cars)))



getwd()
# setwd('C:/Users/User/OneDrive/Edu/R/Podyplomowe R/R')
htmlwidgets::saveWidget(cars_chart, 'cars_chart.html')


###################################################
### 03
###################################################

# Create violin plot showing distribution of petal length
# among different iris species.
# Add axis labels and box plot.

library(plotly)

?iris

plot_ly(iris) %>%
  add_trace(x = ~Species,
            y = ~Petal.Length,
            box = list(visible = TRUE),
            type = 'violin') %>%
  layout(xaxis = list(title = 'Species [cm]'),
         yaxis = list(title = 'Petal length'))
