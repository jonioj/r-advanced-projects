
library(ggplot2)
library(shiny)
library(gganimate)
library (ggiraph)
library(patchwork)
library(dplyr)
heart = read.csv('heart.csv')
heart$name = row.names(heart)

theme_set(theme_bw())



ui <- basicPage(
  titlePanel("Project 4"),
  mainPanel(uiOutput('tabs')))

server <- function(input, output) {
  output$tabs = renderUI({
    tabsetPanel(type = "tabs",
                tabPanel('Animated',imageOutput("plot1")),
                tabPanel("Interactive", girafeOutput("plot2"))
    )
  })
  
  output$plot2 = renderGirafe({
    gg_1 <- ggplot(heart) + geom_point_interactive (aes (x = age,
                                                              y = chol,
                                                              color = factor(sex),
                                                              data_id = name
                                                              )) +theme_bw()
    gg_2 <- ggplot(heart) + geom_point_interactive (aes (x = age,
                                                         y = trestbps,
                                                         color = factor(sex),
                                                         data_id = name)) +theme_bw()
    girafe(code = print(gg_1 + gg_2), width_svg = 8, height_svg = 4) %>% girafe_options(opts_hover(css = "fill:wheat;stroke:orange;r:5pt;"))
    
   })

  output$plot1 <- renderImage({
        list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
        )}, deleteFile = FALSE)}

shinyApp(ui, server)