library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(reshape2)
library(forecast)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Covid App"),
    mainPanel(
              
              uiOutput('tabs'),
              uiOutput('slider')

    )
)


server <- function(input, output, session) {
    
    output$mymap <- renderLeaflet({
        leaflet() %>% addTiles()
    })
    
    observe({
      data = read.csv('covid-data.csv')
        data_dates = data[,5:96]
        rad = apply(data_dates,2,sqrt)/10
        leafletProxy("mymap", data = data)  %>%
            clearMarkers %>%
            addCircleMarkers(lng = ~Long, lat = ~Lat, weight = 1,
                             radius = ~(rad[,input$slider1]), popup= paste("Region:", data$Country.Region,"<br>",
                                                                           "Infected", data_dates[,input$slider1]
                                                                                  )
            )
    })
    
    df = reactive({
      data = read.csv('covid-data.csv')
      return (data)
      
    })
    continents  =
    output$continents <- renderPlot({
      
      data = df()
      
      data_dates = data[,5:96]
      dates = colnames(data_dates)
      Continents <- data %>% group_by(Region.Name) %>% summarise_at(dates,sum)
      continentNames = vector()
      for (continent in Continents$Region.Name){
        continentNames = c(continentNames,continent)
      }
      continentNames[1] = "Other"
      numbers =Continents[,2:93]
      numbers = t(numbers)
      numbers = data.frame(numbers)
      colnames(numbers) = continentNames
      rownames(numbers) = 1:92
      days = 1:92
      Continents_graph = data.frame(days,numbers)
      
      continents.melted <- melt(Continents_graph, id = "days")
      ggplot(data = continents.melted, aes(x = days, y = value, color = variable)) +
       geom_line() + xlab(label = "Days since 22.01.2020") + ylab(label = 'Number of cases') +
       geom_vline(xintercept = input$slider1, linetype="dotted",size = 1.5) 
                  

    })
    output$forecast = renderPlot({
      tryCatch({
        data = df()
        data_dates = data[,5:96]
        dates = colnames(data_dates)
        Countries <- data %>% group_by(Country.Region) %>% summarise_at(dates,sum)
        countryNames = vector()
        for (country in Countries$Country.Region){
          country = gsub("[^(a-zA-Z)|//)]", "", country)
          country = gsub("\\)|\\(", "", country)
          countryNames = c(countryNames,country)
          
        }
        numbers =Countries[,2:93]
        numbers = t(numbers)
        numbers = data.frame(numbers)
        colnames(numbers) = countryNames
        rownames(numbers) = 1:92
        days = 1:92
        Countries_graph = data.frame(days,numbers)
        fcast = forecast(Countries_graph[,input$country])
        
        plot(fcast, main = c(input$country, "forecast"), ylab = "Number of cases", xlab= 'Days since outbrake 22.01.2020')
        points(x = input$slider1, y = Countries_graph[input$slider1,input$country])
        text(x = input$slider1, y = Countries_graph[input$slider1,input$country], labels='Slider position', cex= 0.7,pos= 3)
        
      
 
    
    },error = function(e) {
      
      #showModal(modalDialog(
      #  title = "Important message",
      #  "Some countries have no data"
      #))
      
    })})
    output$world = renderPlot({
      data = df()
      data_dates = data[,5:96]
      allCases = colSums(data_dates)
      allCases = data.frame(allCases)
      ggplot(allCases, aes(x = 1:92,y = allCases))+geom_line()+ labs(title = "All cases since 22.01.2020")+
        geom_point(aes(,x = input$slider1, y = allCases[input$slider1])) +
        geom_text(aes(label='Slider position',x = input$slider1-2, y = allCases[input$slider1]+100000)) +
        ylab(label = "Number of cases") + xlab(label = "Days")
      
      
    })
    
    output$numbers = renderTable({
      data = df()
      data_dates = data[,5:96]
      dates = colnames(data_dates)
      Countries <- data %>% group_by(Country.Region) %>% summarise_at(dates,sum)
      countryNames = vector()
      for (country in Countries$Country.Region){
        countryNames = c(countryNames,country)
      }
      numbers =Countries[,2:93]
      numbers = t(numbers)
      numbers = data.frame(numbers)
      colnames(numbers) = countryNames
      rownames(numbers) = 1:92
      days = 1:92
      Countries_graph = data.frame(days,numbers)
      
      
    })
   
    output$pickCountry <- renderUI({
      data = df()
      data_dates = data[,5:96]
      dates = colnames(data_dates)
      Countries <- data %>% group_by(Country.Region) %>% summarise_at(dates,sum)
      countryNames = vector()
      for (country in Countries$Country.Region){
        country = gsub("[^(a-zA-Z)]", "", country)
        country = gsub("\\)|\\(", "", country)
        countryNames = c(countryNames,country)
      }
      
      selectInput('country', 'Country', countryNames, selected = countryNames[1], multiple = FALSE, selectize = TRUE)
    })
    output$tabs = renderUI({
      tabsetPanel(type = "tabs",
                  tabPanel('Map',leafletOutput("mymap")),
                  tabPanel("World", plotOutput("world")),
                  tabPanel("Continents", plotOutput("continents")),
                  tabPanel("Countries", uiOutput('pickCountry'),plotOutput("forecast",width = 600, height = 600))
      )
    })
    output$slider = renderUI({
      sliderInput("slider1", h3("Days since 22.01.2020"),
                  min = 1, max = 91, value = 1)
    })
    output$noData = renderUI({
      
      
    })
   
    
}


shinyApp(ui, server)