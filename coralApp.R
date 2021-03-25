library(shiny)                           #load all necessary libraries
library(shinyWidgets)
library(ggplot2)
library(leaflet)
setwd("C:/Users/manas/Desktop/github projects/R shiny app")
d <- read.csv("coralbleach.csv",stringsAsFactors = FALSE)
d$bleaching=as.numeric(sub("%","",d$bleaching))
d$latitude=as.numeric(d$latitude)
#rename column year correctly
names(d)[1] <- "year"
d$type <- trimws(d$type, which = c("both"))       # remove leading and trailing  whitespaces

#UI SCRIPT

Ui<-fluidPage(
  
  titlePanel("Coral Bleaching at the Great Barrier Reef "),        #use fluid page to create a layout
  sidebarLayout(
    sidebarPanel(
      selectInput("coral_type","Coral Type",c("blue corals"='blue corals',"hard corals"='hard corals',"soft corals" = "soft corals","sea pens"= "sea pens" , "sea fans" = "sea fans")),
      selectInput("smoother","Smoother Type",
                  c("Linear Model"='lm',"Generalised Linear Model"='glm',
                    "Generalized Addictive Model"='gam', "Local Regression"='loess'
                  ))
      ),
      mainPanel(
      plotOutput("percentage_of_bleaching"))                    #outputids to identify inputs

  ),
  leafletOutput(outputId = "great_barrier_reef")
  )
  
#SERVER SCRIPT

server<-function(input, output){
  
  output$percentage_of_bleaching <- renderPlot({                        #renders plot outputs based on inputs through widgets
      df_subset = subset(d, type == input$coral_type)
      k=ggplot(data = df_subset, aes(x= year,y = bleaching))+ geom_point(aes(color=type))+facet_wrap(type~site)+geom_smooth(method = input$smoother,formula = y~x)+theme_light()
      print(k)
  
  })
  
  output$great_barrier_reef<-renderLeaflet({

      leaflet(data = subset(d,type == input$coral_type))%>%
        addTiles()%>%
        addMarkers(lat=~latitude,lng = ~longitude,popup = ~site)%>%
        addCircleMarkers(lat=~latitude,lng = ~longitude)
      
  
  
  
})
}
shinyApp(ui = Ui, server = server)                      #runs app integrating both server and ui scripts
