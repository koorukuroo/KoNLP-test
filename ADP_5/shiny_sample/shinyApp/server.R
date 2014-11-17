library(shiny)
library(ggplot2)

theme<-theme_bw()+theme(panel.grid=element_blank())

shinyServer(function(input, output) {
  data <- reactive({
    movies <- movies[movies$year %in% seq(as.integer(input$start), as.integer(input$end)),]
    movies
  })
  
  output$plot <- renderPlot({
    theGraph <- ggplot(data(), aes(x=year,y=rating))+geom_point(alpha=input$alphaSlider) + theme
    if(input$smooth){
      theGraph <- theGraph + geom_smooth()
    }
    print(theGraph)
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
  
  output$table <- renderDataTable({
    data()[,c(1:2,5:16)]
  }, options=list(aLengthMenu=c(5,30,50),iDisplayLength=100))
  
})
