# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Monty Hall problem",
                  tabPanel("Approach1 Switching", selectInput("userpick","Pick a door",c(1,2,3)),
                           mainPanel(
                             h1("Here is the probility graph for switching your choices"),
                             tabPanel("Plot", plotlyOutput("plot")),
                             
                             h4("Select how many times you wanna run"),
                             #tableOutput("txtout"),        
                             sliderInput("range1", 
                                         label = "Number of trials:",
                                         min = 3000, max = 30000, value =3001, step=100)
                             
                           )
                  ), # Navbar 1, tabPanel
                  
                  tabPanel("Approach1 No Switching", selectInput("userpick","Pick a door",c(1,2,3)),
                           mainPanel(
                             h1("Here is the probility graph for not switching your choices"),
                             tabPanel("Plot", plotlyOutput("plot1")),
                             
                             h4("Select how many times you wanna run"),
                             #tableOutput("txtout1"),        
                             sliderInput("range1", 
                                         label = "Number of trials:",
                                         min = 3000, max = 30000, value =3001, step=100)
                             
                           )
                  ),
                  tabPanel("Approach2 Switching",
                           mainPanel(
                             h1("Here is the probility graph for switching your choices"),
                             h3("Users pick door 1 ONLY"),
                             tabPanel("Plot", plotlyOutput("plot2")),
                             
                             h4("Select how many times you wanna run"),
                             #tableOutput("txtout2"),        
                             sliderInput("range1", 
                                         label = "Number of trials:",
                                         min = 3000, max = 30000, value =3001, step=100)
                             
                           )
                  ),
                  tabPanel("Approach2 No Switching",
                           mainPanel(
                             h1("Here is the probility graph for switching your choices"),
                             h3("Users only pick door 1"),
                             tabPanel("Plot", plotlyOutput("plot3")),
                             
                             h4("Select how many times you wanna run"),
                             sliderInput("range1", 
                                         label = "Number of trials:",
                                         min = 3000, max = 30000, value =3001, step=100)
                             
                           )
                  ),
                  tabPanel("Explanation",
                           mainPanel( tags$img(src="ShinyProjpic1.png"),
                             h1("From Approach1 and Approach2"),
                             h3("Two approaches have same ideas"),
                             h4("Statistically showing that after running thousnads of times, swtiching win rate is much higher than
                                not switching. That is because if you switch your choice, then it is like you picked two doors. If 
                                you do not switch doors, then it does not matter if the host open a door or not. The probility of 
                                winning is always 1/3."),tags$img(src="ShinyProjpic2.png",width= "900px", height="500px"),
                           )
                  
                  
                ) # navbarPage
)) # fluidPage

#renderTable({switching(input$userpick,input$range1)})

# Define server function  
server <- function(input, output) {
  approach11_not_switching <- reactive({
    approach1_not_switching <- tibble("winorlose" = character())
    for (i in seq(input$range1)){
      car <- sample(1:3,1,replace = TRUE)
      if (input$userpick == car){
        approach1_not_switching <- add_row(approach1_not_switching, "winorlose" = "win")}
      else{
        approach1_not_switching <- add_row(approach1_not_switching, "winorlose" = "lose")}
    }
    return(approach1_not_switching)
  })
  
  
    approach11_switching <- reactive({
      approach1_switching <- tibble("winorlose" = character())
      for (i in seq(input$range1)){
        car <- sample(1:3,1,replace = TRUE)
        if (input$userpick != car){
          approach1_switching <- add_row(approach1_switching, "winorlose" = "win")}
        else{
          approach1_switching <- add_row(approach1_switching, "winorlose" = "lose")}
      }
      return(approach1_switching)
    })
    
    approach22_not_switching <- reactive({
      approach2_not_switching <- tibble("winorlose" = character())
      for (i in seq(input$range1)){
        car <- sample(1:3,1,replace = TRUE)
        if (car == 1){
          approach2_not_switching <- add_row(approach2_not_switching, "winorlose" = "win")}
        else{
          approach2_not_switching <- add_row(approach2_not_switching, "winorlose" = "lose")}
      }
      return(approach2_not_switching)
    })
    
    approach22_switching <- reactive({
      approach2_switching <- tibble("winorlose" = character())
      for (i in seq(input$range1)){
        car <- sample(1:3,1,replace = TRUE)
        if (car != 1){
          approach2_switching <- add_row(approach2_switching, "winorlose" = "win")}
        else{
          approach2_switching <- add_row(approach2_switching, "winorlose" = "lose")}
      }
      return(approach2_switching)
    })
    
  output$plot <-  renderPlotly({plot_ly(approach11_switching(), x = ~winorlose)})
  output$plot1 <-  renderPlotly({plot_ly(approach11_not_switching(), x = ~winorlose)})
  output$plot2 <-  renderPlotly({plot_ly(approach22_switching(), x = ~winorlose)})
  output$plot3 <-  renderPlotly({plot_ly(approach22_not_switching(), x = ~winorlose)})

  #output$txtout1 <- renderTable({not_switching()})
  
  
}



# Create Shiny object

shinyApp(ui = ui, server = server)