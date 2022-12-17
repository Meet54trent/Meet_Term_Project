
# Term Project - Trends in Income and Demographics


library(shiny)
library(tidyverse)
library(rio)
library(ggthemes)

person_info=read.csv("personinfo.csv")
head(person_info)

# Application UI Layout
ui=shinyUI(fluidPage( style = "background: lightpink",
  br(),
 
  titlePanel("Trends in Income and Demographics"),
  p("Explore the differences between those who make less than $50,000 and those who make more than $50,000. You can search for specific demographic data after filtering the data by country."),
  
  
  fluidRow(style = "background: lightpink",
    column(12, 
           wellPanel(style = "background: lightgreen", selectInput("country","Select Country",choices=c("United-States","Canada","Mexico","Germany","Philippines")))
    )   
  ),
  
  
  fluidRow(column(3, 
                  wellPanel(style = "background: lightblue",
                    p("Select a continuous variable and graph type (Histogram or Boxplot) to view on the right."),
                    radioButtons("continuous_variable","Continuous",choices=c("age","hours_per_week")),   
                    radioButtons("graph_type","Graph",choices=c("Histogram","Boxplot"))    
                  )
  ),column(9, plotOutput("p1"))  
  ),
  
  
  fluidRow(
    column(3, 
           wellPanel( style = "background: lightblue",
             p("Select a categorical variable to view bar chart on the right. Use the check box to view a stacked bar chart to combine the income levels into one graph. "),
             radioButtons("categorical_variable","Category",choices=c("education","workclass","sex")),    
             checkboxInput("is_stacked","Stack Bar",value=FALSE)     
            
           )
    ),
    column(9, plotOutput("p2"))  
  )
)
)

# Define server logic
server=shinyServer(function(input, output) {
  person_info=import("personinfo.csv")               
  names(person_info)=tolower(names(person_info))      
  
  df_country <- reactive({
    person_info %>% filter(native_country == input$country)
  })
  
  
  output$p1 <- renderPlot(
                          {
    if (input$graph_type == "Histogram") {
      
      ggplot(df_country(), aes_string(x =input$continuous_variable)) +
        geom_histogram(color="black",fill="red") +  
        labs(y="Number of People", title=paste("Trend of ",input$continuous_variable)) +  
        facet_wrap(~prediction)+ 
        theme_dark()
    } else {
      
      ggplot(df_country(), aes_string(y = input$continuous_variable)) +
        geom_boxplot(color="chocolate",fill="darkblue") +  
        coord_flip() +  
        labs(x="Number of People", title=paste("Boxplot of",input$continuous_variable)) +  
        facet_wrap(~prediction)+    
        theme_dark()
    }
    
  })
  
  

  output$p2 <- renderPlot( 
                      {
    
    p <- ggplot(df_country(), aes_string(x =input$categorical_variable)) +
      labs(y="Number of People",title=(paste("Trend of",input$categorical_variable))) +  
      theme_dark()+
      theme(axis.text.x=element_text(angle=45),legend.position="bottom")    
    
    
    if (input$is_stacked) {
      p + geom_bar(aes(fill=prediction))  
    }
    else{
      p + 
        geom_bar(aes_string(fill=input$categorical_variable)) +  
        facet_wrap(~prediction)   
    }
  })

})
# Run the application 
shinyApp(ui = ui, server = server)
