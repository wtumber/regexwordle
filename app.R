#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(magrittr)

word_list <- read.table("./answers.txt",col.names = "words")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Regex Wordle"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          textInput("regex_to_match",label = "Letter placement...",placeholder = "....."),
          textInput("regex_contains",label = "Word contains letters",placeholder = ""),
          textInput("regex_does_not_contain",label = "Word does not contain letters",placeholder = ""),
            shiny::actionButton("send","Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("results"),
          textOutput("words")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  processed_regex <-  eventReactive(input$send, {
    full_regex <- paste0(input$regex_to_match)
    
    if(nchar(full_regex) > 0){
      if(strsplit(full_regex,"")[nchar(full_regex)] != "$"){
        full_regex <- paste0(full_regex,"$")
      }
      
      if(grepl("…",full_regex,fixed=TRUE)){
        full_regex <- gsub("…","...",full_regex)
      }
    }
    
    if(nchar(input$regex_contains) > 1){
      
      must_contain_regex <- sapply(strsplit(input$regex_contains,""), function(x){
        return(paste0("(?=.*",x,")"))
      })
      
      must_contain_regex <- paste0(must_contain_regex,collapse="")
      
      full_regex <- paste0(c(must_contain_regex,full_regex),collapse="")
      
      
    }
    
    if(nchar(input$regex_does_not_contain) > 1){
      
      full_regex <- paste0(c(paste0("(?!.*[",input$regex_does_not_contain,"])"),full_regex),collapse="")
      
      
    }
    
    print(full_regex)
    
    full_regex
    
    
  })
  
  
  valid_words <- reactive({
    
    full_regex <- processed_regex()
    
    does_word_match_regex <- sapply(word_list$words, function(x) grepl(full_regex,x,ignore.case=TRUE,perl=TRUE) )
    
    
    word_list[does_word_match_regex,1]
  })
  
  
  df <- reactive({
    
    matching_letter_matrix <- data.frame(do.call(rbind, strsplit(valid_words(),split = "")))
    
    df <- matching_letter_matrix %>% 
      tidyr::gather() %>% 
      dplyr::count(key, value, name = "n") %>% 
      dplyr::arrange(key,desc(n))%>%
      dplyr::group_by(key) %>%
      dplyr::slice_max(n, n = 10) 
    
  })
  
  
    output$results <- renderPlot({
      
      ggplot2::ggplot(df(), ggplot2::aes(x = n, y = value)) + 
        ggplot2::geom_bar(stat = "identity") + 
        ggplot2::facet_wrap(~ key , scales = "free",ncol=5) + 
        ggplot2::labs(x = "Count", y = "Value",title = processed_regex()) + 
        ggplot2::theme_bw()
      
      
    })
    
    output$words <- renderText({
      valid_words()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
