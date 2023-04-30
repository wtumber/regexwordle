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
            textInput("regex_to_match",label = "Input regex",placeholder = ".....")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("results")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$results <- renderPlot({
      does_word_match_regex <- sapply(word_list$words, function(x) grepl(input$regex_to_match,x,ignore.case=TRUE,perl=TRUE) )
      
      matching_letter_matrix <- data.frame(do.call(rbind, strsplit(word_list[does_word_match_regex,1],split = "")))
      
      df <- matching_letter_matrix %>% 
        tidyr::gather() %>% 
        dplyr::count(key, value, name = "n") %>% 
        dplyr::arrange(key,desc(n))%>%
        dplyr::group_by(key) %>%
        dplyr::slice_max(n, n = 10) 
      
      
      ggplot2::ggplot(df, ggplot2::aes(x = n, y = value)) + 
        ggplot2::geom_bar(stat = "identity") + 
        ggplot2::facet_wrap(~ key , scales = "free",ncol=5) + 
        ggplot2::labs(x = "Count", y = "Value") + 
        ggplot2::theme_bw()
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
