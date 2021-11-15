library(shiny)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}


# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    position = "left",
    sidebarPanel(
      # task2: add in the inputs in the sidebarPanel
        selectInput("SelectInput", "Choose a Book", books),
        checkboxInput("CheckBoxInput", "Remove Stopwords?", value = TRUE),
        actionButton("ActionButtonInput", "Run"),
        hr(),
        h3("Word Cloud Settings"),
        sliderInput("Max", "Max Number of Words", min = 10, max = 200, value = 100, step = 10),
        sliderInput("Largest", "Size of Largest Words:", min = 1, max = 8, value = 4),
        sliderInput("Smallest", "Size of Smallest Words:", min = .1, max = 4, value = .5),
        hr(),
        h3("Word Count Settings"),
        sliderInput("Min", "Minimum words for Count Chart", min = 10, max = 100, value = 25),
        sliderInput("Font", "Word size for Count Chart", min = 8, max = 30, value = 14)
    ),
    mainPanel(
      # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
      tabsetPanel(
        type = "tabs",
        tabPanel("Word Cloud", plotOutput("cloud", height = "600px")),
        tabPanel("Word Counts", plotOutput("freq", height = "600px"))
    )
    )
  )
  # task6: and modify your figure heights
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq = eventReactive(
    input$ActionButtonInput,
    {
      withProgress({
        setProgress(message = "Processing corpus...")
        getFreq(input$SelectInput, input$CheckBoxInput)})
    }
  )
  
  output$cloud = renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$Largest, input$Smallest),
          random.order = FALSE, 
          max.words = input$Max, 
          colors=pal))
  })

output$freq = renderPlot({
  v <- freq()
  v %>%
    filter(n > input$Min) %>%
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip() +
    theme(text = element_text(size=input$Font)) +
    labs(x = '', y = '')
  
})

}

shinyApp(ui = ui, server = server)
