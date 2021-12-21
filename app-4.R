library(shiny)
library(data.table)
library(plotly)
library(dplyr)
library(qdapTools)
options(shiny.fullstacktrace=FALSE)
all_movies <- read.csv("all_movies.csv", na.strings=c("","NA"))

## fill NA in 'BoxOffice' column with zero
all_movies$BoxOffice[is.na(all_movies$BoxOffice)] <- 0

## fill NA in 'Rated' column with 'Unrated'
all_movies$Rated <- as.character(all_movies$Rated)
all_movies$Rated[is.na(all_movies$Rated)] <- 'Unrated'

## Set a threshold: If movies with a given rating are less than the threshold, those movies' rating will be re-classified as "Other".
threshold = 10
above_threshold <- all_movies %>% count(Rated) %>% filter(n >= threshold)
all_movies$Rated[!all_movies$Rated %in% above_threshold$Rated] <- "Other"
all_movies %>% count(Rated)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("A movie explorer"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        label = "Year released",
                        min = min(all_movies["Year"]),
                        max = max(all_movies["Year"]),
                        value = c(min,max),
                        sep = ""
            ),
            
            selectInput("genre", 
                        label = "Genre", 
                        choices = c("All", "Action", "Adventure", "Animation", "Biography","Comedy", 
                                  "Crime", "Documentary", "Drama", "Family", "Fantasy", "Film-Noir", 
                                  "History","Horror", "Music", "Musical", "Mystery", "News", "Romance", 
                                  "Sci-Fi", "Short", "Sport", "Thriller", "War", "Western"),
                        selected = "All"
            ),
           
            selectInput("rating", 
                        label = "Rated", 
                        choices = c('All',sort(unique(all_movies$Rated))),
                        selected = "All"
            ),
            
            sliderInput("oscars", 
                        label = "Bare minimum of Oscar wins, across all categories",
                        min = 0, 
                        max = 5, 
                        value = 0, 
                        step = 1
            ),
            textInput("cast", "Cast names contains (e.g. Ryan Gosling)")
        ),
  
        mainPanel(
            navbarPage(
                "Graphs: Box office revenue VS User review ratings",
                tabPanel("Rating 10",
                         fluidPage(
                             plotlyOutput("plot1"),
                             HTML("<br><br><br>"),
                             verbatimTextOutput("n_movies1"),
                         )
                ),
                tabPanel("Rotten Tomatoes",
                         fluidPage(
                             plotlyOutput("plot2"),
                             HTML("<br><br><br>"),
                             verbatimTextOutput("n_movies2"),
                         )
                )
            ), collapsible = TRUE
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Filter the movies, returning a data frame
    filtered <- reactive({
      minyear <- input$year[1]
      maxyear <- input$year[2]
      oscars <- input$oscars
      
      m <- subset(all_movies, Oscars >= oscars & Year >= minyear & Year <= maxyear)
      
      # filter by genre
      if (input$genre != "All") {
        m <- m %>% filter(Genre %like% input$genre)
      }
      
      # filter by rating
      if (input$rating != "All") {
        m <- m %>% filter(Rated %in% input$rating)
      }
      
      # Optional: filter by cast member
      if (!is.null(input$cast) && input$cast != "") {
        m <- m %>% filter(Cast %like% input$cast)
      }
      
      m <- as.data.frame(m)
      
    })
    

    output$plot1 <- renderPlotly({
      filtered() %>% plot_ly(x=~jitter(Rating10), y=~BoxOffice / 10^6, type = "scatter", 
                             hoverinfo = "text", mode = "markers", 
                             marker = list(color = "turquoise", 
                                           line = list(color = "purple", width = 1), 
                                           size = 8, opacity = 0.6), 
                             text = ~paste("Rating10: ", Rating10, "<br>Box Office: $", BoxOffice / 10^6, 
                                           "Million", "<br>Title: ", Title)) %>%
        layout(xaxis = list(title = "10-pt Rating10", zeroline = FALSE), 
               yaxis = list(title = "Box Office Revenue (Million)", zeroline = FALSE),
               annotations = list(x = 1, y = -0.1, 
                                  text = "Note: Box office revenue = 0 means there is no data values", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="red"))
        )
      
    })
    
    output$plot2 <- renderPlotly({
      filtered() %>% plot_ly(x=~jitter(Rotten), y=~BoxOffice / 10^6, type = "scatter", 
                             hoverinfo = "text", mode = "markers", 
                             marker = list(color = "yellowgreen", 
                                           line = list(color = "purple", width = 1), 
                                           size = 8, opacity = 0.6),
                             text = ~paste("Rotten Tomatoes: ", Rotten, "<br>Box Office: $", BoxOffice / 10^6,
                                           "Million", "<br>Title: ", Title)) %>%
        layout(xaxis = list(title = "Rotten Tomatoes", zeroline = FALSE), 
                    yaxis = list(title = "Box Office Revenue (Million)", zeroline = FALSE),
                    annotations = list(x = 1, y = -0.1, 
                                       text = "Note: Box office revenue = 0 means there is no data values",
                                       showarrow = F, xref='paper', yref='paper', 
                                       xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                       font=list(size=10, color="red"))
             )
      
    })
    
    output$n_movies1 <- renderText({ paste("Number of movies slected: ", nrow(filtered())) })
    output$n_movies2 <- renderText({ paste("Number of movies slected: ", nrow(filtered())) })
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
