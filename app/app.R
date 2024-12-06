library(shiny)
library(dplyr)
library(ggplot2)
library(webshot)
library(htmlwidgets)
library(wordcloud2)

#### Helper Functions ####

# a simple helper function to scrape the chosen data set from the website that is hosting it
scrape_dataset <- function(url) {
  temp <- tempfile(fileext = ".zip")
  download.file(url, temp, mode = "wb")

  temp_dir <- tempdir()
  unzip(temp, exdir = temp_dir)

  file <- file.path(temp_dir, "occurrences.txt")

  data <- read.delim(
    file,
    stringsAsFactors = FALSE,
    quote = "",
    fill = TRUE,
    na.strings = c("", "NA"))

  chosen_cols <- c("OccurrenceID", "Accession", "Genus", "SpecificEpithet", "YearCollected", "SpecimenNotes")
  data <- data %>%
    select(any_of(chosen_cols))

  unlink(temp)

  return(data)
}

# a helper function for generating a wordcloud
generate_word_cloud <- function(data, column) {
  word_freq <- table(data[[column]])
  wordcloud2(data.frame(word = names(word_freq), freq = as.numeric(word_freq)))
}

# a helper function for generating a bar chart
generate_bar_chart <- function(data, column) {
  word_freq <- table(data[[column]])
  word_freq <- head(sort(word_freq, decreasing = TRUE), 10)

  # change the table to a data frame
  df <- data.frame(word = names(word_freq), freq = as.numeric(word_freq))

  # generate the plot
  ggplot(df, aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity", fill = "#556b2f") +
    theme_minimal() +
    labs(title = paste("Counts of the Top Selections", column),
         x = column,
         y = "Frequency") +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    ) +
    coord_flip()
}

url <- "https://www.pnwherbaria.org/data/getdataset.php?File=UBC_Fungi_Native.zip"

#### UI ####

ui <- fluidPage(
  includeCSS("www/theme.css"),
  fluidRow(
    column(2, img(src = "fungi_image.png", height = 80)),
    column(10, h2("UBC Fungi Native Dataset Summary"))
  ),

  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      helpText("Filter the dataset using one or more of the following columns"),
      br(),
      textInput("occurrence_id", "Search by OccurenceID:", ""),
      textInput("genus", "Search by Genus:", ""),
      sliderInput("year_collected", "Search by YearCollected:",
                  min = 1850, max = 2023,
                  value = c(1850, 2023),
                  step = 1),
      br(),
      selectInput("sort_column", "Sort by:",
                  choices = c("OccurrenceID", "Accession", "Genus", "SpecificEpithet", "YearCollected")),
      radioButtons("sort_order", "Sort order:",
                   choices = c("Ascending" = "asc", "Descending" = "desc"))

    ),
    mainPanel(
      class = "main-content",
      tabsetPanel(
        id = "tabset1",
        tabPanel(
          "Data Table",
          textOutput("count"),

          div(
            style = "overflow-x: auto; overflow-y: auto; height: 600px; width: 100%;",
            tableOutput("table")
          )
        ),
        tabPanel(
          "Data Visualization",
          fluidRow(
            column(4,
              radioButtons("viz_type", "Choose visualization:",
                           choices = c("Word Cloud", "Bar Chart"))
            ),
            column(4,
              selectInput(
                "visual_column",
                "Choose column:",
                choices = c("Genus", "SpecificEpithet"))
            ),
            column(4,
              downloadButton("downloadVisual", "Download Visualization"))
          ),

          conditionalPanel(
            condition = "input.viz_type == 'Word Cloud'",
            wordcloud2Output("wordcloud", width = "100%", height = "600px")
          ),
          conditionalPanel(
            condition = "input.viz_type == 'Bar Chart'",
            plotOutput("barchart")
          )
        )
      )
    )
  )
)

#### Server ####

server <- function(input, output) {
  # loading the data reactively from the url chosen above
  load_data <- reactive({
    # have a progress meter
    withProgress(message = 'Loading data...', value = 0.1, {
      # surround in a try/catch block to throw an error if the dataset can not be scraped
      tryCatch({
        fungi_dataset <- scrape_dataset(url)
        incProgress(0.9)
        fungi_dataset
      }, error = function(e) {
        showNotification(
          paste("Failed to load data:", e$message),
          type = "error"
        )
        NULL
      })
    })
  })

  # feature: filtering the data by the desired values. This helps the user to
  # a specific entry within the UBC fungi dataset, and explore the data more
  # efficiently.
  filter_data <- reactive({
    filtered_data <- load_data()

    # filter based on the OccurrenceID
    if (input$occurrence_id != "") {
      filtered_data <- filtered_data %>%
        filter(grepl(input$occurrence_id, OccurrenceID, ignore.case = TRUE))
    }

    # filter based on the Genus
    if (input$genus != "") {
      filtered_data <- filtered_data %>%
        filter(grepl(tolower(input$genus), tolower(Genus)))
    }

    # filter based on the YearCollected
    filtered_data <- filtered_data %>%
      filter(YearCollected >= input$year_collected[1] & YearCollected <= input$year_collected[2])

    return(filtered_data)
  })

  # feature: sorting the data by any one column (except SpecimenNotes). Once
  # again aids the user in the exploration of the data, making it easier to
  # navigate a large dataset.
  sort_data <- reactive({
    sorted_data <- filter_data()

    # dynamically read in the designated sort_column values
    column_name <- sym(input$sort_column)

    # check if asc or desc is specified, use !!column_name to ignore any quotations
    if (input$sort_order == "desc") {
      sorted_data <- sorted_data %>%
        arrange(desc(!!column_name))
    } else {
      sorted_data <- sorted_data %>%
        arrange(!!column_name)
    }


    return(sorted_data)
  })

  # feature: updates the number of results found when the filtering options are
  # changed. Useful for informing the user of how large the subset is, especially
  # given how the renderTable() function below only shows the first 1000 results
  # in order to speed up loading times.
  output$count <- renderText({
    count <- filter_data()

    paste("Number of results found:", nrow(count))
  })

  # output the first 1000 results given the filtering and sorting options chosen
  output$table <- renderTable({
    dataset <- sort_data()
    head(dataset, 1000)
  })

  # feature: creates a word cloud of the most commonly used words, which can help
  # with the visualization of the number of each genus and species found given
  # the selected
  output$wordcloud <- renderWordcloud2({
    # check that the Word Cloud option is selected or return NULL
    if(input$viz_type != "Word Cloud") {
      return(NULL)
    }

    data <- filter_data()

    # check if the above data is empty
    if (length(data) == 0) {
      return(NULL)
    }

   generate_word_cloud(data, input$visual_column)
  })

  # feature: creates a bar chart that displays the counts of the 10 most common
  # selected outputs given the filtering criteria
  output$barchart <- renderPlot({
    # check that the bar chart option is selected, or return NULL
    if (input$viz_type != "Bar Chart") {
      return(NULL)
    }

    data <- filter_data()

    if (length(data) == 0) {
      return(NULL)
    }

    generate_bar_chart(data, input$visual_column)
  })

  # feature: download the generated visualization
  output$downloadVisual <- downloadHandler(
    # determine the file name based on whether or not a word cloud or bar chart
    # is currently generated
    filename = function() {
      if (input$viz_type == "Word Cloud") {
        paste("word_cloud_", Sys.Date(), ".png", sep = "")
      } else {
        paste("bar_chart_", Sys.Date(), ".png", sep = "")
      }
    },
    content = function(file) {
      if (input$viz_type == "Word Cloud") {

        temp_file <- tempfile(fileext = ".html")
        data <- filter_data()
        saveWidget(generate_word_cloud(data, input$visual_column), file = temp_file, selfcontained = TRUE)
        webshot(temp_file, file = file, vwidth = 800, vheight = 600, cliprect = "viewport")

      } else if (input$viz_type == "Bar Chart") {

        png(file)
        data <- filter_data()
        print(generate_bar_chart(data, input$visual_column))
        dev.off()

      }
    }
  )
}

shinyApp(ui, server)
