library(shiny)
library(dplyr)
library(shinythemes)


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

url <- "https://www.pnwherbaria.org/data/getdataset.php?File=UBC_Fungi_Native.zip"

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("UBC Fungi Native Dataset Summary"),

  sidebarLayout(
    sidebarPanel(
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
      textOutput("count"),

      div(
        style = "overflow-x: auto; overflow-y: auto; height: 600px; width: 100%;",
        tableOutput("table")
      )
      #tableOutput("table")
    )
  )
)

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
        filter(grepl(input$genus, Genus, ignore.case = TRUE))
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
}

shinyApp(ui, server)
