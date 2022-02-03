#-----------------------------------------------------------------------------#
#                     Validation app for spirometry data                      #
#-----------------------------------------------------------------------------#

library("cowplot")
library("gridExtra")
library("magick")
library("shiny")
library("shinyalert")
library("tidyverse")

# Load data we need to run the app
tab <- readr::read_csv("input/spiro_tables.csv")
paths <- readr::read_csv("input/image_paths.csv")
alarms <- readr::read_csv("input/alarms.csv")

# Interface ----
ui <- fluidPage(
  titlePanel("Spirometry validation"),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
      
      # Instructions
      "Welcome to the spiro validation app. ",
      "Spiro curves and tables not yet validated will be prompted. ",
      "The main screen includes participant and test identifiers and alarms (top),",
      "flow and volume curves (left), ",
      "image table (right, top) and numeric automatic extraction (right, bottom).",
      "You can choose the status of the curve and write a comment if you wish",
      "(e.g. wrong automatic number extraction), then click submit to go to the next. ",
      "To load a previous results file, do it below. Otherwise just start from scratch.  ",
      "Save the current results file whenever you want to stop. ",
      hr(),
      
      # File manager part of the interface
      radioButtons("status", label = h4("Is it a valid measurement?"),
                   choices = list("Valid" = 1, 
                                  "Only FEV1" = 2, 
                                  "Invalid" = 3,
                                  "Not sure" = 4), 
                   selected = 4),
      textInput("comment", label = h4("Comments"), value = ""),
      actionButton("submit", label = "Submit"),
      
      hr(),
      
      # Start analysis
      h4("Start from scratch"),
      actionButton("start", label = "Start"),
      hr(),
      
      # Load previous results
      fileInput("file", label = h4("Load previous results")),
      actionButton("load", label = "Load"),
      hr(),
      
      # Save current results
      h4("Save file"),
      helpText("Save validation results to output/datetime.csv"),
      actionButton("save", label = "Save")
      
    ),
    
    mainPanel(width = 9,
      
      # Participant ID and curve
      strong("ParticipantID: "), textOutput("currentID", inline =TRUE),br(),
      strong("Test number: "), textOutput("currenttest", inline =TRUE),br(),
      strong("PEF alarm: "), textOutput("current_pef_check", inline =TRUE),br(),
      strong("FVC alarm: "), textOutput("current_fvc_check", inline =TRUE),br(),
      strong("FEV1 alarm: "), textOutput("current_fev1_check", inline =TRUE),br(),
      hr(),
      
      # Curves and tables,
      plotOutput("curves", height = "700px", width = "100%")
    )
    
  )
)


# Analysis ----
server <- function(input, output) {
  
  ## Start from scratch ----
  observeEvent(input$start, {
    
    # Create results tibble
    results <<- tibble(participantID = tab$participantID, 
                      testnum = tab$testnum, 
                      status = "", 
                      comment = "")
    
    # Create current values
    current_ID <<- results$participantID[1]
    current_test <<- results$testnum[1]
    current_pathTable <- as.character(paths[paths$participantID == current_ID & paths$testnum == current_test, "pathTable"])
    current_curve1 <- as.character(paths[paths$participantID == current_ID & paths$testnum == current_test, "curve1"])
    current_curve2 <- as.character(paths[paths$participantID == current_ID & paths$testnum == current_test, "curve2"])
    current_pef_check <- as.character(alarms[alarms$participantID == current_ID & alarms$testnum == current_test, "pef_check"])
    current_fvc_check <- as.character(alarms[alarms$participantID == current_ID & alarms$testnum == current_test, "fvc_check"])
    current_fev1_check <- as.character(alarms[alarms$participantID == current_ID & alarms$testnum == current_test, "fev1_check"])
    
    # Parse table, ready to plot
    current_parsed <- tab[tab$participantID == current_ID & tab$testnum == current_test,] %>%
      dplyr::select(-participantID, -testnum) %>%
      tidyr::gather() %>%
      dplyr::mutate(col = ifelse(grepl("pred", key), "Pred", "Test")) %>%
      dplyr::group_by(key) %>%
      dplyr::mutate(key2 = strsplit(key, "_")[[1]][1]) %>%
      dplyr::ungroup() %>%
      dplyr::select(-key) %>%
      dplyr::rename(parameter = key2) %>%
      tidyr::spread(value = value, key = col) %>%
      dplyr::mutate(parameter = relevel(as.factor(parameter), ref = "fvc")) %>%
      dplyr::arrange(parameter)
    
    # Get identifier text to be displayed
    output$currentID <- renderText(as.character(current_ID))
    output$currenttest <- renderText(as.character(current_test))
    output$current_pef_check <- renderText(as.character(current_pef_check))
    output$current_fvc_check <- renderText(as.character(current_fvc_check))
    output$current_fev1_check <- renderText(as.character(current_fev1_check))
    
    # Draw spiros
    output$curves <- renderPlot({
      img1 <- image_read(current_curve1)
      plot1 <- image_ggplot(img1) + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
      img2 <- image_read(current_curve2)
      plot2 <- image_ggplot(img2) + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
      img3 <- image_read(current_pathTable)
      plot3 <- image_ggplot(img3) + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
      plot4 <- tableGrob(current_parsed, rows = NULL, theme = ttheme_default(base_size = 16))
      plot_grid(plot1, plot3, plot2, plot4, ncol = 2, rel_widths = c(0.6, 0.4))
    })
  })
  
  ## Submit ----
  observeEvent(input$submit, {
    
    # Enter status
    results[results$participantID == current_ID & results$testnum == current_test,]$status <<- dplyr::case_when(
      input$status == 1 ~ "Valid",
      input$status == 2 ~ "Only FEV1",
      input$status == 3 ~ "Invalid",
      input$status == 4 ~ "Not sure"
    )
    
    # Enter comment
    results[results$participantID == current_ID & results$testnum == current_test,]$comment <<- input$comment

    # Keep updating the subset of spiros with NAs
    results_na <- results[results$status == "",]
    if(nrow(results_na) == 0){
      write_csv(results, paste0("output/", gsub(":","-", Sys.time()), ".csv"))
      shinyalert(title = "Done!", 
                 text = "You have revised all available spiros. A results file 
                 has been saved in the output folder", 
                 type = "info", closeOnEsc = F, closeOnClickOutside = F, 
                 showCancelButton = F, showConfirmButton = F)
      Sys.sleep(1000000000)
    }

    # Create current values
    current_ID <<- results_na$participantID[1]
    current_test <<- results_na$testnum[1]
    current_pathTable <- as.character(paths[paths$participantID == current_ID & paths$testnum == current_test, "pathTable"])
    current_curve1 <- as.character(paths[paths$participantID == current_ID & paths$testnum == current_test, "curve1"])
    current_curve2 <- as.character(paths[paths$participantID == current_ID & paths$testnum == current_test, "curve2"])
    current_pef_check <- as.character(alarms[alarms$participantID == current_ID & alarms$testnum == current_test, "pef_check"])
    current_fvc_check <- as.character(alarms[alarms$participantID == current_ID & alarms$testnum == current_test, "fvc_check"])
    current_fev1_check <- as.character(alarms[alarms$participantID == current_ID & alarms$testnum == current_test, "fev1_check"])
    
    # Parse table, ready to plot
    current_parsed <- tab[tab$participantID == current_ID & tab$testnum == current_test,] %>%
      dplyr::select(-participantID, -testnum) %>%
      tidyr::gather() %>%
      dplyr::mutate(col = ifelse(grepl("pred", key), "Pred", "Test")) %>%
      dplyr::group_by(key) %>%
      dplyr::mutate(key2 = strsplit(key, "_")[[1]][1]) %>%
      dplyr::ungroup() %>%
      dplyr::select(-key) %>%
      dplyr::rename(parameter = key2) %>%
      tidyr::spread(value = value, key = col) %>%
      dplyr::mutate(parameter = relevel(as.factor(parameter), ref = "fvc")) %>%
      dplyr::arrange(parameter)

    # Get identifier text to be displayed
    output$currentID <- renderText(as.character(current_ID))
    output$currenttest <- renderText(as.character(current_test))
    output$current_pef_check <- renderText(as.character(current_pef_check))
    output$current_fvc_check <- renderText(as.character(current_fvc_check))
    output$current_fev1_check <- renderText(as.character(current_fev1_check))

    # Draw spiros
    output$curves <- renderPlot({
      img1 <- image_read(current_curve1)
      plot1 <- image_ggplot(img1) + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
      img2 <- image_read(current_curve2)
      plot2 <- image_ggplot(img2) + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
      img3 <- image_read(current_pathTable)
      plot3 <- image_ggplot(img3) + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
      plot4 <- tableGrob(current_parsed, rows = NULL, theme = ttheme_default(base_size = 16))
      plot_grid(plot1, plot3, plot2, plot4, ncol = 2, rel_widths = c(0.6, 0.4))
    })
  })
  
  ## Load previous results ----
  observeEvent(input$load, {
    
    results <<- read_csv(input$file$datapath)
    results$comment <<- ifelse(is.na(results$comment), "", results$comment)
    results$status <<- ifelse(is.na(results$status), "", results$status)
    
    # Keep updating the subset of spiros with NAs
    results_na <- results[results$status == "",]

    if(nrow(results_na) == 0){
      write_csv(results, paste0("output/", gsub(":","-", Sys.time()), ".csv"))
      shinyalert(title = "Done!", 
                 text = "You have revised all available spiros. A results file 
                 has been saved in the output folder", 
                 type = "info", closeOnEsc = F, closeOnClickOutside = F, 
                 showCancelButton = F, showConfirmButton = F)
      Sys.sleep(1000000000)
    }
    
    # Create current values
    current_ID <<- results_na$participantID[1]
    current_test <<- results_na$testnum[1]
    current_pathTable <- as.character(paths[paths$participantID == current_ID & paths$testnum == current_test, "pathTable"])
    current_curve1 <- as.character(paths[paths$participantID == current_ID & paths$testnum == current_test, "curve1"])
    current_curve2 <- as.character(paths[paths$participantID == current_ID & paths$testnum == current_test, "curve2"])
    current_pef_check <- as.character(alarms[alarms$participantID == current_ID & alarms$testnum == current_test, "pef_check"])
    current_fvc_check <- as.character(alarms[alarms$participantID == current_ID & alarms$testnum == current_test, "fvc_check"])
    current_fev1_check <- as.character(alarms[alarms$participantID == current_ID & alarms$testnum == current_test, "fev1_check"])
    
    # Parse table, ready to plot
    current_parsed <- tab[tab$participantID == current_ID & tab$testnum == current_test,] %>%
      dplyr::select(-participantID, -testnum) %>%
      tidyr::gather() %>%
      dplyr::mutate(col = ifelse(grepl("pred", key), "Pred", "Test")) %>%
      dplyr::group_by(key) %>%
      dplyr::mutate(key2 = strsplit(key, "_")[[1]][1]) %>%
      dplyr::ungroup() %>%
      dplyr::select(-key) %>%
      dplyr::rename(parameter = key2) %>%
      tidyr::spread(value = value, key = col) %>%
      dplyr::mutate(parameter = relevel(as.factor(parameter), ref = "fvc")) %>%
      dplyr::arrange(parameter)
    
    # Get identifier text to be displayed
    output$currentID <- renderText(as.character(current_ID))
    output$currenttest <- renderText(as.character(current_test))
    output$current_pef_check <- renderText(as.character(current_pef_check))
    output$current_fvc_check <- renderText(as.character(current_fvc_check))
    output$current_fev1_check <- renderText(as.character(current_fev1_check))
    
    # Draw spiros
    output$curves <- renderPlot({
      img1 <- image_read(current_curve1)
      plot1 <- image_ggplot(img1) + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
      img2 <- image_read(current_curve2)
      plot2 <- image_ggplot(img2) + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
      img3 <- image_read(current_pathTable)
      plot3 <- image_ggplot(img3) + theme(plot.margin = margin(0, 0, 0, 0, "cm"))
      plot4 <- tableGrob(current_parsed, rows = NULL, theme = ttheme_default(base_size = 16))
      plot_grid(plot1, plot3, plot2, plot4, ncol = 2, rel_widths = c(0.6, 0.4))
    })
    
  })
  
  ## Write current results ----
  observeEvent(input$save, {
    write_csv(results, paste0("output/", gsub(":","-", Sys.time()), ".csv"))
  })

}

# Run app
shinyApp(ui = ui, server = server)