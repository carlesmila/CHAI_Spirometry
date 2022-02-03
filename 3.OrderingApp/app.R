#-----------------------------------------------------------------------------#
#                      Ordering app for spirometry data                       #
#-----------------------------------------------------------------------------#

library("cowplot")
library("gridExtra")
library("magick")
library("shiny")
library("shinyalert")
library("tidyverse")

# Load data we need to run the app
paths <- read_csv("input/image_paths.csv")
spirodata <- read_csv("input/validation.csv") %>%
  filter(status %in% c("Valid", "Only FEV1", "Not sure")) %>%
  left_join(paths, by = c("participantID", "testnum"))
spirodata <- mutate(spirodata,
                    plabel = paste0("Test number: ", testnum,
                                    ", status: ", status, 
                                    ", comment: ", comment))

# Interface ----
ui <- fluidPage(
  
  titlePanel("Spirometry ordering"),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
      
      # Ordering
      h4("Curve ordering"),
      "Welcome to the spiro ordering app. ",
      "In this app, curves labelled as 'Valid', 'Not sure' and 'only FEV1' in the ",
      "first stage will be shown (invalid curves are discarded). ",
      "For each participant, select the top 3 test numbers in order of quality. ", 
      "You can also write a comment if you wish, then click submit to go to the next. ",
      "To load a previous results file, do it below. Otherwise just start from scratch.  ",
      "Save the current results file whenever you want to stop. ",
      numericInput("c1", label = h5("Best curve"), value = 0, min = 0, max = 9),
      numericInput("c2", label = h5("2nd best"), value = 0, min = 0, max = 9),
      numericInput("c3", label = h5("3th best"), value = 0, min = 0, max = 10),
      
      # Comments
      textInput("comment", label = h4("Comments"), value = ""),
      
      # Submit
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
    
    # Create results dataframe
    allID <- unique(spirodata$participantID)
    results <<- tibble(participantID = allID, option1 = 0, option2 = 0,
                      option3 = 0, comment = "")
    
    # Create initial ID and
    currentID <<- allID[1]
    curvestoplot <<- filter(spirodata, participantID == currentID)
    
    # Get identifier text to be displayed
    output$currentID <- renderText(as.character(currentID))
    
    # Draw spiros
    output$curves <- renderPlot({
      
      # Plot 1, available for all
      img11 <- image_read(curvestoplot$curve1[1])
      plot11 <- image_ggplot(img11) + theme(plot.margin = margin(0,0,0,0,"cm")) +
        ggtitle(curvestoplot$plabel[1])
      img12 <- image_read(curvestoplot$curve2[1])
      plot12 <- image_ggplot(img12) + theme(plot.margin = margin(0,0,0,0,"cm"))
      
      # Plot 2, available for all
      img21 <- image_read(curvestoplot$curve1[2])
      plot21 <- image_ggplot(img21) + theme(plot.margin = margin(0,0,0,0,"cm")) +
        ggtitle(curvestoplot$plabel[2])
      img22 <- image_read(curvestoplot$curve2[2])
      plot22 <- image_ggplot(img22) + theme(plot.margin = margin(0,0,0,0,"cm")) 
      
      # Null plots by default
      plot31 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot32 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot41 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot42 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot51 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot52 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot61 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot62 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      
      # If needed, substitute
      if(nrow(curvestoplot) > 2){ # 3 or more curves
        img31 <- image_read(curvestoplot$curve1[3])
        plot31 <- image_ggplot(img31) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[3])
        img32 <- image_read(curvestoplot$curve2[3])
        plot32 <- image_ggplot(img32) + theme(plot.margin = margin(0,0,0,0,"cm"))
        
      }
      
      if(nrow(curvestoplot) > 3){ # 4 or more curves
        img41 <- image_read(curvestoplot$curve1[4])
        plot41 <- image_ggplot(img41) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[4])
        img42 <- image_read(curvestoplot$curve2[4])
        plot42 <- image_ggplot(img42) + theme(plot.margin = margin(0,0,0,0,"cm")) 
        
      }
      
      if(nrow(curvestoplot) > 4){ # 5 or more curves
        img51 <- image_read(curvestoplot$curve1[5])
        plot51 <- image_ggplot(img51) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[5])
        img52 <- image_read(curvestoplot$curve2[5])
        plot52 <- image_ggplot(img52) + theme(plot.margin = margin(0,0,0,0,"cm")) 
        
      }
      
      if(nrow(curvestoplot) == 6){ # 6 curves
        img61 <- image_read(curvestoplot$curve1[6])
        plot61 <- image_ggplot(img61) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[6])
        img62 <- image_read(curvestoplot$curve2[6])
        plot62 <- image_ggplot(img62) + theme(plot.margin = margin(0,0,0,0,"cm"))
        
      }
      
      plot_grid(plot11, plot12,
                plot21, plot22,
                plot31, plot32,
                plot41, plot42,
                plot51, plot52,
                plot61, plot62,
                ncol = 2, 
                rel_widths = c(0.5, 0.5))
      
    }, height = 2000)
 
  })
  
  
  ## Submit results ----
  observeEvent(input$submit, {

    # Enter results
    results[results$participantID == currentID,]$option1 <<- input$c1
    results[results$participantID == currentID,]$option2 <<- input$c2
    # Only if more than 2 curves:
    results[results$participantID == currentID,]$option3 <<- 
      ifelse(nrow(curvestoplot)==2, NA, input$c3) 
    results[results$participantID == currentID,]$comment <<- input$comment

    # Keep updating the subset of spiros with NAs
    results_na <- results[results$option1 == 0,]
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
    currentID <<- results_na$participantID[1]
    curvestoplot <<- filter(spirodata, participantID == currentID)

    # Get identifier text to be displayed
    output$currentID <- renderText(as.character(currentID))
    
    # Draw spiros
    output$curves <- renderPlot({
      
      # Plot 1, available for all
      img11 <- image_read(curvestoplot$curve1[1])
      plot11 <- image_ggplot(img11) + theme(plot.margin = margin(0,0,0,0,"cm")) +
        ggtitle(curvestoplot$plabel[1])
      img12 <- image_read(curvestoplot$curve2[1])
      plot12 <- image_ggplot(img12) + theme(plot.margin = margin(0,0,0,0,"cm"))
      
      # Plot 2, available for all
      img21 <- image_read(curvestoplot$curve1[2])
      plot21 <- image_ggplot(img21) + theme(plot.margin = margin(0,0,0,0,"cm")) +
        ggtitle(curvestoplot$plabel[2])
      img22 <- image_read(curvestoplot$curve2[2])
      plot22 <- image_ggplot(img22) + theme(plot.margin = margin(0,0,0,0,"cm")) 
      
      # Null plots by default
      plot31 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot32 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot41 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot42 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot51 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot52 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot61 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot62 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      
      # If needed, substitute
      if(nrow(curvestoplot) > 2){ # 3 or more curves
        img31 <- image_read(curvestoplot$curve1[3])
        plot31 <- image_ggplot(img31) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[3])
        img32 <- image_read(curvestoplot$curve2[3])
        plot32 <- image_ggplot(img32) + theme(plot.margin = margin(0,0,0,0,"cm"))
        
      }
      
      if(nrow(curvestoplot) > 3){ # 4 or more curves
        img41 <- image_read(curvestoplot$curve1[4])
        plot41 <- image_ggplot(img41) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[4])
        img42 <- image_read(curvestoplot$curve2[4])
        plot42 <- image_ggplot(img42) + theme(plot.margin = margin(0,0,0,0,"cm")) 
        
      }
      
      if(nrow(curvestoplot) > 4){ # 5 or more curves
        img51 <- image_read(curvestoplot$curve1[5])
        plot51 <- image_ggplot(img51) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[5])
        img52 <- image_read(curvestoplot$curve2[5])
        plot52 <- image_ggplot(img52) + theme(plot.margin = margin(0,0,0,0,"cm")) 
        
      }
      
      if(nrow(curvestoplot) == 6){ # 6 curves
        img61 <- image_read(curvestoplot$curve1[6])
        plot61 <- image_ggplot(img61) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[6])
        img62 <- image_read(curvestoplot$curve2[6])
        plot62 <- image_ggplot(img62) + theme(plot.margin = margin(0,0,0,0,"cm"))
        
      }
      
      plot_grid(plot11, plot12,
                plot21, plot22,
                plot31, plot32,
                plot41, plot42,
                plot51, plot52,
                plot61, plot62,
                ncol = 2, 
                rel_widths = c(0.5, 0.5))
      
    }, height = 2000)
    
    
    
  })

  ## Load previous results ----
  observeEvent(input$load, {

    results <<- read_csv(input$file$datapath)
    results$comment <<- ifelse(is.na(results$comment), "", results$comment)
    
    # Keep updating the subset of spiros with NAs
    results_na <- results[results$option1 == 0,]
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
    currentID <<- results_na$participantID[1]
    curvestoplot <<- filter(spirodata, participantID == currentID)
    
    # Get identifier text to be displayed
    output$currentID <- renderText(as.character(currentID))
    
    # Draw spiros
    output$curves <- renderPlot({
      
      # Plot 1, available for all
      img11 <- image_read(curvestoplot$curve1[1])
      plot11 <- image_ggplot(img11) + theme(plot.margin = margin(0,0,0,0,"cm")) +
        ggtitle(curvestoplot$plabel[1])
      img12 <- image_read(curvestoplot$curve2[1])
      plot12 <- image_ggplot(img12) + theme(plot.margin = margin(0,0,0,0,"cm"))
      
      # Plot 2, available for all
      img21 <- image_read(curvestoplot$curve1[2])
      plot21 <- image_ggplot(img21) + theme(plot.margin = margin(0,0,0,0,"cm")) +
        ggtitle(curvestoplot$plabel[2])
      img22 <- image_read(curvestoplot$curve2[2])
      plot22 <- image_ggplot(img22) + theme(plot.margin = margin(0,0,0,0,"cm")) 
      
      # Null plots by default
      plot31 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot32 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot41 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot42 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot51 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot52 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot61 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      plot62 <- ggplot() + theme(plot.margin = margin(0,0,0,0,"cm"))
      
      # If needed, substitute
      if(nrow(curvestoplot) > 2){ # 3 or more curves
        img31 <- image_read(curvestoplot$curve1[3])
        plot31 <- image_ggplot(img31) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[3])
        img32 <- image_read(curvestoplot$curve2[3])
        plot32 <- image_ggplot(img32) + theme(plot.margin = margin(0,0,0,0,"cm"))
        
      }
      
      if(nrow(curvestoplot) > 3){ # 4 or more curves
        img41 <- image_read(curvestoplot$curve1[4])
        plot41 <- image_ggplot(img41) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[4])
        img42 <- image_read(curvestoplot$curve2[4])
        plot42 <- image_ggplot(img42) + theme(plot.margin = margin(0,0,0,0,"cm")) 
        
      }
      
      if(nrow(curvestoplot) > 4){ # 5 or more curves
        img51 <- image_read(curvestoplot$curve1[5])
        plot51 <- image_ggplot(img51) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[5])
        img52 <- image_read(curvestoplot$curve2[5])
        plot52 <- image_ggplot(img52) + theme(plot.margin = margin(0,0,0,0,"cm")) 
        
      }
      
      if(nrow(curvestoplot) == 6){ # 6 curves
        img61 <- image_read(curvestoplot$curve1[6])
        plot61 <- image_ggplot(img61) + theme(plot.margin = margin(0,0,0,0,"cm")) +
          ggtitle(curvestoplot$plabel[6])
        img62 <- image_read(curvestoplot$curve2[6])
        plot62 <- image_ggplot(img62) + theme(plot.margin = margin(0,0,0,0,"cm"))
        
      }
      
      plot_grid(plot11, plot12,
                plot21, plot22,
                plot31, plot32,
                plot41, plot42,
                plot51, plot52,
                plot61, plot62,
                ncol = 2, 
                rel_widths = c(0.5, 0.5))
      
    }, height = 2000)

  })

  # Write current results
  observeEvent(input$save, {
    write_csv(results, paste0("output/", gsub(":","-", Sys.time()), ".csv"))
  })

}

# Run app
shinyApp(ui = ui, server = server)