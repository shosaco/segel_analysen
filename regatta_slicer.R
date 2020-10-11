# Shiny Gadget to identify the rounds of a regatta and export as tidy GPX track

library(shiny)
library(miniUI)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(lubridate)
library(ggmap)
library(purrr)
library(shinyjs)

create_gpx <- function(session_df, file_name){
  cat(c('<?xml version="1.0" encoding="UTF-8" ?><gpx version="1.1" creator="R">',
        split(session_df, session_df$Runde) %>% map(df2trk) %>% unlist,
        '</gpx>'),
      file=file_name, 
      sep="\n")
}

df2trk <- function(df) {
  o <- c('<trk>','<trkseg>')
  o <- c(o, paste('<trkpt lat="',df$Lat,'" lon="',df$Long,'"><time>',paste(gsub(' ','T', as.character(df$Date)), 'Z', sep=''),
                  '</time></trkpt>', sep=''))
  o <- c(o, '</trkseg>', '</trk>')
  return(o)
}

regatta_tool <- function() {
  
  
  ui <- miniPage(
    gadgetTitleBar("Regatta Slicer"),
    
    miniContentPanel(useShinyjs(),
                     div(id = "fileUpload", fileInput("upload", "CSV hochladen", accept = ".csv")),
                     radioButtons("round", "Slider für Runde", choices = 1:4, inline=TRUE),
                     sliderInput("slice", "Range", min=1, max = 1000, value=c(1,100), timeFormat = "%T", timezone = "+0200")),
    plotOutput("plot")
  )

server <- function(input, output, session) {
  
  rounds <- reactiveVal()
  
  current_dat <- reactiveVal()
  dat_input <- reactiveVal()
  observeEvent(input$upload, {
    print(input$upload)
    out <- read_csv(input$upload$datapath) %>% select(Lat, Long, Date, contains("Speed")) %>% set_names(make.names)
    updateSliderInput(session, "slice", value = range(out$Date), min = min(out$Date), max = max(out$Date))
    dat_input(out)
    hide("fileUpload", anim=TRUE)
  })
  
  # upon change of slice, save current slicer value for current round
  observeEvent(input$slice, {
    rounds_val <- rounds()
    rounds_val[[input$round]] <- input$slice
    rounds(rounds_val)
    print(rounds())
  })
  
  # upon new round selection, update slicer to saved value for new round
  observeEvent(input$round, {
    if(length(rounds()[[input$round]]) > 0) updateSliderInput(session, "slice", value = rounds()[[input$round]])
  })
  
  # apply rounds boundaries to uploaded dataframe
  current_dat <- reactive({
    req(!is.null(dat_input()))
    input$slice
    out <- dat_input()
    out$Runde <- NA_character_
    for(nr in names(rounds())){
      out <- out %>% mutate(Runde = ifelse(between(Date, rounds()[[nr]][1], rounds()[[nr]][2]), nr, Runde))
    }
    
    out <- out %>% filter(!is.na(Runde))
    return(out)
  })
  
  this_map <- reactive({
    validate(need(nrow(current_dat()) > 0, "Keine Daten übrig."))
    get_stamenmap(c(left=min(current_dat()$Long), 
                    right=max(current_dat()$Long), 
                    bottom=min(current_dat()$Lat), 
                    top=max(current_dat()$Lat)), zoom=15)
  })
  
  output$plot <- renderPlot({
    validate(need(nrow(current_dat()) > 0, "Keine Daten übrig."))
    dat_sliced <- current_dat() %>% 
      mutate(across(Runde, ~paste("Runde", .x))) %>%
      mutate(across(Runde, factor)) %>%
      group_by(Runde) %>% mutate(alpha = row_number()/n())
    
    speed_avg <- dat_sliced %>% summarize("speed_avg" = round(mean(Speed..m.s. * 1.94384), 1), .groups = "drop")
    dat_sliced <- dat_sliced %>% left_join(speed_avg, "Runde") %>% mutate(title = paste(Runde, "(Ø", speed_avg, "kn)"))
    
    ggmap(this_map()) + 
      geom_point(data = dat_sliced, aes(x=Long, y = Lat, color = Runde, alpha = 1-alpha)) +
      theme(legend.position = "none", axis.text = element_blank(), axis.title = element_blank()) +
      facet_grid(.~title) +
      scale_color_manual(values = rainbow(n_distinct(dat_sliced$Runde)))
    
  })
  
  # When the Done button is clicked, write found rounds and clean GPX track to disk
  observeEvent(input$done, {
    write_csv(as_tibble(rounds(), paste0("rounds_", input$upload$name)))
    
    current_dat() %>% 
      create_gpx(file_name = paste0("clean_", str_replace(input$upload$name, ".csv", ".gpx")))
    stopApp("Success")
  })
}

runGadget(ui, server)
}

regatta_tool()

