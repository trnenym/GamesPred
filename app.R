# User interface for predcitions implemented in the Shiny framework (https://shiny.rstudio.com/)

source("./Init.R")
Init()

library(shiny)

if(!file.exists("./DataProcess/Data/cache.RData")) {
  stop("You need to run evaluation first to train a model!")
} else {
  load("./DataProcess/Data/cache.RData")
}

`%then%` <- shiny:::`%OR%`
message.unknown <- paste("<b>Sorry!</b><br/> We cannot find more than one game from the given developer nor publisher."
                         ,"We are unable to reliably predict such games.<br/>"
                         
                         ,"In general, over 91 % of such games have less than 30 players on average in the first 2 months after release,"
                         ,"over 83 % games have less than 10 players,"
                         ,"and over 58 % games have less than 2 players on average.", sep = '<br/>')

#wheather a prediction can be made is determined when developer and publisher are inputted
validate.devpub <- function(input.dev, input.pub){
  is.dev.exp <- input.dev %in% cache$devs.exp
  is.pub.exp <- input.pub %in% cache$pubs.exp
  
  if(!(is.dev.exp || is.pub.exp)) {
    return(message.unknown)
  } else {
    return('Looking good!<br/><br/>Hit "OK" in the bottom when ready. Your prediction will show up here.')
  }
}

# attributes passed to the main prediction function
fields <- c("Name", "Developer", "Publisher", "AgeRequirements", "ReleaseDate", "Price", "ShortDescription", "Description", "Platforms"
            , "Features", "HWCPU", "HWGPU", "HWRAM", "HWHDD", "HWDx", "Languages", "Genres", "Thumbnail", "Screenshots", "Trailers", "UserTags", "DRM")

# Shiny UI
ui <- fluidPage(
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;}"
  ),
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: black;
      }
    ")),
    tags$style(HTML("
      .help-block {
        color: black;
      }
    "))
  ),
  
  tags$div(href="top"),
  headerPanel("GamesPred"),
  
  sidebarPanel(
    helpText(paste("GamesPred allows you to insert info about your upcoming (even unannounced!) game and it will attempt to predict its average number", 
                   "of concurrent players in the first two months following the game's release.")),
    helpText("Note: Not all games can be reliably predicted. You will be immediatelly notified when that is the case"),
    helpText("*fields are required, the rest can be left empty but at the cost of decreased accuracy of predictions"),
    
    textInput("Name", "Name"),
    textInput("Developer", "Developer*"),
    textInput("Publisher", "Publisher*"),
    selectInput("AgeRequirements", "Age Requirements", choices = c("Everyone" = "e", "Early Childhood" = "ec", "Teen" = "t", "Mature" = "m", "Adults Only" = "ao"
                                                                   , "Unknown/Rating Pending" = "rp")),
    dateInput("ReleaseDate", "Release date*", value = as.Date(Sys.time()), min = "2016-01-01", max = "2017-12-31"),
    numericInput("Price", "Price (USD)", 20),
    textAreaInput("ShortDescription", "Short Description"),
    textAreaInput("Description", "Long Description", rows = 10),
    checkboxGroupInput("Platforms", "Platforms", choices = c("Windows", "Mac", "Linux")),
    checkboxGroupInput("Features", "Features", choices = c("Single Player" = "sp", "Multi Player" = "mp", "Coop" = "co", "Local Coop" = "lco"
                                                           , "Steam Achievements" = "ach", "Steam Trading Cards" = "cards", "Steam Workshop" = "wshop"
                                                           , "VR Support" = "vr", "Controller Support" = "con")),
    numericInput("HWCPU", "CPU (MHz)", 1024),
    textInput("HWGPU", "GPU (e.g. GeForce GTX 1070)", value = "Intel HD 4600"),
    numericInput("HWRAM", "RAM (MB)", 1024),
    numericInput("HWHDD", "Disk space (MB)", 200),
    numericInput("HWDx", "DirectX", 9.0),
    
    textInput("Languages", "Languages (separated by commas)", value = "English"),
    
    checkboxGroupInput("Genres", "Genres", choices = c("RPG", "Strategy", "Adventure", "Action", "Simulation", "Racing", "Casual", "Sports"
                                                       , "Massively Multiplayer" = "MassivelyMultiplayer", "Education", "Indie")),
    
    fileInput("Thumbnail", "Select a thumbnail (.jpg)", accept = c(".jpg")),
    
    numericInput("Screenshots", "Number of screenshots", 0),
    numericInput("Trailers", "Number of trailers", 0),
    
    textInput("UserTags", "Other genre or theme specification (separated by commas - e.g. FPS, Horror)"),
    
    checkboxGroupInput("DRM", "DRM", choices
                       = c("Third-party DRM (in general)" = "DRMNotice", "Third-party Eula" = "DRMEula", "Third-party account" = "DRMAccount")),
    
    actionButton("OKButton", "OK"),
    
    tags$p(HTML("")),
    
    tags$p(HTML("<a href='#top'>Back to top</a>"))
  ),
  
  mainPanel(
    tags$head(tags$style(type="text/css", "
             #loadmessage {
                         position: fixed;
                         top: 0px;
                         left: 0px;
                         width: 100%;
                         padding: 5px 0px 5px 0px;
                         text-align: center;
                         font-weight: bold;
                         font-size: 100%;
                         color: #000000;
                         background-color: #CCFF66;
                         z-index: 105;
                         }
                         ")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Working...",id="loadmessage")),
    htmlOutput("Status"),
    htmlOutput("Message"),
    dataTableOutput("Similar")
  )
)

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  validation.message <- reactive({
    validate(
      need(input$Developer != "", label = "Developer") %then%
        need(input$Publisher != "", label = "Publisher")
    )
    message <- validate.devpub(input$Developer, input$Publisher)
  })
  
  output$Status <- renderUI(HTML(validation.message()))
  
  observeEvent(input$OKButton, {
    game.data <- sapply(fields, function(x) input[[x]])
    game.data$Thumbnail <- input$Thumbnail$datapath
    source("./Prediction.Main.R")
    prediction <- Predict.game(game.data)
    if(prediction$message == "unknown") {
      prediction$message <- ""
    }
    output$Message <- renderUI(HTML(paste0("<br/><br/>", prediction$message)))
    output$Similar <- renderDataTable(prediction$similar, escape = FALSE, options = list(searching = FALSE, paging = FALSE))
  })
}

shinyApp(ui = ui, server = server)