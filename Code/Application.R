library(shiny)
library(FSA)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(12,
           h1("Condition Calculator", align = "center", style = "font-family: 'times'; font-si16pt"),
           fluidRow(
             column(4,
                    h4("Input parameters", align = "center", style = "font-family: 'times'; font-si16pt"),
                    
                    fluidRow(style = "border: 2px solid blue;",
                             column(4, 
                                    h4("Species", align = "center", style = "font-family: 'times'; font-si16pt")),
                             tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 32px;} .selectize-dropdown { font-size: 12px; line-height: 28px; }"),
                             column(8, selectInput("species", " ",
                                                   c("Bluegill" = "Bluegill",
                                                     "Largemouth Bass" = "Largemouth Bass",
                                                     "Striped Bass" = "Striped Bass")))),
                    
                    br(),
                    
                    fluidRow(style = "border: 2px solid blue;",
                             column(4, 
                                    h4("Length", align = "center", style = "font-family: 'times'; font-si16pt")),
                             column(4, numericInput("Length", 
                                                    h6("Inches"), 
                                                    value = 10))),
                    
                    br(),
                    
                    fluidRow(style = "border: 2px solid blue;",
                             column(4, 
                                    h4("Weight", align = "center", style = "font-family: 'times'; font-si16pt")),
                             column(4, numericInput("lbs", 
                                                    h6("Pounds"),
                                                    min = 0,
                                                    max = 100,
                                                    value = 1)),
                             column(4, 
                                    ""),
                             column(4, numericInput("oz", 
                                                    h6("Ounces"), 
                                                    min = 0,
                                                    max = 15,
                                                    value = 1)))
             ),
             column(width = 8,
                    br(),
                    actionButton("calc", "Calculate Relative Weight",  style="font-family: 'times'; color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    verbatimTextOutput("nText"),
                    tags$head(tags$style("#nText{white-space: normal; color:black; max-width: 100; word-break: keep-all; font-size: 20px; font-family: 'times'; background: white;}")),
                    plotOutput("plot"))
           )
    )
  )
)

server = function(input, output) {
  
  ntext = eventReactive(input$calc, {
    (((input$lbs + (input$oz/15)) / 0.00220462) / (10^(wsVal(input$species)$int + wsVal(input$species)$slope * log10(input$Length * 25.4)))) * 100
  })
  
  output$nText = renderText({
    paste("The relative weight for this fish is", trunc(ntext()), "which is considered", 
          ifelse(ntext() > 95, "excellent condition. Consider releasing this fish.", 
                 ifelse(ntext() <95 & ntext() > 85, "average condition. Consider releasing this fish.", 
                        "poor condition. Consider harvesting this fish")))
  })
  
  b = eventReactive(input$calc, { 
    
    length = seq(wsVal(input$species)$min.TL, wsVal(input$species)$min.TL * 5, 1)
    weight = 10^(wsVal(input$species)$int + wsVal(input$species)$slope * log10(length))
    
    length = length / 25.4
    weight = weight * 0.00220462
    
    as.data.frame(cbind(length, weight))
    
  })
  
  output$plot = renderPlot({
    
    b = b()
    
    fish = data.frame(length = input$Length, weight = (input$lbs + input$oz/15))
    
    ggplot(b, aes(x = length, y= weight)) +
      geom_line(size  = 1.5, colour = "blue") +
      geom_point(data = fish, col = 'red', size = 3) +
      
      theme_bw() + #Adds black border around figure
      theme(axis.line = element_line(colour = "black"), #Removes grid
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      
      theme(axis.title.y = element_text(face = "bold", color = "black", size = 14),
            axis.text.y  = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.text.x  = element_text(face = "bold", color = "black", size = 12)) +
      
      xlab("Length (Inches)") + 
      
      ylab("Weight (Pounds)") +
      
      xlim(0, max(b$length)) +
      
      ylim(0, max(b$weight)) 
    
  })
  
}

shinyApp(ui, server)