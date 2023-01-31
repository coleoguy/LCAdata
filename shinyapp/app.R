library(shiny)
ref <- read.csv("ref_shiny.version.csv")
# Define UI for application that creates figure
ui <- fluidPage(
    # Application title
    titlePanel("The Role of Epistasis"),
    # Sidebar with various dropdowns to subset data 
    sidebarLayout(
        sidebarPanel(
          h4("Select Data to include:"),
          selectInput("clade", label = h5("Clade"), 
                      choices = list("Plant" = "plant", 
                                     "Animal" = "animal", 
                                     "All" = "all"), 
                      selected = "all"),
          selectInput("diverg", label = h5("Divergence"), 
                      choices = list("Within species" = "within", 
                                     "Between species" = "between", 
                                     "All" = "all"), 
                      selected = "all"),
          selectInput("class", label = h5("Trait class"), 
                      choices = list("Life history" = "LH", 
                                     "Morphology" = "M", 
                                     "All" = "all"), 
                      selected = "all"),
          selectInput("status", label = h5("Organism type"), 
                        choices = list("Domesticated" = "domestic", 
                                       "Lab" = "lab", 
                                       "Wild" = "wild",
                                       "All" = "all"), 
                        selected = "all"),
          selectInput("regress", label = h5("Regression type"), 
                      choices = list("Weighted LSR" = "Y", 
                                     "Unweighted LSR" = "N",
                                     "All" = "all"), 
                      selected = "all"),
          selectInput("method", label = h5("Method"), 
                      choices = list("Standard" = "standard", 
                                     "Cmatrix provided" = "cmat",
                                     "Parental Sex Unknown" = "PSU",
                                     "All" = "all"), 
                      selected = "all"),
          radioButtons("colorby", label = h4("Color by:"),
                       choices = list("Clade" = 7, 
                                      "Divergence" = 12, 
                                      "Trait class" = 6,
                                      "Organism type" = 8,
                                      "Sex chromosome system" = 11,
                                      "Method" = 14,
                                      "Regression type" = 13), 
                       selected = 7),
        ),
        mainPanel(
          
          
          tabsetPanel(
            
            
            tabPanel("Plot",plotOutput("distPlot")),
            tabPanel("Table",
                     checkboxGroupInput(inputId = "table", label = "Choose columns to include",
                                        choices = c("File name",
                                                    "Organism",
                                                    "Phenotype",
                                                    "Sex chromosome System",
                                                    "Trait class",
                                                    "Clade",
                                                    "Organism type",
                                                    "Regression type",
                                                    "Method",
                                                    "Citation"),
                                        selected = c("File name", "Organism", "Phenotype"),
                                        inline = T),
                     downloadButton("downloadData", "Download"), tableOutput("table"))
          )
        )
    )
)


# Define server logic required to create  plot
server <- function(input, output) {
  output$value <- renderPrint({ input$method })


    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- read.csv("complete.results.csv")
        x <- x[complete.cases(x),]
        if(input$clade != "all"){
          x <- x[x$kingdom == input$clade,]
        }
        if(input$diverg != "all"){
          x <- x[x$divergence == input$diverg,]
        }
        if(input$class != "all"){
          x <- x[x$class == input$class,]
        }
        if(input$status != "all"){
          x <- x[x$domestication == input$status,]
        }
        if(input$regress != "all"){
          x <- x[x$weighted == input$regress,]
        }
        if(input$method != "all"){
          x <- x[x$method == input$method,]
        }
        z <- as.numeric(input$colorby)
        states <- unique(x[, z])

        y1 <- sort(x$epi[x[, z] == states[1]])
        y2 <- sort(x$epi[x[, z] == states[2]])
        y3 <- sort(x$epi[x[, z] == states[3]])
        y4 <- sort(x$epi[x[, z] == states[4]])
        x1 <- seq(from=0, to=1, length.out=length(y1))
        x2 <- seq(from=0, to=1, length.out=length(y2))
        x3 <- seq(from=0, to=1, length.out=length(y3))
        x4 <- seq(from=0, to=1, length.out=length(y4))
        plot(y=y1, x=x1, col="red", type="l",
             xlab="proportion of datasets analyzed",
             ylab="proportion of trait divergence that is epistasis",
             ylim=c(0,1))
        lines(y=y2, x=x2, col="blue")
        lines(y=y3, x=x3, col="green")
        lines(y=y4, x=x4, col="black")
        points(y=y1, x=x1, col="red",pch=16, cex=.9)
        points(y=y2, x=x2, col="blue", pch=16, cex=.9)
        points(y=y3, x=x3, col="green", pch=16, cex=.9)
        points(y=y4, x=x4, col="black", pch=16, cex=.9)
        cols <- c("red", "blue", "green", "black")
        yinc <- -.04
        ns <- c(length(x1), length(x2), length(x3), length(x4))[1:length(states)]
        for(i in 1:length(states)){
          points(x=0,y=(.96+yinc*i), pch=15,col=cols[i],cex=1.1)
          text(x=0,y=(.96+yinc*i), pos=4, paste(states[i]," ", "(n = ", ns[i],")", sep=""))
        }
     })
      ref.table <- reactive({
        x <- 0
      if("File name" %in% input$table) x <- c(x, 1)
      if("Organism" %in% input$table) x <- c(x, 2)
      if("Phenotype" %in% input$table) x <- c(x, 3)
      if("Sex chromosome System" %in% input$table) x <- c(x, 4)
      if("Trait class" %in% input$table) x <- c(x, 5)
      if("Clade" %in% input$table) x <- c(x, 6)
      if("Divergence" %in% input$table) x <- c(x, 7)
      if("Organism type" %in% input$table) x <- c(x, 8)
      if("Regression type" %in% input$table) x <- c(x, 9)
      if("Method" %in% input$table) x <- c(x, 10)
      if("Citation" %in% input$table) x <- c(x, 11)
      return(x)
    })
    output$table <- renderTable(ref[,ref.table()],
                                  na = "",
                                  striped = T)
    
      
    output$downloadData <- downloadHandler(
      filename = paste("epistasis", Sys.Date(), ".csv", sep=""),
      content = function(file) {
        write.csv(x[,ref.table()], file, row.names = FALSE)


    })
}



# Run the application 
shinyApp(ui = ui, server = server)
