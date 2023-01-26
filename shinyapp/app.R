library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("The Role of Epistasis"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          h4("Select Data to include:"),
          selectInput("clade", label = h5("Clade"), 
                      choices = list("Plant" = "plant", 
                                     "Animal" = "animal", 
                                     "All" = "all"), 
                      selected = "all"),
          selectInput("diverg", label = h5("Divergence"), 
                      choices = list("Within Species" = "within", 
                                     "Between Species" = "between", 
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
          h4("Design Plot"),
          radioButtons("colorby", label = h3("Color by:"),
                       choices = list("Clade" = 7, 
                                      "Divergence" = 12, 
                                      "Trait class" = 6,
                                      "Organism type" = 8,
                                      "Sex chromosome system" = 11,
                                      "Method" = 14,
                                      "Regression type" = 13), 
                       selected = 7)
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

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
        z <- as.numeric(input$colorby)
        states <- unique(x[, z])

        y1 <- sort(x$epi[x[, z] == states[1]])
        y2 <- sort(x$epi[x[, z] == states[2]])
        x1 <- seq(from=0, to=1, length.out=length(y1))
        x2 <- seq(from=0, to=1, length.out=length(y2))
        plot(y=y1, x=x1, col="red", type="l",
             xlab="proportion of datasets analyzed",
             ylab="proportion of trait divergence that is epistasis",
             ylim=c(0,1))
        lines(y=y2, x=x2, col="blue")
        points(y=y1, x=x1, col="red",pch=16, cex=.9)
        points(y=y2, x=x2, col="blue", pch=16, cex=.9)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
