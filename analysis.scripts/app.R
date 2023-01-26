#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The Role of Epistasis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("clade", label = h3("Clade Selection"), 
                        choices = list("Plant" = "plant", 
                                       "Animal" = "animal", 
                                       "All" = "all"), 
                        selected = "all"),
            selectInput("diverg", label = h3("Divergence Selection"), 
                        choices = list("Within Species" = "within", 
                                       "Between Species" = "between", 
                                       "All" = "all"), 
                        selected = "all")
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
        lh <- sort(x$epi[x$class == "LH"])
        m <- sort(x$epi[x$class == "M"])
        lhx <- seq(from=0, to=1, length.out=length(lh))
        mx <- seq(from=0, to=1, length.out=length(m))
        plot(y=lh, x=lhx, col="red", type="l", 
             xlab="proportion of datasets analyzed", 
             ylab="proportion of trait divergence that is epistasis")
        lines(y=m, x=mx, col="blue")
        points(y=lh, x=lhx, col="red",pch=16, cex=.9)
        points(y=m, x=mx, col="blue", pch=16, cex=.9)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
