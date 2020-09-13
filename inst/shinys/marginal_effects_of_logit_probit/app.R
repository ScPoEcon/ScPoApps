library(shiny)
library(ggplot2)
#Using Taylor formula, so there is a control of precision.
delta_x <- 0.0001 
p <- ggplot(data.frame(x = c(-5,5)), aes(x=x)) + 
    stat_function(fun = pnorm, aes(colour = "Probit")) + 
    stat_function(fun = plogis, aes(colour = "Logit")) + 
    theme_bw() + 
    scale_colour_manual(name = "Function G",values = c("red", "blue")) +
    scale_y_continuous(name = "Pr(y = 1 | x)")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Marginal effect of Probit and Logit"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("x",
                        "Value of x:",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = 0.1),
            selectInput("model",
                        "Choose a nonlinear model:",
                        choices = c("logit","probit")
                        )
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
        
    if (input$model == "probit"){
        tangent<-(pnorm(input$x+delta_x)-pnorm(input$x))/delta_x
        p + geom_segment(
            x = input$x-0.5, xend = input$x + 0.5,
            y = pnorm(input$x)-0.5*tangent, yend = pnorm(input$x)+0.5*tangent, size = 0.7
        )
    }else{
        tangent<-(plogis(input$x+delta_x)-plogis(input$x))/delta_x 
        p + geom_segment(
            x = input$x-0.5, xend = input$x + 0.5,
            y = plogis(input$x)-0.5*tangent, yend = plogis(input$x)+0.5*tangent,
        size = 0.7)
    }

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
