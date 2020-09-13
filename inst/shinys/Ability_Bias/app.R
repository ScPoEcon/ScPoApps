library(shiny)
library(MASS)
??mvnorm

n<-1000
A<-rnorm(1.3*n,mean = 0.5, sd = 0.25)
A<-A[A>=0 & A<=1]
A<-A[1:n]
Z<-rnorm(1.3*n,mean = 0, sd = 1)
Z<-Z[Z>=-2 & Z<=2]
Z<-Z[1:n]
x<-rbinom(n,size = 1,prob = 0.4) 
u<-rnorm(n,mean = 0, sd = 1)
e<-rnorm(n,mean = 0, sd = 1)

# define our model's coefficients
a0<-1
b0<-1
b1<-1
a1<-1
b2<-1
b3<-1
rho<-0

ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    withMathJax(),
    helpText('The real model: $$y = b_0 + b_1x + (b_2 + b_3A)s + u$$'),
    helpText('The model with intrumental variable Z: $$s = a_0 + a_1A +a_2Z + e$$'),
    helpText('With: $$b_3=a_1$$'),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("a2",
                        "Value of a2 (Strength of instrument):",
                        min = 0,
                        max = 5,
                        value = 0.5,step = 0.1),
            dataTableOutput('table')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
           
        )
    ),
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        s<-a0+a1*A+input$a2*Z+e
        y<-b0+b1*x+(b2+a1*A)*s+u
        
        par(mfrow=c(1,2))
        plot(s,y)
        fit1<-lm(y ~ s)
        abline(fit1)
        plot(Z,s)
        fit2<-lm(s ~ Z)
        abline(fit2)
        
    })
    
    output$table <- renderDataTable({
        s<-a0+a1*A+input$a2*Z+e
        y<-b0+b1*x+(b2+a1*A)*s+u
        rho<-cor(y,s)
        data.frame(b1,a1,rho)
        }
        )
}

# Run the application 
shinyApp(ui = ui, server = server)
