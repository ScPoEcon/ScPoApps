library(shiny)
library(MASS)
library(tidyverse)
library(estimatr)
library(gridExtra)
library(data.table)


run <- function(alpha){
    d = makedata(alpha1 = alpha)
    ols <- coef(lm(y ~ X + s, d))
    iv  <- coef(estimatr::iv_robust(y ~ X + s | X + Z,d))
    return(list(ols_s = ols["s"],
                iv_s  = iv["s"]))
}

makedata <- function(alpha1,n = 1000){
    # y = b0 + b1 x + g1 A + b2 s + u
    # s = a0 + g1 A + a1 Z + e
    
    # true values
    # y equation
    b0 = 0.7
    b1 = 0.3
    b2 = 1.0
    
    # s equation
    a0 = 5.0

    g1 = 10
    
    # set.seed(1111)
    
    d = data.table(u = rnorm(n),
                   e = rnorm(n),
                   A = rnorm(n,mean = 0.5, sd = 0.25),
                   Z = rnorm(n),
                   # D = rnorm(n),
                   X = rbinom(n,size = 1,prob = 0.4))
    
    # impose truncations
    d[A > 1 , A := 1]
    d[A < 0 , A := 0]
    d[Z > 2 , Z := 2]
    d[Z < -2 , Z := -2]
    # d[D > 2 , D := 2]
    # d[D < -2 , D := -2]
    
    # put together data   
    # d[, s := a0 +  g1 * A + alpha1 * Z + (5-alpha1) * D + e]
    d[, s := a0 +  g1 * A + alpha1 * Z + e]
    d[, y := b0 + b1 * X +  (g1 * A) + b2 * s + u]
    d
}


ui <- fluidPage(

    # Application title
    titlePanel("Ability Bias"),
    withMathJax(),
    textOutput("text"),
    helpText("Real Model : $$y = b_0 + b_1x + g_1A + b_2s + u$$"),
    helpText('With: $$a_0 = 5, g_1 = 10 $$'),
    helpText('Determination of s: $$s = a_0 + g_1A + a_1Z + e$$'),
    helpText('With: $$ b_0 = 0.7 , b_1 = 0.3 , b_2 = 1$$'),
    helpText('$$ A \\sim N(0.5,0.25); \\quad Z,x,u,e \\sim N(0,1)$$ '),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("a1",
                        "Value of a1 (Strength of instrument):",
                        min = 0,
                        max = 2,
                        value = 0.1,step = 0.001),

            tableOutput('table')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")

        )
    )
    
)



server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        data<-makedata(input$a1)

        p1 <- ggplot(data, aes(x = Z, y = s, color = A)) + geom_point() + stat_smooth(method = "lm",formula = y ~ x)
        p2 <- ggplot(data, aes(x = s, y = y, color = A)) + geom_point() + stat_smooth(method = "lm",formula = y ~ x)
        grid.arrange(p1,p2,ncol = 2)

    })
    
    
    output$table <- renderTable({
        
        coef<-run(input$a1)

        ols_est<-coef["ols_s"]
        iv_est<-coef["iv_s"]
        coef<-"b2"
        true<-1
        data.frame(coef,true,ols_est,iv_est)

        }
        )
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
