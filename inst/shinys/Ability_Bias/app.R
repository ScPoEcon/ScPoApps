library(shiny)
library(MASS)
library(tidyverse)
library(estimatr)
library(gridExtra)


# n<-1000
# A<-rnorm(1.3*n,mean = 0.5, sd = 0.25)
# A<-A[A>=0 & A<=1]
# A<-A[1:n]
# z<-rnorm(1.3*n,mean = 0, sd = 1)
# z<-z[z>=-2 & z<=2]
# z<-z[1:n]
# x<-rbinom(n,size = 1,prob = 0.4) 
# u<-rnorm(n,mean = 0, sd = 1)
# e<-rnorm(n,mean = 0, sd = 1)
# 
# # define our model's coefficients
# a0<-1
# b0<-1
# b1<-1
# b3<-5
# a1<-1 # constraint
# b2<-1
# a2<-0.2
# 
# s<-a0+a1*A+a2*z+e
# 
# y<-b0+b1*x+b2*s+b3*A+u
# data<-tibble(A,z,x,s,y,u,e)
# biased <- lm(y ~ x + s)
# iv <- estimatr::iv_robust(y ~ x + s | x + z, data)
# estimate<-biased$coefficients["s"]
# estimate_iv<-iv$coefficients["s"]
# data.frame(coef<-"b2",true<-b2,estimate,estimate_iv)
# 



ui <- fluidPage(

    # Application title
    titlePanel("Ability Bias"),
    withMathJax(),
    textOutput("text"),
    helpText("model one (interaction): $$y = b_0 + b_1x + (b_2 + b_3A)s + u$$"),
    helpText('model two (linear combination): $$y = b_0 + b_1x + b_2s + b_3A + u$$'),
    helpText('Relation with intrumental variable Z: $$s = a_0 + a_1A +a_2Z + e$$'),
    helpText('With: $$a_0 = 1$$ $$ b_0 = 1$$ $$ a_1 = 1$$'),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            radioButtons("model","choose a model:",choices = list("linear combination","interaction")),

            
            sliderInput("a2",
                        "Value of a2 (Strength of instrument):",
                        min = 0,
                        max = 5,
                        value = 1,step = 0.1),

            sliderInput("a1",
                        "Value of a1:",
                        min = 0,
                        max = 5,
                        value = 4,step = 0.1),


            sliderInput("b2",
                        "Value of b2:",
                        min = 0,
                        max = 5,
                        value = 0.5,step = 0.1),
            
            sliderInput("b3",
                        "Value of b3:",
                        min = 0,
                        max = 5,
                        value = 4,step = 0.1),
            
            sliderInput("n",
                        "Value of n (Sample Size):",
                        min = 0,
                        max = 5000,
                        value = 2500,step = 1),
            
            tableOutput('table')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")

        )
    )
    
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # coef<-reactive({
    #     unlist(list("a1"<-input$a1,"a2"<-input$a2,"b2"<-input$b1,"b3"<-input$b3,"n"<-input$n))
    # })
    
    mydata<-reactive({
        n<-input$n
        A<-rnorm(1.3*n,mean = 0.5, sd = 0.25)
        A<-A[A>=0 & A<=1]
        A<-A[1:n]
        z<-rnorm(1.3*n,mean = 0, sd = 1)
        z<-z[z>=-2 & z<=2]
        z<-z[1:n]
        x<-rbinom(n,size = 1,prob = 0.4) 
        u<-rnorm(n,mean = 0, sd = 1)
        e<-rnorm(n,mean = 0, sd = 1)
        
        # define our model's coefficients
        a0<-1
        b0<-1
        b1<-1
        b3<-input$b3
        a1<-input$a1 # constraint
        b2<-input$b2
        a2<-input$a2
        
        s<-a0+a1*A+a2*z+e
        
        if(input$model=="linear combination"){
            y<-b0+b1*x+b2*s+b3*A+u
        }else{
            y<-b0+b1*x+(b2+b3*A)*s+u
        }
        
        
        data.frame(A,z,x,s,y,u,e)
        
        })

    output$distPlot <- renderPlot({
        data<-mydata()
        biased <- lm(y ~ x + s,data = data)
        estimate<-biased$coefficients["s"]

        p1 <- ggplot(data, aes(x = z, y = s, color = A)) + geom_point() + stat_smooth(method = "lm",formula = y ~ x)
        p2 <- ggplot(data, aes(x = s, y = y, color = A)) + geom_point() + stat_smooth(method = "lm",formula = y ~ x)
        #+ geom_segment(x = mean(data$s)-2, xend = mean(data$s) + 2, y = mean(data$y)-2*estimate, yend = mean(data$y)+2*estimate, size = 2) 
        grid.arrange(p1,p2,ncol = 2)

    })
    
    
    output$table <- renderTable({
        
        data<-mydata()
        biased <- lm(y ~ x + s,data = data)
        iv <- estimatr::iv_robust(y ~ x + s | x + z, data)
        estimate<-biased$coefficients["s"]
        estimate_iv<-iv$coefficients["s"]
        coef<-"b2"
        true<-input$b2
        data.frame(coef,true,estimate,estimate_iv)
        
        
        }
        )
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
