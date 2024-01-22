#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(gghighlight)
library(cowplot)
library(tidyr)

# globals
STEP = 10
MAXDF = 60
EPS = 1
DFS = c(2:8,seq(10,MAXDF,by = STEP))

getmodels <- function(x,y,newx,dfs = 2:20){
    r = data.frame(x=x,y=y)
    o = data.frame(x=newx)
    s = list()
    # browser()
    for (i in 1:length(dfs)){
        if (dfs[i] == 2){
            s[[i]] <- lm(y~x,r)
            o = cbind(o, predict(s[[i]], newdata = o))
        } else {
            s[[i]] <- smooth.spline(x,y,df = dfs[i])
            o = cbind(o, predict(s[[i]], o$x)$y)
        }
    }
    names(o)[-c(1)] <- paste0("df",dfs)
    names(s) <- paste0("df",dfs)
    list(models = s, pred = o, original = r)
}

data_alldf <- function(fun = function(x) {x*sin(x-2) + 0.2*x},n=90,ub = 5,nnew = 200){
    set.seed(1234)
    # eps = 1 # hard code variance
    
    r = data.frame(x = seq(0,ub,length.out = n))
    r$truth = fun(r$x) 
    r$epsi = rnorm(n,mean = 0, sd = EPS)
    r$y = r$truth + r$epsi
    
    mods = getmodels(r$x,r$y,seq(0,ub, length.out = nnew),dfs = DFS)
    # add test data to predictions
    mods$pred$truth = fun(mods$pred$x)
    mods$pred$testdata = mods$pred$truth + rnorm(nnew,mean = 0, sd = EPS)
    mods$plotdata = mods$pred %>% 
        select(-testdata) %>%
        tidyr::pivot_longer(-x,names_to = "flexibility")
    
    # mses and bias
    mses = list(
        train = colMeans(sapply(mods$models,residuals)^2)
    ) # test mses
    mses$test <- colMeans((mods$pred[,names(mods$models)] - mods$pred[,"testdata"])^2)
    
    # bias
    mses$bias <- colMeans((mods$pred[,names(mods$models)] - mods$pred[,"truth"])^2)
    mses$var <- diag(var(mods$pred[,names(mods$models)]))
    
    mses$plotdata <- data.frame(mses)
    mses$plotdata$flexibility = DFS
    mses$plotdata = mses$plotdata %>% tidyr::pivot_longer(-flexibility, names_to = "variable")
    
    
    list(models = mods,mses = mses)
    
}



plot_singledf <- function(x,df){
    d = x$models$plotdata
    m = x$mses$plotdata %>% filter(variable %in% c("test","train"))
    
    p = ggplot(d %>% filter(flexibility != "truth"))
    p = p + geom_line(aes(x = x, y = value, color = flexibility), size = 1) + gghighlight(flexibility == paste0("df",df))
    p = p + geom_line(data = d %>% filter(flexibility == "truth"), aes(x=x,y = value), color = "black", size = 1)
    p = p + geom_point(data = x$models$original, aes(x,y), shape = 1,size = 2) + theme_bw() + labs(title = "Fitting Data on f: f(x) + e", caption = "Solid black line is true f. Circles are realizations of f(X) + e: the data.") + ylab("Estimate of f")
    
    b = ggplot(m, aes(x=flexibility,y = value, color = variable)) + 
        geom_point(size = 3, shape = 15) + gghighlight(flexibility == df)
    b = b + geom_line(data = m, aes(x=flexibility, y = value, group = variable), color = "grey")  + ylab("MSE") + xlab("degree of flexibility (degrees of freedom)")
    b = b + geom_hline(yintercept = 1, linetype = "dashed", color = "grey")+ theme_bw() + labs(title = "Mean Squared Errors")
    cowplot::plot_grid(p,b)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bias Variance Tradeoff"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            shinyWidgets::sliderTextInput("df",
                        "Degrees of Freedom:",
                        choices = DFS)
                        # min = 2,
                        # max = MAXDF,
                        # step = 1,
                        # value = 2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("dfPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    d = data_alldf()

    output$dfPlot <- renderPlot({
        plot_singledf(d,input$df)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)