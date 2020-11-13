#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(gghighlight)
library(cowplot)
library(tidyr)

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

data_alldf <- function(fun = function(x) {x*sin(x-2) + 0.2*x},n=90,ub = 5,nnew = 200,maxdf=60){
    set.seed(1234)
    eps = 1 # hard code variance
    
    r = data.frame(x = seq(0,ub,length.out = n))
    r$truth = fun(r$x) 
    r$epsi = rnorm(n,mean = 0, sd = eps)
    r$y = r$truth + r$epsi
    
    mods = getmodels(r$x,r$y,seq(0,ub, length.out = nnew),dfs = seq(2,maxdf,by = 2))
    # add test data to predictions
    mods$pred$truth = fun(mods$pred$x)
    mods$pred$testdata = mods$pred$truth + rnorm(nnew,mean = 0, sd = eps)
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
    mses$plotdata$flexibility = seq(2,maxdf,by = 2)
    mses$plotdata = mses$plotdata %>% tidyr::pivot_longer(-flexibility, names_to = "variable")
    
    
    list(models = mods,mses = mses)
    
}



plot_singledf <- function(x,df){
    d = x$models$plotdata
    m = x$mses$plotdata %>% filter(variable %in% c("test","train"))
    
    p = ggplot(d %>% filter(flexibility != "truth"))
    p = p + geom_line(aes(x = x, y = value, color = flexibility), size = 1) + gghighlight(flexibility == paste0("df",df))
    p = p + geom_line(data = d %>% filter(flexibility == "truth"), aes(x=x,y = value), color = "black", size = 1)
    p = p + geom_point(data = x$models$original, aes(x,y), shape = 1,size = 2) + theme_bw()
    
    b = ggplot(m, aes(x=flexibility,y = value, color = variable)) + 
        geom_point() + gghighlight(flexibility == df)
    b = b + geom_hline(yintercept = 1, linetype = "dashed", color = "grey")+ theme_bw()
    cowplot::plot_grid(p,b)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bias Variance Tradeoff"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("df",
                        "Degrees of Freedom:",
                        min = 2,
                        max = 60,
                        step = 2,
                        value = 2)
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