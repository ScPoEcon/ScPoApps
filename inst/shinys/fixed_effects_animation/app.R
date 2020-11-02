library(shiny)
library(tidyverse)
library(gganimate)
library(ggthemes)
library(gifski)

ui <- fluidPage(

    # Application title
    titlePanel("Fixed effects animation"),
    
    # Math Model
    h5("The model generating our data:"),
    withMathJax("$$X=0.5+0.5(Person-2.5)+\\varepsilon$$"),
    withMathJax("$$Y=\\beta X + (Person-2.5)+\\varepsilon$$"),
    withMathJax("$$\\varepsilon \\sim N(0,1) , \\quad Person \\in \\{1,2,3,4\\}$$"),
    
    sidebarLayout(
        sidebarPanel(
            withMathJax(),
            sliderInput("cor",
                        label = "the value of \\( \\beta \\) :",
                        min = -1,
                        max = 1,
                        value = -0.5,step = 0.1)
        ),

        mainPanel(
            helpText("Please wait about 30 seconds every time you select a new value..."),
            imageOutput("scatterPlot")
        )
    )
)


server <- function(input, output) {

    output$scatterPlot <- renderImage({
        
        df <- data.frame(Person = rep(1:4,50)) %>%
            mutate(X = .5+.5*(Person-2.5) + rnorm(200)) %>%
            mutate(Y = (input$cor)*X + (Person-2.5) + 1 + rnorm(200),time="1") %>%
            group_by(Person) %>%
            mutate(mean_X=mean(X),mean_Y=mean(Y)) %>%
            ungroup()
        
        #Calculate correlations
        before_cor <- paste("1. Start with raw data. Correlation between X and Y: ",round(cor(df$X,df$Y),3),sep='')
        after_cor <- paste("6. Analyze what's left! Within-Individual Correlation Between X and Y: ",round(cor(df$X-df$mean_X,df$Y-df$mean_Y),3),sep='')
        
        
        dffull <- rbind(
            #Step 1: Raw data only
            df %>% mutate(mean_X=NA,mean_Y=NA,time=before_cor),
            #Step 2: Add x-lines
            df %>% mutate(mean_Y=NA,time='2. Figure out any between-Individual differences in X'),
            #Step 3: X de-meaned 
            df %>% mutate(X = X - mean_X,mean_X=0,mean_Y=NA,time="3. Remove all between-Individual differences in X"),
            #Step 4: Remove X lines, add Y
            df %>% mutate(X = X - mean_X,mean_X=NA,time="4. Figure out any between-Individual differences in Y"),
            #Step 5: Y de-meaned
            df %>% mutate(X = X - mean_X,Y = Y - mean_Y,mean_X=NA,mean_Y=0,time="5. Remove all between-Individual differences in Y"),
            #Step 6: Raw demeaned data only
            df %>% mutate(X = X - mean_X,Y = Y - mean_Y,mean_X=NA,mean_Y=NA,time=after_cor))
        
        p <- ggplot(dffull,aes(y=Y,x=X,color=as.factor(Person)))+geom_point()+
                geom_vline(aes(xintercept=mean_X,color=as.factor(Person)))+
                geom_hline(aes(yintercept=mean_Y,color=as.factor(Person)))+
                guides(color=guide_legend(title="Individual"))+
                scale_color_colorblind()+
                labs(title = 'The Relationship between Y and X, with Individual Fixed Effects \n{next_state}')+
                transition_states(time,transition_length=c(12,32,12,32,12,12),state_length=c(160,100,75,100,75,160),wrap=FALSE)+
                ease_aes('sine-in-out')+
                exit_fade()+enter_fade()
            
        
        anim_save("outfile.gif", animate(p,duration = 5, fps = 20, width = 500, height = 500, nframes=200, renderer = gifski_renderer())) 

        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, deleteFile = TRUE)


}

# Run the application 
shinyApp(ui = ui, server = server)
