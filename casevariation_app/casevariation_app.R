library(shiny)
library(ggplot2)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
  titlePanel("How the cases you choose affect the answers you get", windowTitle = "Simulate research designs"),
   
  sidebarLayout(
    sidebarPanel(
          sliderInput(inputId = "obs",
                     label = "Restrict share of observations",
                     min = .05,
                     max = 1,
                     step=0.05,
                     value = 1),
        sliderInput(inputId = "xrange",
                     label = "Restrict range of independent variable (X)",
                     min = -125,
                     max = 125,
                     value = c(-125,125),
                     step = 10),
         sliderInput(inputId = "yrange",
                     label = "Restrict range of dependent variable (Y)",
                     min = 500,
                     max = 13000,
                     value = c(500,13000),
                     step = 10)
         ),
        
    # Show a plot of the generated distribution
      mainPanel(
        plotOutput("multplot"))
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$multplot <-renderPlot({
    # Simulate data
    b <- 50 # coefficient
    a <- 5000 # intercept
    n <- 200 # observations
    k <- rbinom(200,1,input$obs)
    
    
    X <- rnorm(n, 10, 50) # Create a sample of (fixed!) observations on variable X.
    
    Y <- a + b*X + rnorm(n,0,1000) # true DGP

    data <- data.frame(X,Y,k)
    data <- data[data$X>-125 & data$X<125 & data$Y>510 & data$Y<13200,]
    print(range(data$X))
    print(range(data$Y))
    
    scatter <- ggplot(data[data$Y>=min(input$yrange) & data$Y<=max(input$yrange)
                           & data$X>=min(input$xrange) & data$X<=max(input$xrange) & data$k==1,], 
                      aes(x=X,y=Y))+
        geom_point(color = "cornflowerblue", alpha = .6, size = 4) +
        stat_smooth(method = "lm",
                    se = T,
                    color = "cornflowerblue", alpha = .3,
                    linetype = "dashed",
                    size = .75) +
        geom_point(data = data, color = "gray", alpha = .25, size=4) +
        geom_smooth(data = data, method = "lm", se=F, color="black", linetype = "dashed", size = .25) +
        theme_bw() + 
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.border = element_rect(fill=NA,size=1,linetype="solid")) +
        coord_cartesian(xlim = c(-125, 125), ylim = c(500,13000))
   
    scatter
    # hist_x <- ggplot(data[data$X>=min(input$xrange) & data$X<=max(input$xrange),], aes(x=X)) +
    #     geom_histogram(color="cornflowerblue", fill = "cornflowerblue") +
    #     ylab("Count") +
    #     theme_bw() + 
    #     theme(panel.grid.minor = element_blank(),
    #           panel.grid.major = element_blank(),
    #           panel.border = element_rect(fill=NA,size=1,linetype="solid")) +
    #     labs(caption = "Shaded area indicates 95% confidence interval.")
    # 
    # hist_y <- ggplot(data[data$Y>=min(input$yrange) & data$Y<=max(input$yrange),], aes(x=Y)) +
    #     geom_histogram(color="cornflowerblue", fill = "cornflowerblue") +
    #     ylab("Count") +
    #     coord_flip() +
    #     theme_bw() + 
    #     theme(panel.grid.minor = element_blank(),
    #           panel.grid.major = element_blank(),
    #           panel.border = element_rect(fill=NA,size=1,linetype="solid"))
    # 
    # empty <- ggplot() +
    #     geom_point(aes(1,1), colour="white") +
    #     theme(axis.ticks=element_blank(),
    #            panel.background=element_blank(),
    #            axis.text.x=element_blank(), axis.text.y=element_blank(),
    #            axis.title.x=element_blank(), axis.title.y=element_blank()) 
    # 
    # grid.arrange(hist_x,empty,scatter,hist_y,
    #              ncol=2,
    #              nrow=2,
    #              widths=c(4, 1),
    #              heights=c(1, 4))
    
     
   })
})

# Run the application 
shinyApp(ui = ui, server = server)