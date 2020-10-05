
library(shiny)
    library(shinydashboard)
    library(dashboardthemes)
    library(dplyr)
    library(ggplot2)
    library(rsconnect)
    library(googlesheets4)
    library(ggiraph)
    library(shinyWidgets)

# Setup:
########
# 1) (register Google account, if not available)
# 2) enter gmail address in gs4_auth() command below (replace 'mail@mail.mail)
# 3) Set up Googlesheets for results and enter links below
# 4) uncomment code

# Authentication
#gs4_auth(
 #   cache = ".secrets",
  #  email = "mail@mail.mail
#)

# Sheet for Prisoner's Dilemma results
#sheet1 <- "https://docs.google.com/spreadsheets/d/"

# Sheet for Tragedy of the Commons results
#sheet2 <- "https://docs.google.com/spreadsheets/d/"


# This creates the blank sheet used for the expunge <- specify to create pure empty sheet with one row
lab <- c(" "," ")
    ex <- data.frame(matrix(nrow=1,data=lab))
    rm(lab)
    
# Table for single-round prisoner's dilemma info
pristab <- c("<table class='extab'>
        <tr style='border-bottom:2px solid rgb(140,140,140)'>
          <td colspan='100%'></td>
        </tr>
        <tr>
          <th> </th>
          <th> </th>
          <th colspan='2'>Your neighbor</th>
        </tr>
        <tr>
          <th> </th>
          <th> </th>
          <th>Help</th>
          <th>Cheat</th>
        </tr>
        <tr style='border-bottom:2px solid rgb(140,140,140)'>
          <td colspan='100%'></td>
        </tr>
        <tr>
          <td rowspan='2' style='vertical-align:middle;'>You</td>
          <td>Help</td>
          <td>3,3</td>
          <td>0,5</td>
        </tr>
        <tr>
          <td>Cheat</td>
          <td>5,0</td>
          <td>1,1</td>
        </tr>
        <tr style='border-bottom:2px solid rgb(140,140,140)'>
          <td colspan='100%'></td>
        </tr>
      </table>")

ui <- dashboardPage(
  dashboardHeader(title="Results"),
  dashboardSidebar(disable = F,collapsed = T,width = 200,
            sidebarMenu(id="tabs",
            menuItem("Main menu",tabName = "main",selected = T),
            menuItem("Game 1",tabName = "pris",
                     menuSubItem("Round 1",tabName = "pris_r1"),
                     menuSubItem("Round 2",tabName = "pris_r2")),
            menuItem("Game 2",tabName = "trag"))
                    ),
  dashboardBody(
    tags$style(".small-box.bg-yellow { background-color: #A9A9A9 !important; color=##000000 !important; }"),
     tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    shinyDashboardThemes(theme = "grey_dark"),
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                       box(width = 12,title = "Welcome",solidHeader = F, collapsible = F,
                           p("This application allows you to retrieve and display the results of games played
                             in the 'Interactive Game Theory Application'. To start, select a game and round
                             in the menu on the left-hand side.")
                       )
              )
      ),
  # Prisoner's dilemma, single round
  ##################################
      tabItem(tabName = "pris_r1",
        fluidRow(
          box(width=12,color="black",title="Fetch/refresh & delete data",solidHeader = T,collapsible = T,collapsed = F,
              column(6,
              actionButton(inputId = "fetch_prisr1",label="Fetch/refresh results",icon("arrow-circle-down",lib="font-awesome"))
                    ),
              column(6,
              actionButton(inputId = "expunge_prisr1",label="Delete data",icon("exclamation-circle",lib="font-awesome"),style="color: red")
                    )
              )
            ),
        fluidRow(width=12,
          column(width = 6,
                box(width=NULL,color="black",title = "Translating outcomes to numbers",solidHeader = T,collapsible = F,
                    align="center",
                  # tags$ul(
                  #   tags$li("You and your neighbor both help each other: You gain 3 points."),
                  #   tags$li("You help your neighbor but they cheat you: You gain 0 points."),
                  #   tags$li("Your neighbor helps you but you cheat them: You gain 5 points."),
                  #   tags$li("You both cheat each other: You gain 1 point.")
                  # )
                  tags$div(
                    HTML(pristab)
                          )
                ), 
                box(width=NULL,color="black",title = "Strategies played",solidHeader = T,collapsible = T,collapsed = T,
                    align="center",
                    verbatimTextOutput("prisr1_strats")
                    ),
                box(width=NULL,color="black",title = "Your outcome by strategy played",solidHeader = T,collapsible = T,collapsed = T,
                    align="center",
                  verbatimTextOutput("prisr1_res")
                ) 
                ),
          column(width = 6,
                 box(width=NULL,color="black", title = "Average outcomes by strategy",solidHeader = T,collapsible = T,collapsed = T,
                     plotOutput("prisr1_plot")
                     )
                 )
        )
      ),
  ##################################
  
  # Prisoner's dilemma, repeated
  ##############################
  tabItem(tabName = "pris_r2",
          fluidRow(
          box(width=12,color="black",title="Fetch/refresh & delete data",solidHeader = T,collapsible = T,collapsed = F,
              column(6,
              actionButton(inputId = "fetch_prisr2",label="Fetch/refresh results",icon("arrow-circle-down",lib="font-awesome"))
                    ),
              column(6,
              actionButton(inputId = "expunge_prisr2",label="Delete data",icon("exclamation-circle",lib="font-awesome"),style="color: red")
                    )
              )
            ),
          fluidRow(
            column(width=6,
                   box(width=NULL,title = "Which strategy worked better?",collapsible = T,collapsed = T,solidHeader = T,
                       girafeOutput("r2scat")
                       )
                   ),
            column(width=6,
                   box(width=NULL,title = "What were they doing?",collapsible = T,collapsed = T,solidHeader = T,
                       girafeOutput("r2_det")
                       )
                   )
          )
          ),
  ##############################
      
  # Tragedy of the commons, R1
  ############################
      tabItem(tabName = "trag",
        fluidRow(
          box(width=12,color="black",title="Fetch/refresh & delete data",solidHeader = T,collapsible = T,collapsed = F,
              align="center",
              column(4,
              awesomeRadio(inputId = "selround",label="Select round",inline = T,status = "warning",
                           choices = c(1,2,3))
                ),
              column(4,
              actionButton(inputId = "fetch_tragr",label="Fetch/refresh results",icon("arrow-circle-down",lib="font-awesome"))
                    ),
              column(4,
              actionButton(inputId = "expunge_tragr",label="Delete data",icon("exclamation-circle",lib="font-awesome"),style="color: red")
                    )
              )
        ),
        fluidRow(
            box(color="black",title = "Premises",solidHeader = T,collapsible = T,collapsed = T,
              valueBoxOutput("size",width = 6),
              valueBoxOutput("prem",width = 6) #
                ),
            box(color="black",title = "Outcomes",solidHeader = T,collapsible = T,collapsed = T,
              valueBoxOutput("sumbox"),
              valueBoxOutput("avbox"),
              valueBoxOutput("maxbox")
              )),
        fluidRow(
          box(title = "Cooperators",solidHeader = F,align="center",collapsible = T,collapsed = T,
            tableOutput('coop')), 
          box(title = "Defectors",solidHeader = F,align="center",collapsible = T,collapsed = T,
              tableOutput('defe')
              )
  )
  )
  ############################
  
  ######################
)
)
)

server <- function(session, input, output) { 
    vals <- reactiveValues()
    
    # Prisoner's dilemma, single round
    ##################################
    observeEvent(input$fetch_prisr1,{
      vals$prisr1 <- range_read(ss=sheet1,sheet = "single_round",col_types = "cii",
                                col_names = c("Player","Outcome","Computer"))
      # Encode strategies played
      vals$prisr1$strat_p <- ifelse(vals$prisr1$Outcome==3 | vals$prisr1$Outcome==0 ,"Cooperate","Defect")
      vals$prisr1$strat_c <- ifelse(vals$prisr1$Computer==3 | vals$prisr1$Computer==0 ,"Cooperate","Defect")
      print(vals$prisr1)
      
      # generate table of strategies
      output$prisr1_strats <- renderPrint(table(vals$prisr1$strat_p,vals$prisr1$strat_c,dnn = c("You","Computer")))
      
      # bar graph, payoffs by strategy played
      output$prisr1_plot <- renderPlot({
        ggplot(vals$prisr1,aes(x=strat_p,y=Outcome)) +
          geom_bar(stat="summary",fill="grey75") +
          xlab("Your strategy") +
          theme_dark() +
            theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) +
            theme(panel.grid.major.y = element_line(color="white")) +
            theme(panel.grid.major.x = element_blank()) +
            theme(panel.grid.minor.y = element_blank()) +
            theme(plot.background = element_rect(fill="#343e48", color="#343e48")) +
            theme(axis.text = element_text(colour = "white")) +
            theme(axis.title = element_text(color = "white"))
            })
      
      # Results, by strategy
     output$prisr1_res <- renderPrint(with(vals$prisr1, tapply(Outcome, list("You"=strat_p,"Computer"=strat_c),mean)))
    }) # generates all results output
    observeEvent(input$expunge_prisr1,{
     range_flood(sheet1,sheet = "single_round")
      showNotification("All data have been deleted!",type="error")
    })
    ##################################
    
    # Prisoner's dilemma, repeated
    ##############################
    observeEvent(input$fetch_prisr2,{
    showModal(modalDialog("Data are being downloaded, please wait...", footer=NULL))  
     meta <- gs4_get(sheet1)
     names <- meta$sheets$name[meta$sheets$name!="single_round"]
     vals$r2data <- sapply(names,function(name){
                            range_read(ss=sheet1,sheet=name)},simplify = F,USE.NAMES = TRUE)
     plays <- as.data.frame(t(sapply(vals$r2data, function(x){apply(x,2,sum)})))[c(2,3,6)]
     plays$player <- rownames(plays)
     removeModal()
     
     plays$ttip <- c(paste0(plays$player,"\n Times cooperated: ",plays$coop,"\n Overall result: ",plays$You,
                            "\n Computer: ",plays$Computer))
     
     output$r2scat <- renderGirafe({
      scatter <- ggplot(plays,aes(x=coop,y=You)) +
        geom_point_interactive(aes(tooltip=ttip, data_id=player),shape=16,size=4,alpha=.75,color="white") +
        ylab("Summed gains") + xlab("Number of times cooperated") +
        theme_minimal() +
            theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) +  
            theme(panel.grid.major.y = element_blank()) +
            theme(panel.grid.major.x = element_blank()) +
            theme(panel.grid.minor.x = element_blank()) +
            theme(panel.grid.minor.y = element_blank()) +
            theme(plot.background = element_rect(fill="#343e48", color="#343e48")) +
            theme(axis.text = element_text(colour = "white",size = 14)) +
            theme(axis.title = element_text(color = "white",size = 14)) 
      
      x <- girafe(ggobj = scatter, fonts = list(sans = "Arial"),
                  options = list(opts_toolbar(position="bottomright"),
                                             opts_toolbar(saveaspng = FALSE), 
                                             opts_hover(css = "fill:orange;cursor:pointer;stroke:black;r:5pt"),
                                             opts_selection(type = "single", css = "fill:orange;stroke:black;r:5pt")))
      x
    })
    })
    observeEvent(input$r2scat_selected,{
    colors <- c("You"="orange","Computer"="grey75")
    print(input$r2scat_selected)
      
    output$r2_det <- renderGirafe({
      p <-  ggplot(vals$r2data[[input$r2scat_selected]],aes(x=Round)) + 
        geom_step_interactive(aes(y=you_sum,color="You")) +
        geom_step_interactive(aes(y=comp_sum,color="Computer"),direction="vh") +
        theme_minimal() +
            theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) +  
            theme(panel.grid.major.y = element_blank()) +
            theme(panel.grid.major.x = element_blank()) +
            theme(panel.grid.minor.x = element_blank()) +
            theme(panel.grid.minor.y = element_blank()) +
            theme(plot.background = element_rect(fill="#343e48", color="#343e48")) +
            theme(axis.text = element_text(colour = "white",size = 14)) +
            theme(axis.title = element_text(color = "white",size = 14)) +
            theme(legend.position = c(.125,.9),
                  legend.text = element_text(color = "white",face = "bold"),
                  legend.background = element_rect(color = "white",fill = "#343e48"),
                  legend.title = element_blank()) +
        labs(y="Summed gains",
             x="Round",
             color="") +
        scale_color_manual(values = colors, labels=c("Computer",input$r2scat_selected))
        
      
     y <- girafe(ggobj = p, fonts = list(sans = "Arial"),
                 options = list(opts_toolbar(position="bottomright"),
                                           opts_zoom(max = 5),
                                           opts_toolbar(saveaspng = FALSE)))
     y
    })
    })
    observeEvent(input$expunge_prisr2,{
      sapply(names,function(name){sheet_delete(sheet1,sheet = name)})
      showNotification("All data have been deleted!",type="error")
    })
    ##############################
    
    # Tragedy of the commons 
    ########################
    observeEvent(input$fetch_tragr,{
    vals$data <- range_read(ss=sheet2,sheet=input$selround,col_types = "ci",col_names = F)  
        vals$data <- na.omit(vals$data)
        vals$data <- rename(vals$data,Name=...1)
        vals$data <- rename(vals$data,Coins=...2)
        vals$size <- length(vals$data$Name)
        vals$fair <- round(100/vals$size,digits = 1)
        
    vals$data <- vals$data[order(-vals$data$Coins),]
        vals$sum <- sum(vals$data$Coins,na.rm = T)
        vals$mean <- round(mean(vals$data$Coins,na.rm = T),digits=1)
        vals$max <- round(max(vals$data$Coins,na.rm = T),digits = 1)
        
    vals$coop <- filter(vals$data,Coins<=100/vals$size) 
    vals$defe <- filter(vals$data,Coins>100/vals$size)
    
    output$prem <- renderValueBox({
            valueBox(vals$fair,"Maximum available coins per student",color="yellow")
    })
    output$size <- renderValueBox({
            valueBox(vals$size,"Number of students",color = "yellow")
    })
    
    if(vals$sum<=100){
    output$sumbox <- renderValueBox({
        valueBox(vals$sum,"Sum of all claims",color = "green")
    })}else{
    output$sumbox <- renderValueBox({
        valueBox(vals$sum,"Sum of all claims",color = "red")
    })}
    if(vals$mean<=vals$fair){
    output$avbox <- renderValueBox({
        valueBox(vals$mean,"Average claim",color = "green")
    })}else{
      output$avbox <- renderValueBox({
        valueBox(vals$mean,"Average claim",color = "red")
      })}
    if(vals$max<=vals$fair){
    output$maxbox <- renderValueBox({
        valueBox(vals$max,"Largest claim",color = "green")
    })}else{
       output$maxbox <- renderValueBox({
        valueBox(vals$max,"Largest claim",color = "red")
    })}
    # Cooperators
    output$coop <- renderTable(vals$coop)
    # Defectors
    output$defe <- renderTable(vals$defe)
    })
    # Expunge data in Google Sheet & reset game
    observeEvent(input$expunge_tragr,{
        range_flood(sheet2,sheet = "1")
        showNotification("All data have been deleted!",type="error")
    })
}

shinyApp(ui, server)