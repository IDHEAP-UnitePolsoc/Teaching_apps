
library(shiny)
    library(shinydashboard)
    library(dashboardthemes)
    library(shinyjs)
    library(shinyWidgets)
    library(googlesheets4)
    library(rsconnect)

# Setup:
########
# 1) (register Google account, if not available)
# 2) enter gmail address in gs4_auth() command below (replace 'mail@mail.mail)
# 3) Set up Googlesheets for results and enter links below
# 4) uncomment code (also within server()!)

#gs4_auth(
 #   cache = ".secrets",
  #  email = "mail@mail.mail"
#)

# Sheet for Prisoner's Dilemma results
#sheet1 <- "https://docs.google.com/spreadsheets/"

# Sheet for Tragedy of the Commons results
#sheet2 <- "https://docs.google.com/spreadsheets/d/"

# Summary table
pristab <- c("<table class='extab'>
        <tr style='border-bottom:2px solid black'>
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
        <tr style='border-bottom:2px solid black'>
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
        <tr style='border-bottom:2px solid black'>
          <td colspan='100%'></td>
        </tr>
      </table>")


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(disable = T),
    dashboardSidebar(collapsed = T,
        sidebarMenu(
            id="tabs",
            menuItem("Welcome!",tabName = "welcome",selected = T),
            menuItem("Choice", tabName = "choice"),
            menuItem("Prisoner's Dilemma",tabName = "pris",
                     menuSubItem("Single round",tabName = "single"),
                     menuSubItem("Repeated",tabName = "repeat")),
            menuItem("Tragedy of the Commons", tabName = "commons",
                     menuSubItem("Round 1",tabName = "round1"),
                     menuSubItem("Round 2", tabName = "round2"),
                     menuSubItem("Round 3",tabName="round3")))
        ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        shinyDashboardThemes(theme = "grey_light"),
        tabItems(
            # Welcome tab
            ##########################
            tabItem(tabName = "welcome",
                    fluidRow(
                        column(12,
                                h2("Interactive Game Theory Application"),
                                box(title = "Welcome!",solidHeader=T,collapsible = F,width=NULL,
                                    p("This application features two cooperative games, one which you will
                                       play with (or against) the computer and another which you will play together with everyone in your
                                       class. Your results will be stored so that we can later on see how everyone played and what
                                       results their actions had. IMPORTANT: This is",em("not a test"),"and your performance in these games
                                       will not be graded! Once we are done, all stored results will be deleted."),
                                    p("To start, kindly enter your name (you can also use a pseudonym, but please make sure it is unique)
                                      into the field below and klick 'Start'."),
                                    column(width=6,
                                        textInput(inputId = "name",label = " ",placeholder = "Your name/pseudonym")),
                                    column(width=6,style = "margin-top: 20px;",
                                        actionButton(inputId = "start_main",label = "Start"))
                                    )
                               )
                    )
                    ),
            ##########################
            
            # Selection tab
            ##########################
            tabItem(tabName = "choice",
                    fluidRow(
                        column(12,
                               box(title = "Select a game",solidHeader=T,collapsible = F,width=NULL,
                                   p("Detailed instructions will be provided once a game is started."))
                               ),
                        column(6,
                               box(title = "Game 1: Help your neighbor",solidHeader = F,collapsible = F,width = NULL,height = 300,
                                   p("In this game, you play the role of a farmer who has to decide if and how to
                                     cooperate with their neighbor, also a farmer, in bringing in both of your
                                     harvests."),
                                   br(),
                                   column(width=6,align="center",
                                          textInput(inputId = "pw_start_pris",label = "",placeholder = "42",width = "50%"),
                                          actionButton(inputId = "start_pris",label = "Round 1")
                                          ),
                                   column(width=6,align="center",
                                          textInput(inputId = "pw_round2_pris",label = "",placeholder = "17",width="50%"),
                                          actionButton(inputId = "round2_pris",label = "Round 2")
                                          )
                                   )
                               ),
                        column(6,
                               box(title = "Game 2: Share a resource",solidHeader = F,collapsible = F,width = NULL,height = 300,
                                p("In this game, you and your classmates need to divide a limited resource 
                                  between yourselves. You only control how much of the resource you as an
                                  individual are using, but you need to make sure as a collective not to over-exploit 
                                  the resource."),
                                   column(4,align="center",
                                          textInput(inputId = "pw_start_r1",label = "",placeholder = "0404",width = "75%"),
                                          actionButton(inputId = "r1_trag",label = "Round 1")
                                          ),
                                    column(4,align="center",
                                          textInput(inputId = "pw_start_r2",label = "",placeholder = "503",width = "75%"),
                                          actionButton(inputId = "r2_trag",label = "Round 2")
                                          ),
                                    column(4,align="center",
                                          textInput(inputId = "pw_start_r3",label = "",placeholder = "451",width = "75%"),
                                          actionButton(inputId = "r3_trag",label = "Round 3")
                                          )
                                   )
                               )
                            )
                    ),
            ##########################
            
            # Prisoner's Dilemma - single round
            ###################################
            tabItem(tabName = "single",
                    fluidRow(
                             box(title = "Round 1",collapsible = F,solidHeader = T,width = NULL,
                                 p("You are a farmer and your crops are ready to be harvested.
                                   The problem is that you are alone and you cannot bring in the entire harvest before
                                   winter comes. Luckily, though, you have a neighbor who has the exact same problem.
                                   You could both combine your efforts and equipment and then bring in both
                                   harvests in time (3 points each)."),
                                 br(),
                                  p("Even better: You could get your neighbor to help you now and when the
                                  time comes to return the favor, you make up an excuse for not helping them. In that case,
                                  your harvest is taken care of and you get time for other things (5 points for you, 0 for them!)"),
                                 br(),
                                 p("Your neighbor could of course do the same: Get you to help them first but then leave 
                                    you alone later (5 points for them, 0 for you)."),
                                 br(),
                                 p("You need to choose what to do. You could be nice, offer your neighbor to help them
                                   first and trust them to return the favor later (there are no guarantees, though!).
                                   But you could also try to cheat your neighbor by asking for help now and then making up an 
                                   excuse for not helping them later."),
                                 br(),
                                 p("The table below summarizes this information."),
                                  tags$div(
                                           HTML(pristab)
                                            ),
                                 br(),
                                 p("You can choose only once."),
                                 br(),
                                 column(6,align="center",
                                        actionButton(inputId = "coop_r1",label = "Be nice",icon("hands-helping",lib="font-awesome"))
                                        ),
                                 column(6,align="center",
                                        actionButton(inputId = "defe_r1",label = "Cheat",icon("hand-rock",lib="font-awesome"))
                                        )
                                 )
                             )
                    ),
            ###################################
            
            # Prisoner's Dilemma - repeated
            ###############################
            tabItem(tabName = "repeat",
                    fluidRow(
                        box(title = "Round 2",collapsible = F,solidHeader = T,width = NULL,
                            p("The situtation is the same as before: You and your neighbor are farmers who need
                              to bring in their harvests. If you collaborate, you can get everything done but each one
                              of you has strong reasons to cheat. The difference: Last round, you played only once.
                              This round, you interact over the course of several harvests (200 in total)."),
                            br(),
                            p("This means that you can respond to your neighbor's actions - and they to yours.")
                            )
                        ),
                    fluidRow(
                        column(width=6,
                              box(width=NULL,title = "Choose your action",collapsible = F,
                                    column(width = 6,align="center",
                            actionButton(inputId = "coop_r2",label = "Be nice",icon("hands-helping",lib="font-awesome"))),
                        column(width = 6,align="center",
                            actionButton(inputId = "defe_r2",label = "Cheat",icon("hand-rock",lib="font-awesome")))
                        ),
                            box(width=NULL,align="center",title = "Current harvest",collapsible = F,
                                # Box with table for current results
                                tableOutput("tab_r2")),
                        ),
                        column(width=6,
                            box(width = NULL,title="Your progress over time",collapsible = F,
                                plotOutput("graph")
                           )
                        )
                     )
                    ),
            
            ###############################
        
            # Tragedy of the Commons - Round 1
            ##################################
            tabItem(tabName = "round1",
                    fluidRow(
                        box(title = "Round 1",collapsible = F,solidHeader = T,width = NULL,
                            p("You and your classmates need to divide 100 gold coins (or, 
                              if you consider that lame, something else you care about) between yourselves."),
                             p("You decide individually how many coins you want to take out: You can take out 
                               nothing (0), everything (100) or any amount in between."),
                            p("Take a moment to think about what everyone else will probably do and, given this, 
                                     what your best strategy is."),
                            ),
                        box(width = 6,align="center",
                            knobInput(inputId = "knob1",label = "Claim your share",
                                      value = 1,
                                      min = 0,
                                      max = 100,
                                      displayPrevious = TRUE, 
                                      lineCap = "round",
                                      fgColor = "#999999",
                                      inputColor = "#999999")
                            ),
                        box(width = 6,align="center",
                            actionButton(inputId = "submit_r1",label = "Submit")
                            )
                        )
                    ),
            ##################################
            
            # Tragedy of the Commons - Round 2
            ##################################
            tabItem(tabName = "round2",
                    fluidRow(
                        box(title = "Round 2",collapsible = F,solidHeader = T,width = NULL,
                            p("You and your classmates need to divide 100 gold coins (or, 
                              if you consider that lame, something else you care about) between yourselves."),
                             p("You decide individually how many coins you want to take out: You can take out 
                               nothing (0), everything (100) or any amount in between."),
                             p(strong("New rule:"),"If all of you combined take out more than is available, no one gets anything."),
                            p("Take a moment to think about what everyone else will probably do and, given this, 
                                     what your best strategy is."),
                            ),
                        box(width = 6,align="center",
                            knobInput(inputId = "knob2",label = "Claim your share",
                                      value = 1,
                                      min = 0,
                                      max = 100,
                                      displayPrevious = TRUE, 
                                      lineCap = "round",
                                      fgColor = "#999999",
                                      inputColor = "#999999")
                            ),
                        box(width = 6,align="center",
                            actionButton(inputId = "submit_r2",label = "Submit")
                            )
                        )
                    ),
            ##################################
            
            # Tragedy of the Commons - Round 3
            ##################################
            tabItem(tabName = "round3",
                    fluidRow(
                        box(title = "Round 3",collapsible = F,solidHeader = T,width = NULL,
                            p("You and your classmates need to divide 100 gold coins (or, 
                              if you consider that lame, something else you care about) between yourselves."),
                             p("You decide individually how many coins you want to take out: You can take out 
                               nothing (0), everything (100) or any amount in between."),
                             p(strong("New rule:"),"If all of you combined take out more than is available, no one gets anything -",
                               strong("except for"),"the three of you who made the highest claims."),
                            p("Take a moment to think about what everyone else will probably do and, given this, 
                                     what your best strategy is."),
                            ),
                        box(width = 6,align="center",
                            knobInput(inputId = "knob3",label = "Claim your share",
                                      value = 1,
                                      min = 0,
                                      max = 100,
                                      displayPrevious = TRUE, 
                                      lineCap = "round",
                                      fgColor = "#999999",
                                      inputColor = "#999999")
                            ),
                        box(width = 6,align="center",
                            actionButton(inputId = "submit_r3",label = "Submit")
                            )
                        )
                    )
            ##################################
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    vals <- reactiveValues() # <- container for values produced during interactions
    observeEvent(input$start_main,{
        vals$player <- input$name
        print(vals$player)
        newtab <- switch(input$tabs,
                     "welcome" = "choice",
                     "choice" = "welcome"
                     )
        updateTabItems(session,"tabs",newtab)
        disable("start_pris")
        disable("round2_pris")
        disable("r1_trag")
        disable("r2_trag")
        disable("r3_trag")
        })
    
    # Prisoner's Dilemmma - Round 1
    ###############################
    observeEvent(input$pw_start_pris,{ # activates Round 1
        if(input$pw_start_pris=="42" & input$start_pris==0){
            enable("start_pris")
        }else{NULL}
    })
    observeEvent(input$start_pris,{
        vals$table <- data.frame(Player=c(vals$player,"Your neighbor"),Result=c(0,0))
        vals$flip <- c(0,1)
        vals$flip <- sample(vals$flip,1,prob = c(.5,.5)) # <- this determines computer's actions
            print(vals$flip)
            print(vals$table)
        newtab <- switch(input$tabs,
                     "choice" = "single",
                     "single" = "choice"
                     )
        updateTabItems(session,"tabs",newtab)
    })
    observeEvent(input$coop_r1,{
        if(vals$flip==1){ # <- computer defects
        vals$table$Result <- c(0,5)
        output$table <- renderTable(vals$table)
        sendSweetAlert(
            session=session,
            title = "Too bad!",
            text = "Your neighbor betrayed you! Now their harvest is taken care of but you are empty-handed. You gain
            0 points, but your neighbor gets 5 points.",
            type = "error")
        output$table <- renderTable(vals$table) 
        } else { # <- computer cooperates
        vals$table$Result <- c(3,3)
        output$table <- renderTable(vals$table)
        sendSweetAlert(
            session=session,
            title = "Yay!",
            text = "Your neighbor helped you too! Both of your harvests are in and winter can come. Both of you gain 3 points.",
            type = "success")
        output$table <- renderTable(vals$table) 
        }
     isolate(data  <-  vals$table) # <- save results and write to GoogleSheet
        comp <- data[2,2]
        data <- data[1,]
        data["comp"] <- NA
        data$comp <- comp
          rm(comp)
    #sheet_append(ss=sheet1,data,sheet = "single_round")
    newtab <- switch(input$tabs,
                     "single" = "choice",
                     "choice" = "single"
                     )
    updateTabItems(session,"tabs",newtab)
    disable("start_pris")
    })
    observeEvent(input$defe_r1,{
    if(vals$flip==1){ # <- computer defects
    vals$table$Result <- c(1,1)
        output$table <- renderTable(vals$table)
        sendSweetAlert(
            session=session,
            title = "Oh no!",
            text = "You did not help each other. Now a part of both of your harvests will spoil. Each of you gets 1 point.",
            type = "error"
         )
        output$table <- renderTable(vals$table) 
        } else { # <- computer cooperates
        vals$table$Result <- c(5,0)
        output$table <- renderTable(vals$table)
        sendSweetAlert(
            session=session,
            title = "Oh yeah!",
            text = "Your neighbor helped you, but you totally betrayed them! Your harvest is in and they will go hungry this winter. 
            You gain 5 points, they get 0.",
            type = "success"
         )
        output$table <- renderTable(vals$table) 
        }
     isolate(data <- vals$table) # <- save results and write to GoogleSheet
        comp <- data[2,2]
        data <- data[1,]
        data["comp"] <- NA
        data$comp <- comp
          rm(comp)
    #sheet_append(ss=sheet1,data,sheet = "single_round")
    newtab <- switch(input$tabs,
                     "single" = "choice",
                     "choice" = "single"
                     )
    updateTabItems(session,"tabs",newtab)
    disable("start_pris")
    })
    
    ###############################
    
   # Prisoner's Dilemma - Round 2
   ##############################
   observeEvent(input$pw_round2_pris,{ # activates Round 2
        if(input$pw_round2_pris=="17" & input$round2_pris==0){
            enable("round2_pris")
        }else{NULL}
    })
   observeEvent(input$round2_pris,{
       newtab <- switch(input$tabs,
                     "choice" = "repeat",
                     "repeat" = "choice")
        updateTabItems(session,"tabs",newtab)
       vals$count <- 0 # <- counter for number of games
       vals$strat <- 0 # <- computer's strategy; "cooperate" (=0) in first round
       vals$pris_r2 <- data.frame(Round=integer(200),You=integer(200),Computer=integer(200),
                             you_sum=integer(200),comp_sum=integer(200)) 
       vals$pris_r2$Round[1] <- 1
       output$tab_r2 <- renderTable(vals$pris_r2[vals$count,1:3],digits = 0,align = 'c')
   })
   # Player defects
   observeEvent(input$defe_r2,{
       print("defect")
       vals$count <- vals$count+1
       vals$pris_r2$Round[vals$count] <- vals$count
       if(vals$strat==0){ # <- computer cooperates
          vals$strat <- 1 # <- computer will defect next time
          vals$pris_r2$You[vals$count] <- 5
          vals$pris_r2$Computer[vals$count] <- 0
            vals$pris_r2$you_sum <- cumsum(vals$pris_r2$You)
            vals$pris_r2$comp_sum <- cumsum(vals$pris_r2$Computer)
            print(vals$pris_r2$comp_sum[vals$count])
      # Table for this round
          output$tab_r2 <- renderTable(vals$pris_r2[vals$count,1:3],digits = 0,align = 'c')
       }else{ # <- computer defects
        vals$strat <- 1 # <- computer will defect next time
        vals$pris_r2$You[vals$count] <- 1
        vals$pris_r2$Computer[vals$count] <- 1
            vals$pris_r2$you_sum <- cumsum(vals$pris_r2$You)
            vals$pris_r2$comp_sum <- cumsum(vals$pris_r2$Computer)
            print(vals$pris_r2$comp_sum[vals$count])
        output$tab_r2 <- renderTable(vals$pris_r2[vals$count,1:3],digits = 0,align = 'c')
       }
     if(vals$count==200){
    disable("coop_r2")
    disable("defe_r2")
    data <-  vals$pris_r2 # <- export to GoogleSheet
      data <- data[which(data$Round!=0),]
      data$coop <- ifelse(data$You==3 | data$You==0 ,1,0)
    sheet_write(data,ss=sheet1,sheet = input$name)
    showNotification("Awesome! Your results have been saved!",type="warning")
    newtab <- switch(input$tabs,
                     "repeat" = "choice",
                     "choice" = "repeat"
                     )
        updateTabItems(session,"tabs",newtab)
        disable("round2_pris")
  }
   })
   # Player cooperates
   observeEvent(input$coop_r2,{
      print("cooperate")
      vals$count <- vals$count+1
      vals$pris_r2$Round[vals$count] <- vals$count
      if(vals$strat==0){ # <- computer cooperates
          vals$strat <- 0 # <- computer will cooperate next time
          vals$pris_r2$You[vals$count] <- 3
          vals$pris_r2$Computer[vals$count] <- 3
            vals$pris_r2$you_sum <- cumsum(vals$pris_r2$You)
            vals$pris_r2$comp_sum <- cumsum(vals$pris_r2$Computer)
            print(vals$pris_r2$comp_sum[vals$count])
          output$tab_r2 <- renderTable(vals$pris_r2[vals$count,1:3],digits = 0,align = 'c')
      }else{ # <- computer defects
          vals$strat <- 0 # <- computer will cooperate next time
          vals$pris_r2$You[vals$count] <- 0
          vals$pris_r2$Computer[vals$count] <- 5
            vals$pris_r2$you_sum <- cumsum(vals$pris_r2$You)
            vals$pris_r2$comp_sum <- cumsum(vals$pris_r2$Computer)
            print(vals$pris_r2$comp_sum[vals$count])
          output$tab_r2 <- renderTable(vals$pris_r2[vals$count,1:3],digits = 0,align = 'c')
      }
   if(vals$count==200){
    disable("coop_r2")
    disable("defe_r2")
    data <-  vals$pris_r2
      data <- data[which(data$Round!=0),]
      data$coop <- ifelse(data$You==3 | data$You==0 ,1,0)
   #sheet_write(data,ss=sheet1,sheet = input$name)
    showNotification("Awesome! Your results have been saved!",type="warning")
    newtab <- switch(input$tabs,
                     "repeat" = "choice",
                     "choice" = "repeat"
                     )
        updateTabItems(session,"tabs",newtab)
        disable("round2_pris")
  }
  })
   output$graph <- renderPlot({
   plot(vals$pris_r2$Round[1:vals$count],vals$pris_r2$you_sum[1:vals$count]+0.1,type = 'S',col="red",
        xlab = "Round",ylab="Overall sum",ylim = c(0,max(vals$pris_r2$comp_sum[1:vals$count],vals$pris_r2$you_sum[1:vals$count])))
    lines(vals$pris_r2$Round[1:vals$count],vals$pris_r2$comp_sum[1:vals$count]-0.1,type = 'S',col="blue")
    legend("topleft",legend=c("You","Computer"),col=c("red","blue"),lty = 1,cex=0.8)
  })
   ##############################
   
   # Commons - Round 1
   ###################
   observeEvent(input$pw_start_r1,{
     if(input$pw_start_r1=="0404" & input$r1_trag==0){
       enable("r1_trag")
     }else{NULL}
   })
   observeEvent(input$r1_trag,{
     newtab <- switch(input$tabs,
                      "choice" = "round1",
                      "round1" = "choice")
     updateTabItems(session,"tabs",newtab)
     vals$r1 <- data.frame(matrix(ncol = 2,nrow=0))
   })
   observeEvent(input$submit_r1,{
        newline <- isolate(c(input$name,input$knob1))
        isolate(data <- rbind(as.data.frame(vals$r1),unlist(newline)))
        #sheet_append(ss=sheet2,data,sheet = "1")
    showNotification("Awesome! Your results have been saved!",type="warning")
    newtab <- switch(input$tabs,
                      "round1" = "choice",
                      "choice" = "round1")
     updateTabItems(session,"tabs",newtab)
     disable("r1_trag")
    })
   ###################
   
   # Commons - Round 2
   ###################
   observeEvent(input$pw_start_r2,{
     if(input$pw_start_r2=="503" & input$r2_trag==0){
       enable("r2_trag")
     }else{NULL}
   })
   observeEvent(input$r2_trag,{
     newtab <- switch(input$tabs,
                      "choice" = "round2",
                      "round2" = "choice")
     updateTabItems(session,"tabs",newtab)
     vals$r2 <- data.frame(matrix(ncol = 2,nrow=0))
   })
   observeEvent(input$submit_r2,{
        newline <- isolate(c(input$name,input$knob2))
        isolate(data <- rbind(as.data.frame(vals$r2),unlist(newline)))
        #sheet_append(ss=sheet2,data,sheet = "2")
    showNotification("Awesome! Your results have been saved!",type="warning")
    newtab <- switch(input$tabs,
                      "round2" = "choice",
                      "choice" = "round2")
     updateTabItems(session,"tabs",newtab)
     disable("r2_trag")
    })
   ###################
   
   # Commons - Round 3
   ###################
   observeEvent(input$pw_start_r3,{
     if(input$pw_start_r3=="451" & input$r3_trag==0){
       enable("r3_trag")
     }else{NULL}
   })
   observeEvent(input$r3_trag,{
     newtab <- switch(input$tabs,
                      "choice" = "round3",
                      "round3" = "choice")
     updateTabItems(session,"tabs",newtab)
     vals$r3 <- data.frame(matrix(ncol = 2,nrow=0))
   })
   observeEvent(input$submit_r3,{
        newline <- isolate(c(input$name,input$knob3))
        isolate(data <- rbind(as.data.frame(vals$r3),unlist(newline)))
        #sheet_append(ss=sheet2,data,sheet = "3")
    showNotification("Awesome! Your results have been saved!",type="warning")
    newtab <- switch(input$tabs,
                      "round3" = "choice",
                      "choice" = "round3")
     updateTabItems(session,"tabs",newtab)
     disable("r3_trag")
    })
   ###################
}

# Run the application 
shinyApp(ui = ui, server = server)
