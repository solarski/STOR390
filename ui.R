library(shiny)
library(broom)
library(shinyjs)
library(lazyeval)
library(tidyverse)

choices <- list.files(pattern="*.csv")

teamData <- read_csv("Teams.csv")
teams <- teamData$Team
team_names <- paste(teamData$TeamName, " (", teamData$Team, ")")
names(teams) <- team_names

playerData <- read_csv("Master.csv")
players <- playerData$Player
player_names <- paste(playerData$FirstName, " ", playerData$LastName, "(", playerData$Player, ")")
names(players) <- player_names

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6, selectInput("table1Selector", "Table for X variable", choices, "Batting.csv")),
                column(6, selectInput("xSelector", "X Variable from table", NULL, "Year"))
            ),
            fluidRow(
                column(6, selectInput("table2Selector", "Table for Y variable", choices, "Batting.csv")),
                column(6, selectInput("ySelector", "Y Variable from table", NULL, "HomeRuns"))
            ),
            
            selectInput("players", "Players to view:", players, multiple = TRUE),
            selectInput("teams", "Teams to view:", teams, multiple = TRUE),
            
            sliderInput("years", "Date range", min = 1850, max = 2020, value = c(1850, 2020), step = 1
            #            animate=animationOptions(interval=1, loop = FALSE,
            #                                    playButton = TRUE, pauseButton = TRUE)
            ),
            
            selectInput("color", "Color by", NULL),
            selectInput("facet", "Facet by", NULL),
            
            radioButtons("mutation", label="Transformation:", 
                             choices=c("None","Mean","Median","Max","Min"), inline=TRUE),
            radioButtons("plotType", label="Plot Type:", 
                         choices=c("Scatter","Scatter (Jitter)","Line","Bar","Histogram"), inline=TRUE),
            
            checkboxInput("regressionSelector", "Plot regression line", value = FALSE),
            actionButton("plotButton", "Plot!"),
            actionButton("animateButton", "Animate!")
        ),
        mainPanel(
            plotOutput("mainPlot",
                       hover = hoverOpts(
                           id = "plotHover",
                           delay = 300,
                           delayType = "debounce",
                           nullOutside = FALSE
                       )),
            tabsetPanel(
                tabPanel("Point Info (hover to get data)",
                         fluidRow(
                             column(10, dataTableOutput("plotHoverInfo"))
                         )),
                tabPanel("Join Info", textOutput("joinText"))
            )
        )
    )
))