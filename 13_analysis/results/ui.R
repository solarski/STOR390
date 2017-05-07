library(shiny)
library(broom)
library(stringr)
library(shinyjs)
library(lazyeval)
library(tidyverse)

fileChoices <- list.files(path = "../data/", pattern="*.csv")

teamData <- read_csv("../data/Teams.csv") %>% arrange(Team) %>% group_by(Team) %>% mutate(newName = paste0(unique(TeamName), " (", unique(Team), ")", collapse = ", ", sep = ""))
teams <- as.list(unique(teamData$Team))
team_names <- as.list(unique(teamData$newName))
names(teams) <- team_names

playerData <- read_csv("../data/Master.csv")
playersFull <- paste(playerData$Player, ':', playerData$FirstName, " ", playerData$LastName, "(", playerData$Player, ")", sep = "")
players <- lapply(str_split(playersFull, ":"), `[[`, 1)
names(players) <- lapply(str_split(playersFull, ":"), `[[`, 2)

shinyUI(fluidPage(
    titlePanel(
        "Baseball by the Numbers"
    ),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6, selectInput("table1Selector", "Table for X variable", fileChoices, "Batting.csv")),
                column(6, selectInput("xSelector", "X Variable from table", NULL, "Year"))
            ),
            fluidRow(
                column(6, selectInput("table2Selector", "Table for Y variable", fileChoices, "Batting.csv")),
                column(6, selectInput("ySelector", "Y Variable from table", NULL, "HomeRuns"))
            ),
            
            selectInput("players", "Players to view:", players, multiple = TRUE),
            selectInput("teams", "Teams to view:", teams, multiple = TRUE),
            
            sliderInput("years", "Date range", min = 1850, max = 2020, value = c(1850, 2020), step = 1),
            
            selectInput("color", "Color by", NULL),
            selectInput("facet", "Facet by", NULL),
            
            radioButtons("mutation", label="Transformation:", 
                             choices=c("None","Mean","Median","Max","Min"), inline=TRUE),
            radioButtons("plotType", label="Plot Type:", 
                         choices=c("Scatter","Scatter (Jitter)","Line","Bar","Histogram"), inline=TRUE),
            
            checkboxInput("regressionSelector", "Plot regression line", value = FALSE),
            actionButton("plotButton", "Plot!")
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
                tabPanel("Join Info", textOutput("joinText")),
                tabPanel("Historical Rules and Changes", pre(includeText("history.txt")))
            )
        )
    )
))