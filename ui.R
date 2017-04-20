library(shiny)

choices <- list.files(pattern="*.csv")

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    sidebarLayout(
        sidebarPanel(
            selectInput("table1Selector", "Table for X variable", choices, "Batting.csv"),
            selectInput("xSelector", "X Variable from table", NULL),
            selectInput("table2Selector", "Table for Y variable", choices, "Batting.csv"),
            selectInput("ySelector", "Y Variable from table", NULL),
            checkboxInput("meanSelector", "Plot average", value = FALSE),
            checkboxInput("regressionSelector", "Plot regression line", value = FALSE),
            actionButton("plotButton", "Plot!")
        ),
        mainPanel(
            plotOutput("mainPlot"),
            textOutput("joinText"),
            dataTableOutput('regressionTable')
        )
    )
))