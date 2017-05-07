library(shiny)
library(broom)
library(shinyjs)
library(lazyeval)
library(tidyverse)

choices <- list.files(pattern="../data/*.csv")
defaultsX <- list("Batting.csv" = "Year", "Salaries.csv" = "Year")
defaultsY <- list("Batting.csv" = "HomeRuns", "Salaries.csv" = "Salary")

shinyServer(function(input, output, session) {
    
    theme_set(theme_gray(base_size = 18))
    
    teamData <- read_csv("../data/Teams.csv")
    teams <- teamData$Team
    
    #Some shared variables that change
    files <- reactiveValues(file1 = NULL)
    names <- reactiveValues(names = NULL)
    data <- reactiveValues(data = NULL, mutatedData = NULL)
    
    #These two observes detect when the tables have been changed (input$table<1,2>Selector)
    #They take the file, load the data, and update the variable selector
    observe({
        files$file1 <- input$table1Selector
        
        if(!is.na(files$file1) && files$file1 != ""){
            tryCatch({
                filename <- paste("../data/", files$file1, sep = "")
                data$data1 <- read_csv(filename)
                
                names$names <- c("None", colnames(data$data1), colnames(data$data2))
                
                updateSelectInput(session, "color",
                                  choices = names$names
                )
                
                updateSelectInput(session, "facet",
                                  choices = names$names
                )
                
                # Can also set the label and select items
                updateSelectInput(session, "xSelector",
                                  choices = colnames(data$data1),
                                  selected = defaultsX[input$table1Selector]
                )
            })
        }
    })
    
    observe({
        files$file2 <- input$table2Selector
        
        if(!is.na(files$file2) && files$file2 != ""){
            tryCatch({
                data$data2 <- read_csv(paste("../data/", files$file2, sep = ""))
                
                names$names <- c("None", colnames(data$data1), colnames(data$data2))
                
                updateSelectInput(session, "color",
                                  choices = names$names
                )
                
                updateSelectInput(session, "facet",
                                  choices = names$names
                )
                
                # Can also set the label and select items
                updateSelectInput(session, "ySelector",
                                  choices = colnames(data$data2),
                                  selected = defaultsY[input$table2Selector]
                )
            })
        }
    })
    
    #Join the two selected data sets and print out the columns which were joined on for reference
    outputJoin <- function(){
        #If the files match, don't join
        if(files$file1 == files$file2){
            data$data <- data$data1
            output$joinText <- renderText({
                paste("No tables have been joined.")
            })
        } else {
            #If the files are different, print out the joined columns
            #Which is just the intersection of columns
            #And join them
            output$joinText <- renderText({
                joined_columns <- paste(intersect(colnames(data$data1), colnames(data$data2)), collapse = ', ')
                joined_sentence <- paste("Joined on: ", joined_columns)
                print(jointed_sentence)
                joined_sentence
            })
            data$data <- inner_join(data$data1, data$data2)
        }
    }
    
    #Fill the table with the points that have been hovered over
    output$plotHoverInfo <- renderDataTable({
        tryCatch({
            nearPoints(data$mutatedData, input$plotHover, threshold = 20)
        })
    }, options = list(scrollX = TRUE))
    
    #The main piece of code which generates the plot
    output$mainPlot <- renderPlot({
        #Determine the variables you want to plot, and whether or not to take the mean/regress
        x_axis <- isolate(input$xSelector)
        x_name <- x_axis
        y_axis <- isolate(input$ySelector)
        y_name <- y_axis
        regression <- isolate(input$regressionSelector)
        
        year_min <- isolate(input$years[1])
        year_max <- isolate(input$years[2])
        
        plot_type <- isolate(input$plotType)
        color <- isolate(input$color)
        facet <- isolate(input$facet)
        mutation <- isolate(input$mutation)
        
        #Run whenever the plot button is clicked
        input$plotButton
        
        #If all of our data is valid
        if(!is.null(x_axis) && !(x_axis == "") && !is.null(y_axis) && !(y_axis == "") 
           && !is.null(data$data1) && !is.null(data$data2)){
            
            outputJoin()
            
            mapping <- NULL
            new_data <- NULL
            reg_type <- NULL
            graph_type <- NULL
            coord_type <- NULL
            facet_type <- NULL
            title_type <- NULL
            
            new_data <- data$data
            
            #Filtering (Player, Team, Year)
            if('Year' %in% colnames(new_data)){
                new_data <- filter(new_data, Year > year_min, Year < year_max)
            }
            if('Player' %in% colnames(new_data)){
                if(!is.null(isolate(input$players)) && !is.na(isolate(input$players))){
                    new_data <- filter(new_data, Player %in% isolate(input$players))
                }
            }
            if('Team' %in% colnames(new_data)){
                if(!is.null(isolate(input$teams)) && !is.na(isolate(input$teams))){
                    new_data <- filter(new_data, Team %in% isolate(input$teams))
                }
            }
            
            #If mean is checked, group by the x variable and take the mean of the y
            #Also, set the graph to be a line. Otherwise, make it a jitter plot
            if(mutation == "Mean"){
                if(facet != "None"){
                    new_data <- new_data %>% group_by_(x_axis, facet) %>% 
                        summarise_(avg = interp(~mean(var, na.rm=TRUE), var = as.name(y_axis)))
                } else {
                    new_data <- new_data %>% group_by_(x_axis) %>% 
                        summarise_(avg = interp(~mean(var, na.rm=TRUE), var = as.name(y_axis)))
                }
                y_name <- paste("Average ", y_axis)
                y_axis <- "avg"
            } else if(mutation == "Median"){
                if(facet != "None"){
                    new_data <- new_data %>% group_by_(x_axis, facet) %>% 
                        summarise_(med = interp(~median(var, na.rm=TRUE), var = as.name(y_axis)))
                } else {
                    new_data <- new_data %>% group_by_(x_axis) %>% 
                        summarise_(med = interp(~median(var, na.rm=TRUE), var = as.name(y_axis)))
                }
                y_name <- paste("Median ", y_axis)
                y_axis <- "med"
            } else if(mutation == "Max"){
                if(facet != "None"){
                    new_data <- new_data %>% group_by_(x_axis, facet) %>% 
                        summarise_(max = interp(~max(var, na.rm=TRUE), var = as.name(y_axis)))
                } else {
                    new_data <- new_data %>% group_by_(x_axis) %>% 
                        summarise_(max = interp(~max(var, na.rm=TRUE), var = as.name(y_axis)))
                }
                y_name <- paste("Maximum ", y_axis)
                y_axis <- "max"
            } else if(mutation == "Min"){
                if(facet != "None"){
                    new_data <- new_data %>% group_by_(x_axis, facet) %>% 
                        summarise_(min = interp(~min(var, na.rm=TRUE), var = as.name(y_axis)))
                } else {
                    new_data <- new_data %>% group_by_(x_axis) %>% 
                        summarise_(min = interp(~min(var, na.rm=TRUE), var = as.name(y_axis)))
                }
                y_name <- paste("Mimumum ", y_axis)
                y_axis <- "min"
            } else {
                new_data <- new_data
            }
            
            #If facet is not set to 'None', facet
            if(facet != "None"){
                facet_type <- facet_wrap(as.formula(paste("~", facet)))
            }
            
            #If regression is checked, generate the regression and display coefficients
            #And set generate a geom_smooth to place on top of the data
            if(regression){
                reg_type <- geom_smooth(method = "lm", col = "blue")
            }
            
            #Generate the aes mapping, potentially applying color
            if(is.null(mapping)){
                mapping <- aes_string(x = x_axis, y = y_axis)
                if(!is.null(color) && !(color == "None")){
                    mapping <- aes_string(x = x_axis, y = y_axis, color = color)
                }
            }
            
            #Determine the plot type
            if(is.null(graph_type)){
                if(plot_type == "Scatter"){
                    graph_type <- geom_point()
                } else if(plot_type == "Scatter (Jitter)"){
                    graph_type <- geom_jitter()
                } else if(plot_type == "Line"){
                    graph_type <- geom_line()
                } else if(plot_type == "Bar"){
                    if(mutation == "None"){
                        graph_type <- geom_bar()
                        mapping <- aes_string(x = x_axis)
                    } else {
                        graph_type <- geom_bar(stat = "identity")
                        mapping <- aes_string(x = x_axis, y = y_axis)
                    }
                } else if(plot_type == "Histogram"){
                    graph_type <- geom_histogram()
                    mapping <- aes_string(x = x_axis)
                }
            }
            
            #Set the title
            if(is.null(title_type)){
                title_type <- ggtitle(paste(y_name, " vs. ", x_name))
            }
            
            data$mutatedData <- new_data
            
            #Finally, generate the plot
            ggplot(data = data$mutatedData, mapping = mapping) + 
                graph_type + 
                reg_type +
                coord_type + 
                facet_type +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                title_type
            
        }
    })
})


