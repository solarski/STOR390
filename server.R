library(shiny)
library(broom)
library(shinyjs)
library(lazyeval)
library(tidyverse)

choices <- list.files(pattern="*.csv")

shinyServer(function(input, output, session) {
    
    #Some shared variables that change
    files = reactiveValues(file1 = NULL)
    data = reactiveValues(data = NULL)
    
    #These two observes detect when the tables have been changed (input$table<1,2>Selector)
    #They take the file, load the data, and update the variable selector
    observe({
        files$file1 <- input$table1Selector
        
        if(!is.na(files$file1)){
            #data$data1 <- read_csv(files$file1)[1:1000,]
            data$data1 <- read_csv(files$file1)
            
            # Can also set the label and select items
            updateSelectInput(session, "xSelector",
                              choices = colnames(data$data1)
            )
        }
    })
    
    observe({
        files$file2 <- input$table2Selector
        
        if(!is.na(files$file2)){
            #data$data2 <- read_csv(files$file2)[1:1000,]
            data$data2 <- read_csv(files$file2)
            
            # Can also set the label and select items
            updateSelectInput(session, "ySelector",
                              choices = colnames(data$data2)
            )
        }
    })
    
    #The main piece of code which generates the plot
    output$mainPlot <- renderPlot({
        #Determine the variables you want to plot, and whether or not to take the mean/regress
        x_var <- isolate(input$xSelector)
        y_var <- isolate(input$ySelector)
        mean <- isolate(input$meanSelector)
        regression <- isolate(input$regressionSelector)
        
        #Run whenever the plot button is clicked
        input$plotButton
        
        #If all of our data is valid
        if(!is.null(x_var) && !(x_var == "") && !is.null(y_var) && !(y_var == "") 
           && !is.null(data$data1) && !is.null(data$data2)){
            
            #If the files match, don't join
            if(files$file1 == files$file2){
                data$data = data$data1
                output$joinText <- renderText({
                    paste("No tables have been joined.")
                })
            } else {
                #If the files are different, print out the joined columns
                #Which is just the intersection of columns
                #And join them
                output$joinText <- renderText({
                    paste("Joined tables on: ", intersect(colnames(data$data1), colnames(data$data2)))
                })
                data$data = inner_join(data$data1, data$data2)
            }
            
            #All of the values we need to plot
            #X and Y variables, the data (maybe transformed), and the ggplot objects
            x_axis = x_var
            y_axis = y_var
            
            new_data = NULL
            reg_type = NULL
            graph_type = NULL
            
            #If mean is checked, group by the x variable and take the mean of the y
            #Also, set the graph to be a line. Otherwise, make it a jitter plot
            if(mean == 1){
                print("Generating average")
                new_data = data$data %>% group_by_(x_var) %>% 
                    summarise_(avg = interp(~mean(var, na.rm=TRUE), var = as.name(y_var)))
                y_axis = "avg"
                graph_type = geom_line()
            } else {
                new_data = data$data
                graph_type = geom_jitter()
            }
            
            #If regression is checked, generate the regression and display coefficients
            #And set generate a geom_smooth to place on top of the data
            if(regression){
                print("Generating regression")
                linreg <- lm(as.formula(paste(y_axis, " ~ ", x_axis)), new_data)
                
                output$regressionTable <- renderDataTable(tidy(linreg))
                
                reg_type = geom_smooth(method = "lm", col = "blue")
            } else {
                output$regressionTable <- renderDataTable(NULL)
            }
            
            #Finally, generate the plot
            ggplot(data = new_data, aes_string(x = x_axis, y = y_axis)) + graph_type + reg_type
            
        }
    })
})


