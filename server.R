library(shiny)
library(broom)
library(shinyjs)
library(lazyeval)
library(tidyverse)

choices <- list.files(pattern="*.csv")

shinyServer(function(input, output, session) {
    
    files = reactiveValues(file1 = NULL)
    data = reactiveValues(data = NULL)
    
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
    
    output$mainPlot <- renderPlot({
        x_var <- isolate(input$xSelector)
        y_var <- isolate(input$ySelector)
        mean <- isolate(input$meanSelector)
        regression <- isolate(input$regressionSelector)
        
        input$plotButton
        
        if(!is.null(x_var) && !(x_var == "") && !is.null(y_var) && !(y_var == "") 
           && !is.null(data$data1) && !is.null(data$data2)){
            
            if(files$file1 == files$file2){
                data$data = data$data1
            } else {
                joined_cols <- intersect(colnames(data$data1), colnames(data$data2))
                print(joined_cols)
                output$joinText <- renderText({
                    paste("Joined tables on: ", joined_cols)
                })
                data$data = inner_join(data$data1, data$data2)
            }
            
            x_axis = x_var
            y_axis = y_var
            
            new_data = NULL
            reg_type = NULL
            graph_type = NULL
            
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
            
            if(regression){
                print("Generating regression")
                linreg <- lm(as.formula(paste(y_axis, " ~ ", x_axis)), new_data)
                
                output$regressionTable <- renderDataTable(tidy(linreg))
                
                reg_type = geom_smooth(method = "lm", col = "blue")
            } else {
                output$regressionTable <- renderDataTable(NULL)
            }
            
            ggplot(data = new_data, aes_string(x = x_axis, y = y_axis)) + graph_type + reg_type
            
        }
    })
})


