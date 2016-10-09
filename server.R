#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#Eventually
#----------
#Make options persistent when other things change!!
#Have scaling choices make sense
#Figure out why "Other" is floating
#Ability to create groups (so you can display one color for strangulation OR asphyxiation?
#"Start now!" button on front page
#Toggle between number and percent

library(shiny)
library(plotly)
library(lubridate)
library(foreign)
library(data.table)
library(tidyr)
library(ggplot2)
library(dplyr)
library(gdata)
library(RColorBrewer)
library(DT)
theme_set(theme_gray(base_size = 16))

source("readCIUS.R")
source("plots.R")
source("useful_things.R")
#Load in all the FBI data
murder_circ_weap <- make_murder_circumstance_weapon()
# murder_by_weapon <- make_murder_by_weapon()
# justifiable_by_weapon <- make_justifiable_by_weapon()
# robbery_by_weapon <- make_robbery_by_weapon()
# homicide_by_weapon <- make_homicide_by_weapon()
# agg_assault_by_weapon <- make_agg_assault_by_weapon()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #=============================================
    #  Prep work
    #=============================================
    
    #Choose our data set- for now, just Murders by circumstance and weapon
    dataset <- murder_circ_weap[[1]]
    human_dataset <- murder_circ_weap[[2]]
    
    #Now figure out what the variables other than the statistic are called.
    #The only non-factor variable is the statistic, so screen that out.
    main_options <- names(dataset)
    main_options <- main_options[sapply(dataset, class)=="factor"]
    names(main_options) <- main_options
    
    #=============================================
    #  Reactive menus
    #=============================================
    
    #--------------
    # Defaults
    
    n_main_options <- sapply(main_options, function(x) length(levels(dataset[,x])))
    # print(n_main_options[["Year"]])
    default_facetVar <- names(main_options[which.max(n_main_options)])
    default_xAxisVar <- reactive({
        options_left <- main_options[!(main_options == input$facetName)]
        n_options_left <- n_main_options[!(main_options == input$facetName)]
        names(options_left[which.min(n_options_left)])
    })
        
    #Menu for which variable is used to facet on
    output$facetMenu <- renderUI({
        selectInput("facetName", label=h3("Variable for Facets"),
                    choices = main_options,
                    selected = default_facetVar
                    )
    })
    
    #Menu to choose which of the values to put as facets
    output$facetValueMenu <- renderUI({
        #Require the facetName
        req(input$facetName)
        
        #Define the seletion options
        selection_options <- levels(dataset[,input$facetName])
        
        #Choose the default facets as the top 3 max values in their row/col (non-total)
        if (length(selection_options)>3){
            expr <- paste0("Number ~ ", input$facetName)
            expr <- parse(text = expr)
            temp <- aggregate(eval(expr), data=dataset, FUN=max)
            
            #We'd rather show "pure" facets, so screen out ones with "other" or "total"
            mask <- !grepl("[T|t]otal", temp[,input$facetName])
            mask <- mask & !grepl("[O|o]ther", temp[,input$facetName])
            temp <- temp[mask,]

            #Grab out the max number for this category and the key value for readability
            num <- temp$Number
            key <- as.character(temp[,input$facetName])
            
            #Figure out which keys correspond to the 3 highest values
            sorted_vals <- sort(num, decreasing=TRUE)
            cutoff <- sorted_vals[3]
            default_options <- key[num>=cutoff]
        } else{
            default_options <- selection_options
        }
            
        checkboxGroupInput("facetValues", paste(input$facetName, "options:"),
                           selection_options, selected = default_options
                           )
    })
    
    #Menu to choose x axis variable
    output$xAxisMenu <- renderUI({
        #Require the inputs that we need for this
        #req(input$facetName)
        selectInput("xAxisName", label=h3("Variable on X-axis"),
                    choices = main_options[main_options!=input$facetName],
                    selected = default_xAxisVar()
                    )
    })
    
    #Menu to choose the values of the x axis variable to show
    output$xAxisValueMenu <- renderUI({
        #State requirements
        req(input$facetName, input$facetValues, input$xAxisName)
        #Define the seletion options
        selection_options <- levels(dataset[,input$xAxisName])
        
        #Choose the default facets as the top 3 max values in their row/col (non-total)
        if (length(selection_options) > 3){
            subset <- dataset[dataset[,input$facetName] %in% input$facetValues,]
            expr <- paste0("Number ~ ", input$xAxisName)
            expr <- parse(text = expr)
            temp <- aggregate(eval(expr), data=subset, FUN=max)
            #We'd rather show "pure" facets, so screen out ones with "other" or "total"
            mask <- !grepl("[T|t]otal", temp[,input$xAxisName])
            mask <- mask & !grepl("[O|o]ther", temp[,input$xAxisName])
            temp <- temp[mask,]
            #Grab out the max number for this category and the key value for readability
            num <- temp$Number
            key <- as.character(temp[,input$xAxisName])
            #Figure out which keys correspond to the 3 highest values
            sorted_vals <- sort(num, decreasing=TRUE)
            cutoff <- sorted_vals[3]
            default_options <- key[num>=cutoff]
        } else {
            default_options <- selection_options
        }
        
        checkboxGroupInput("xAxisValues", paste(input$xAxisName, "options:"),
                           selection_options,
                           selected = default_options
        )
    })
    
    #Set the fill variable to be the last option
    fillName <- reactive({
            #Requirements
            req(input$facetName, input$xAxisName)
        
            taken_opts <- c(input$facetName, input$xAxisName)
            taken_mask <- main_options %in% taken_opts
            main_options[!taken_mask]
        })
    
    #Make a label to say what the fill variable is
    output$fillNameText <- renderText({
        paste("Fill by", fillName())
    })
    
    #And finally make a menu to choose which of the fills to show
    output$fillValueMenu <- renderUI({
        #Requirements
        req(input$facetName, input$facetValues, input$xAxisName, input$xAxisValues)
        
        these_options <- levels(dataset[,fillName()])
        viable_mask <- !grepl("[T|t]otal", these_options)
        selection_options <- these_options[viable_mask]
        
        #Figure out the default options
        if (length(selection_options) > 3){
            #Only grab the rows that we're looking at earlier
            subset <- dataset[dataset[,input$facetName] %in% input$facetValues,]
            subset <- subset[subset[,input$xAxisName] %in% input$xAxisValues,]
            
            #pull out the max value for each fillName row
            expr <- paste0("Number ~ ", fillName())
            expr <- parse(text = expr)
            temp <- aggregate(eval(expr), data=subset, FUN=max)
            
            #We'd rather show "pure" facets, so screen out ones with "other" or "total"
            mask <- !grepl("[T|t]otal", temp[,fillName()])
            mask <- mask & !grepl("[O|o]ther", temp[,fillName()])
            temp <- temp[mask,]
            
            #Grab out the max number for this category and the key value for readability
            num <- temp$Number
            key <- as.character(temp[,fillName()])
            
            #Figure out which keys correspond to the 3 highest values
            sorted_vals <- sort(num, decreasing=TRUE)
            cutoff <- sorted_vals[3]
            default_options <- key[num>=cutoff]
        } else {
            default_options <- selection_options
        }
        
        checkboxGroupInput("fillValues", paste("Include", fillName(), "options:"),
                           selection_options, selected = default_options
        )
    })
     
    #=============================================
    #  Creating the plot
    #=============================================

    # output$holder <- renderText(default_xAxisVar())
    
    output$statPlot <- renderPlot({
        validate(need(input$facetValues, message="You must choose at least one value of the facet variable"))
        validate(need(input$xAxisValues, message="You must choose at least one value for the x axis variable"))
        req(input$facetName, input$xAxisName)
        
        dat <- prep_data(dataset, input$facetName, input$facetValues, 
                         input$xAxisName, input$xAxisValues, fillName(), input$fillValues)
        stacked_bar(dat, input$xAxisName, fillName())
        }, height = 500,
        #height= exprToFunction(700+17*length(input$fillValues)), 
        width = exprToFunction(min(700, 300 + 100*length(input$facetValues)))
        )

    
    #=============================================
    #  Data tables
    #=============================================
    
    #Show the datatable
    output$dataTable10 <- DT::renderDataTable(
        DT::datatable(human_dataset[[1]], extensions = c('Buttons', 'FixedColumns', "FixedHeader"), 
                      options = list(pageLength = 25, dom = 'Bfrtip', 
                                     buttons = I('colvis'),
                                     scrollX=TRUE,
                                     fixedHeader=TRUE,
                                     fixedColumns = list(leftColumns=2, rightColumns=0))
                      )
    )
    output$dataTable11 <- DT::renderDataTable(
        DT::datatable(human_dataset[[2]], extensions = c('Buttons', 'FixedColumns', "FixedHeader"),
                      options = list(pageLength = 25, dom = 'Bfrtip',
                                     buttons = I('colvis'),
                                     scrollX=TRUE,
                                     fixedHeader=TRUE,
                                     fixedColumns = list(leftColumns=2, rightColumns=0))
        )
    )
    output$dataTable12 <- DT::renderDataTable(
        DT::datatable(human_dataset[[3]], extensions = c('Buttons', 'FixedColumns', "FixedHeader"),
                      options = list(pageLength = 25, dom = 'Bfrtip',
                                     buttons = I('colvis'),
                                     scrollX=TRUE,
                                     fixedHeader=TRUE,
                                     fixedColumns = list(leftColumns=2, rightColumns=0))
        )
    )
    output$dataTable13 <- DT::renderDataTable(
        DT::datatable(human_dataset[[4]], extensions = c('Buttons', 'FixedColumns', "FixedHeader"),
                      options = list(pageLength = 25, dom = 'Bfrtip',
                                     buttons = I('colvis'),
                                     scrollX=TRUE,
                                     fixedHeader=TRUE,
                                     fixedColumns = list(leftColumns=2, rightColumns=0))
        )
    )
    output$dataTable14 <- DT::renderDataTable(
        DT::datatable(human_dataset[[5]], extensions = c('Buttons', 'FixedColumns', "FixedHeader"),
                      options = list(pageLength = 25, dom = 'Bfrtip',
                                     buttons = I('colvis'),
                                     scrollX=TRUE,
                                     fixedHeader=TRUE,
                                     fixedColumns = list(leftColumns=2, rightColumns=0))
        )
    )
    output$dataTable15 <- DT::renderDataTable(
        DT::datatable(human_dataset[[6]], extensions = c('Buttons', 'FixedColumns', "FixedHeader"),
                      options = list(pageLength = 25, dom = 'Bfrtip',
                                     buttons = I('colvis'),
                                     scrollX=TRUE,
                                     fixedHeader=TRUE,
                                     fixedColumns = list(leftColumns=2, rightColumns=0))
        )
    )
})
