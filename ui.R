#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# We want a main page with plots, plus tabs that show the data tables
#   and some information about the data sets themselves.
shinyUI(
    navbarPage("FBI 2015 Crime in the US Statistics",
        tabPanel("Welcome",
            fluidPage(
                fluidRow(
                    column(8, offset=2,
                h1("Welcome to the FBI Crime Statistics Plotter!", align="center"),
                br(),
                h2("Overview"),
                p(paste("This tool was created for the Coursera Developing",
                        "Data Products course to visualize a subset of the",
                        "FBI Crime in the US (CIUS) murder statistics. ",
                        "The question the visualizations are intended to answer",
                        "are of the form 'What murder weapons are used in ",
                        "different circumstances?'")),
                br(),
                h2("The Data"),
                p("The", a('FBI CIUS site', href='https://ucr.fbi.gov/crime-in-the-u.s/', 
                           target='_blank'), "is the source of all of the data we use.",
                   paste("We specifically use the Expanded Homicide Data Table 11, Murders",
                         "by Circumstance and Weapon, from 2010 through 2015. ",
                         "'Circumstances' are the context for the murder, such as",
                         "robbery or gangland killings.  The tables break down",
                         "the murders reported to the FBI for each circumstance", 
                         "by the weapon used to commit them.  Since reporting is ",
                         "not homogeneous, many of the murders fall into the 'Other'",
                         "or 'Unknown' categories.")),
                
                br(),
                h2("The 'Plot' Tab"),
                p(paste("This tab allows you to plot up the murders by year, weapon, and",
                         "circumstance in stacked bar chart form.  You have control ",
                        "over which values of each variable are shown and what part ",
                        "of the plot they correspond to.",
                        " The three aspects of the plots you have control over are")),
                tags$ol(
                  tags$li("the way the data is broken into panels,"), 
                  tags$li("the variable along the x-axis, and"), 
                  tags$li("the fill of the bars in the stacked bar chart.")
                ),

                p("The three variables that appear in the dataset are"),
                tags$ol(
                  tags$li("the circumstance, meaning the context in which the murder was committed,"), 
                  tags$li("the murder weapon, and"), 
                  tags$li("the the year.")
                ),
                p("Each of these can be assigned to any of the three aspects of the plot."),
                
                h4("The 'Facet Variable'"),
                p(paste("The Facet Variable is the variable that determines how the ",
                        "data is split into panels.  For instance, if the facet",
                        "variable is 'Circumstances', then you could choose to show",
                        "murders commited during robberies in one panel and those",
                        "committed due to a brawl over narcotics in a second.  If",
                        "the facet variable is 'Weapon', then you could show ",
                        "murders committed with handguns",
                        "in one panel and blunt objects in another.")),
                p(paste("Once you've chosen a value for the facet variable, you can",
                        "choose which values of that variable are shown with the checkbox",
                        "menu just below the facet variable menu.  By default, the",
                        "three values of variable with the highest total murder count",
                        "are chosen (excluding values with the words 'total' or 'other'",
                        "in them).")),
                
                h4("The 'X-Axis' Variable"),
                p(paste("The variable displayed along the x-axis.  Pretty",
                        "self-explanatory. I've found that most often Year is the most",
                        "useful choice for this.  You can choose the values for to show",
                        "on the x-axis in the checkbox menu below the x-axis variable",
                        "selection menu.")),
                
                h4("The Fill Variable"),
                p(paste("Once you have chosen the variables for the facets and x-axis,",
                        "the third variable is assigned to the fill.  You can choose",
                        "what values of the fill variable in the checkbox menu below",
                        "'Fill by [fill variable]'.  By default the top three values",
                        "are chosen, again excluding values containing 'total' or 'other.'")),
                
                br(),
                h2("The 'Data tables' Tab"),
                p(paste("Clicking on this tab will give you the option to view the",
                        "Expanded Homicide Data Table 11 for any year from 2010-2015.",
                        "Each table is as it appears in the raw data, with the exception",
                        "of text formatting of the row and column names.  Because the",
                        "table is wide, I have included the option to toggle whether or",
                        "not each column is displayed.  If you click the 'Column Visibility'",
                        "button, a list of columns will appear, by default with all columns",
                        "toggled on.  Clicking the column name toggles that column between",
                        "shown and hidden."))
                    )
                )
            )
        ),
               
        tabPanel("Plot",
            sidebarLayout(
                sidebarPanel(
                    #Set up the controls for all of the aspects of the plot
                    
                    #Start with the faceting properties- both the variable and the
                    #values of that variable to use
                    uiOutput("facetMenu"),
                    h4("Facet values"),
                    wellPanel(
                        style = "overflow-y:scroll; max-height: 200px",
                        uiOutput("facetValueMenu")
                    ),
                    
                    #Now figure out the x axis, variable and values
                    uiOutput("xAxisMenu"),
                    h4("Values to show on X-axis"),
                    wellPanel(
                        style = "overflow-y:scroll; max-height: 200px",
                        uiOutput("xAxisValueMenu")
                    ),
                    
                    #With the other two determined, the third variable is the fill.
                    #Get the values to show
                    h3(textOutput("fillNameText")),
                    wellPanel(
                        style = "overflow-y:scroll; max-height: 200px",
                        uiOutput("fillValueMenu")
                    )

                ),
                
                # This is the main panel, which holds the plot.
                mainPanel(
                    #textOutput("holder")
                    plotOutput("statPlot")
                )
            )
        ),
        
        #The second panel will be the raw data that we're looking at
        navbarMenu("Data tables",
            tabPanel("2010 data",
                 h2("FBI CIUS data 2010"),
                 h3("Expanded Homicide Data Table 11, Murder by Circumstance and Weapon"),
                 # selectInput("dataTableYear", options=),
                 DT::dataTableOutput('dataTable10')
            ),
            tabPanel("2011 data",
                     h2("FBI CIUS data 2011"),
                     h3("Expanded Homicide Data Table 11, Murder by Circumstance and Weapon"),
                     # selectInput("dataTableYear", options=),
                     DT::dataTableOutput('dataTable11')
            ),
            tabPanel("2012 data",
                     h2("FBI CIUS data 2012"),
                     h3("Extended Homicide Table 11, Murder by Circumstance and Weapon"),
                     # selectInput("dataTableYear", options=),
                     DT::dataTableOutput('dataTable12')
            ),
            tabPanel("2013 data",
                     h2("FBI CIUS data 2013"),
                     h3("Expanded Homicide Data Table 11, Murder by Circumstance and Weapon"),
                     # selectInput("dataTableYear", options=),
                     DT::dataTableOutput('dataTable13')
            ),
            tabPanel("2014 data",
                     h2("FBI CIUS data 2014"),
                     h3("Expanded Homicide Data Table 11, Murder by Circumstance and Weapon"),
                     # selectInput("dataTableYear", options=),
                     DT::dataTableOutput('dataTable14')
            ),
            tabPanel("2015 data",
                     h2("FBI CIUS data 2015"),
                     h3("Expanded Homicide Data Table 11, Murder by Circumstance and Weapon"),
                     # selectInput("dataTableYear", options=),
                     DT::dataTableOutput('dataTable15')
            )
        )
        
))
