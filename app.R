# Load packages ----
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)

filepath <- "data/cleanedDataCPI_1972-2022.csv"
cpi_df <- read.csv(filepath)
cpi_years <- c(lubridate::ymd(cpi_df$years, truncated=2L))


# User interface ----
ui <- fluidPage(theme=shinytheme("superhero"),
        navbarPage("Inflation calculator",
           tabPanel("Home",
              sidebarPanel(
                HTML("<h3>Input parameters </h3>"),
                #historical has suffix of 1, current has suffix of 2
                sliderInput("year1",
                            "Their year",
                            value = ymd("1965", truncated=2L),
                            min = min(cpi_years),
                            max = max(cpi_years),
                            step= 365.25
                            ),

                sliderInput("year2",
                            "Your year:",
                            value = ymd("2022", truncated=2L),
                            min = min(cpi_years),
                            max = max(cpi_years),
                            step= 365.25
                            ),
                numericInput("amount1",
                             "What they paid ($):",
                             value = 1000
                              ),
                textInput("label1",
                          "Expense name: ",
                          value = "Rent"
                           ),
                actionButton("submitbutton",
                             "Submit",
                             class="btn btn-primary")
              ), #sidebarPanel
              mainPanel(

                  h3("Calculator"),
                  p("The following tool is to translate your parents earnings to current day dollars."),
                  br(),
                  p("Calculations are done using Consumer Performance Index values (see about tab for more)."),
                  br(),
                  h4(textOutput("cpi_out")),
                  h3(textOutput("amount_out")),
                  br(),
                  h3(textOutput("inflation_out")),
                  br(),
                  markdown("*Statistics Canada. Table 18-10-0005-01  Consumer Price Index, annual average, not seasonally adjusted.*   ([link](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810000501&pickMembers%5B0%5D=1.2&cubeTimeFrame.startYear=1952&cubeTimeFrame.endYear=2022&referencePeriods=19520101%2C20220101))")
              ) #mainPanel
            ), #tabPanle, home
           tabPanel("About",
                    titlePanel("About"),
                    div(includeMarkdown("about.md"),
                        align="justify")
           ) #tabPanel(), about
         ) #navbarPage

)

# Server logic
server <- function(input, output, session) {


      observeEvent(input$submitbutton, {
        #get year component and covert back to ymd() to get in YYYY-01-01 format
        year1 <- year(input$year1)
        year1 <- ymd(year1, truncated=2L)
        year2 <- year(input$year2)
        year2 <- ymd(year2, truncated=2L)
        #get cpi from cpi_df() using year values
        year1_index <- which(cpi_years == year1)
        year2_index <- which(cpi_years == year2)
        year1_cpi <- cpi_df$CPI[year1_index]
        year2_cpi <- cpi_df$CPI[year2_index]

        #write output variables
        cpi_text <- paste("The CPI in", input$year1, "is", year1_cpi, ". The CPI in", input$year2, "is", year2_cpi)
        amount2 <- round(((year2_cpi / year1_cpi) * input$amount1), 2)
        amount_text <- paste("Paying $", input$amount1, "(CAD) for", input$label1, "in", year(input$year1), "equates to a value of $", amount2, " (CAD) in", year(input$year2), ".")
        cum_inflation <- ((year2_cpi - year1_cpi) / year1_cpi) * 100
        inflation_text <- paste("Cumulative inflation between",year(input$year1), "and",year(input$year2), "was", cum_inflation, "%")
        output$cpi_out <- renderText({cpi_text})
        output$amount_out <- renderText({amount_text})
        output$inflation_out <- renderText({inflation_text})
      })
}

# Run the app
shinyApp(ui, server)
