# Load packages ----
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)

filepath <- "data/cleanedDataCPI_1972-2022.csv"
cpi_df <- read.csv(filepath)
cpi_years <- c(lubridate::ymd(cpi_df$years, truncated=2L))


# User interface ----
ui <- fluidPage(theme=shinytheme("united"),
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
                             2000
                              ),
                textInput("label1",
                          "Expense name: ",
                          "Rent"
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
                  markdown("*Statistics Canada. Table 18-10-0005-01  Consumer Price Index, annual average, not seasonally adjusted.*   ([link](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810000501&pickMembers%5B0%5D=1.2&cubeTimeFrame.startYear=1952&cubeTimeFrame.endYear=2022&referencePeriods=19520101%2C20220101))")
              ) #mainPanel
            ) #tabPanle, home
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
        output$cpi_out <- renderText({cpi_text})
        output$amount_out <- renderText({amount_text})
      })
      # #reactively get cpi using input from year1, year2
      # year1_cpi <- reactive({
      #   year1 <- year(input$year1)
      #   year1 <- ymd(year1, truncated=2L)
      #   year1_index <- which(cpi_years == year1)
      #   year1_cpi <- cpi_df$CPI[year1_index]
      #   })
      #
      # year2_cpi <- reactive({
      #   year2 <- year(input$year2)
      #   year2 <- ymd(year2, truncated=2L)
      #   year2_index <- which(cpi_years == year2)
      #   year2_cpi <- cpi_df$CPI[year2_index]
      # })
      #
      # #process year data - calculate and generate text
      # cpi_text <- reactive({
      #   if (input$submitbutton > 0) {
      #     paste("The CPI in", input$year1, "is", year1_cpi(),". The CPI in",input$year2,"is", year2_cpi())
      #   }
      # })
      #
      # amount_text <- reactive({
      #   if (input$submitbutton > 0) {
      #     amount2 <- (year2_cpi() / year1_cpi()) * input$amount1
      #     paste("Paying", input$amount1, "for", input$label1, "in", input$year1, "equates to", amount2, "in", input$year2, ".")
      #   }
      # })
      #
      #
      # #output
      # output$cpi_out <- renderText({
      #       if (input$submitbutton > 0) {
      #         # isolate(year1_cpi())
      #         print(cpi_text())
      #       }
      #   })
      #
      # output$amount_out <- renderText({
      #   if (input$submitbutton > 0) {
      #     print(amount_text())
      #   }
      # })

}

# Run the app
shinyApp(ui, server)
