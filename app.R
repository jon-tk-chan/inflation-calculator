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
                sliderInput("year1",
                            "Current year:",
                            value = ymd("2022", truncated=2L),
                            min = min(cpi_years),
                            max = max(cpi_years),
                            step= 365.25
                            ),

                sliderInput("year2",
                            "Their year:",
                            value = ymd("1965", truncated=2L),
                            min = min(cpi_years),
                            max = max(cpi_years),
                            step= 365.25
                            ),
                numericInput("paid_amount",
                             "What they paid ($):",
                             2000
                              ),
                textInput("paid_label",
                          "Expense name: ",
                          "Rent"
                           ),
                actionButton("submitbutton",
                             "Submit",
                             class="btn btn-primary")
              ), #sidebarPanel
              mainPanel(

                  h1("Header 1"),
                  p("The following tool is to translate your parents earnings to current day dollars.

                    Calculations are done using Consumer Performance Index values (see about tab for more)."),
                  verbatimTextOutput("cpi_out"),
                  verbatimTextOutput("amount_out")
              ) #mainPanel
            ) #tabPanle, home
         ) #navbarPage

)

# Server logic
server <- function(input, output, session) {
      #reactively get cpi using input from year1, year2
      year1_cpi <- reactive({
        year1 <- year(input$year1)
        year1 <- ymd(year1, truncated=2L)
        year1_index <- which(cpi_years == year1)
        year1_cpi <- cpi_df$CPI[year1_index]
        })

      year2_cpi <- reactive({
        year2 <- year(input$year2)
        year2 <- ymd(year2, truncated=2L)
        year2_index <- which(cpi_years == year2)
        year2_cpi <- cpi_df$CPI[year2_index]
      })

      #process year data - calculate and generate text
      cpi_text <- reactive({
        if (input$submitbutton > 0) {
          paste("The CPI in", input$year1, "is", year1_cpi(),". The CPI in",input$year2,"is", year2_cpi())
        }
      })

      amount_text <- reactive({
        if (input$submitbutton > 0) {
          new_amount <- (year1_cpi() / year2_cpi()) * input$paid_amount
          paste("Paying", input$paid_amount, "for", input$paid_label, "in", input$year1, "equates to", new_amount, "in", input$year2, ".")
        }
      })


      #output
      output$cpi_out <- renderText({
            if (input$submitbutton > 0) {
              # isolate(year1_cpi())
              print(cpi_text())
            }
        })

      output$amount_out <- renderText({
        if (input$submitbutton > 0) {
          # isolate(year1_cpi())
          print(amount_text())
        }
      })


      # output_texts <- reactive({
      #   year1 <- year(input$year1)
      #   year1 <- ymd(year1, truncated=2L)
      #   year1_index <- which(cpi_years == year1)
      #   year1_cpi <- cpi_df$CPI[year1_index]
      #
      #   year2 <- year(input$year2)
      #   year2 <- ymd(year2, truncated=2L)
      #   year2_index <- which(cpi_years == year2)
      #   year2_cpi <- cpi_df$CPI[year2_index]
      #
      #   new_amount <- (year1_cpi / year2_cpi) * input$paid_amount
      #
      #   eq_amount <- (year2_cpi / year1_cpi) * input$paid_amount
      #   paste("CPI 1: ",year1_cpi, "CPI 2", year2_cpi, "NEW AMOUNT", new_amount)
      #   # paste("Them paying", input$paid_amount,"for",input$paid_label, "in", year2, "amounts to", new_amount, "in", year1)
      #    paste("EQ AMOUNT: ", eq_amount)
      #
      # })
      # output$out_amounts <- renderText({
      #     if (input$submitbutton > 0) {
      #       isolate(output_texts())
      #     }
      # })

}

# Run the app
shinyApp(ui, server)
