library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)
library(readxl)

ui <- page_navbar(
  title = "TRiP App",
  selected = "1. Screening",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  nav_panel(
    title = "1. Screening",
    grid_container(
      layout = c(
        "area0 area1",
        "area0 area1"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        card_header(
          markdown(
            mds = c(
              "The **TRiP App** was created to help small- to medium-sized transit agencies create short-term forecasts to help with their yearly budgets. ",
              "<br>",
              "<br>",
              "Please answer the following questions to see if this app is for your agency. "
            )
          )
        ),
        card_body(
          radioButtons(
            inputId = "qRedesign",
            label = "Has your agency had a system redesign within the last three years?",
            choices = list(
              "Yes" = "Warning: Because your agency has implemented a system redesign within the last three years, historic route ridership data may not accurately reflect current of future service patterns. As a result, this tool may not provide reliable ridership forecasts for your system.",
              "No" = ""
            ),
            width = "100%",
            selected = ""
          ),
          radioButtons(
            inputId = "qUniversity",
            label = "Is a majority of your system ridership from a university or a single employer?",
            choices = list(
              "Yes" = "Warning: Systems with ridership primarily tied to a university or single employer often exhibit unique travel patterns compared to typical community-based bus transit systems. These travel patterns are outside of the assumptions of this tool.",
              "No" = ""
            ),
            width = "100%",
            selected = ""
          ),
          radioButtons(
            inputId = "qRail",
            label = "Does your city have light- or heavy-rail transit? If so, have there been any major rail transit investments in the last three years?",
            choices = list(
              "Yes" = "Warning: This tool is meant for cities and demographics that are served primarily by bus.",
              "No" = ""
            ),
            width = "100%",
            selected = ""
          )
        ),
        card_header(),
        card_body()
      ),
      grid_card(
        area = "area1",
        card_body(
          textOutput(outputId = "textWarn1"),
          textOutput(outputId = "textWarn2"),
          textOutput(outputId = "textWarn3")
        ),
        card_body()
      )
    )
  ),
  nav_panel(
    title = "2. Data Input",
    grid_container(
      layout = c(
        "area1 vrm  ",
        "area1 area2"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "vrm",
        card_header("UPT input example"),
        card_body(tableOutput(outputId = "tableUPT"))
      ),
      grid_card(
        area = "area1",
        card_header("Upload VRM, UPT, and Route data."),
        card_body(
          "Unlinked Passenger Trips excel data (see example to the right)",
          actionButton(inputId = "buttonUPT", label = "UPT Upload"),
          "Vehicle Revenue Miles excel data (see example to the right)",
          "",
          actionButton(inputId = "buttonVRM", label = "VRM Upload"),
          "This can be GTFS data or a different file as long as it is one shapefile containing all of your routes. ",
          actionButton(
            inputId = "buttonRoute",
            label = "Route Upload"
          )
        )
      ),
      grid_card(
        area = "area2",
        card_header("VRM input example"),
        card_body(tableOutput(outputId = "tableVRM"))
      )
    )
  ),
  nav_panel(
    title = "3. Model Creation",
    card(
      full_screen = TRUE,
      card_body(
        grid_container(
          layout = c(
            "area1 area0",
            "area2 area3"
          ),
          row_sizes = c(
            "0.86fr",
            "1.14fr"
          ),
          col_sizes = c(
            "1fr",
            "1fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(
              actionButton(inputId = "myButton", label = "Create Model")
            )
          ),
          grid_card(
            area = "area1",
            card_body(
              markdown(
                mds = c(
                  "### Core Variables",
                  "",
                  "VRM",
                  "",
                  "Monthly Factors",
                  "",
                  "Year Quadratic",
                  "",
                  "Gas Price"
                )
              ),
              "",
              "",
              ""
            )
          ),
          grid_card(
            area = "area2",
            card_body(
              markdown(
                mds = c(
                  "### Candidate Variables",
                  "",
                  "The selected variables will be used in the stepwise regression. You can deselect any of these variables you don't want to include."
                )
              ),
              checkboxGroupInput(
                inputId = "myCheckboxGroup",
                label = "",
                choices = list(
                  "Households No Vehicle Available" = "log_perc_hshlds_noveh",
                  "Workers Below FPL" = "log_below_fpl",
                  "Workers Commuting - Car" = "log_perc_car",
                  "Workers Commuting - Ridehailing" = "log_perc_taxicab",
                  "Workers Commuting - Work from Home" = "log_perc_wfh",
                  "Female Workers" = "log_perc_female",
                  "Workers 100%-150% FPL" = "log_fpl_100_150",
                  "Workers in Renting Households" = "log_perc_renter_occupied",
                  "Labor Participation Rate" = "log_labor_part_rate",
                  "Unemployment Rate" = "log_unemp_rate"
                ),
                selected = c(
                  "log_perc_hshlds_noveh",
                  "log_below_fpl",
                  "log_perc_car",
                  "log_perc_taxicab",
                  "log_perc_wfh",
                  "log_perc_female",
                  "log_fpl_100_150",
                  "log_perc_renter_occupied",
                  "log_labor_part_rate",
                  "log_unemp_rate"
                ),
                width = "100%"
              )
            )
          ),
          grid_card(
            area = "area3",
            card_header("Stepwise Regression Results"),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          )
        )
      )
    )
  ),
  nav_panel(
    title = "4. Forecasting Inputs",
    grid_container(
      layout = c(
        "area0 area1",
        "area0 area1"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        card_header(
          markdown(
            mds = c(
              "Change the values in the table to your own predictions. ",
              "",
              "<br>",
              "",
              "The values represent the percent change in the determinant."
            )
          )
        ),
        card_body(
          DTOutput(outputId = "dtScenarios", width = "100%")
        )
      ),
      grid_card(
        area = "area1",
        card_body(
          actionButton(inputId = "buttonRun", label = "Run Forecasts")
        )
      )
    )
  ),
  nav_panel(
    title = "5. Visualization",
    grid_container(
      layout = c(
        "area0 area1",
        "area0 area1"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "0.3fr",
        "1.7fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        card_body(
          selectInput(
            inputId = "mySelectInput",
            label = "Plot Type",
            choices = list(
              "All Routes" = "a",
              "Route 1" = "b",
              "Route 2" = "value3",
              "Route 3" = "value4"
            )
          )
        )
      ),
      grid_card_plot(area = "area1")
    )
  ),
  nav_panel(
    title = "6. Output",
    grid_container(
      layout = c(
        "area1 area0",
        "area1 area0"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        card_body(
          DTOutput(outputId = "outputExample", width = "100%")
        )
      ),
      grid_card(
        area = "area1",
        card_header("Download Output"),
        card_body(
          "Select the format you want the downloaded output to be in.",
          actionButton(inputId = "outputCSV", label = "CSV"),
          actionButton(inputId = "outputExcel", label = "Excel")
        )
      )
    )
  )
)


server <- function(input, output) {
   
  output$linePlots <- renderPlot({
    obs_to_include <- as.integer(ChickWeight$Chick) <= input$numChicks
    chicks <- ChickWeight[obs_to_include, ]
  
    ggplot(
      chicks,
      aes(
        x = Time,
        y = weight,
        group = Chick
      )
    ) +
      geom_line(alpha = 0.5) +
      ggtitle("Chick weights over time")
  })
  
  output$dists <- renderPlot({
    ggplot(
      ChickWeight,
      aes(x = weight)
    ) +
      facet_wrap(input$distFacet) +
      geom_density(fill = "#fa551b", color = "#ee6331") +
      ggtitle("Distribution of weights by diet")
  })
  
  output$textWarn1 <- renderText({
    if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
      paste("This app might be useful to your agency :)")
    } else{
      paste(input$qRedesign)
    }
  })
  
  output$textWarn2 <- renderText({
    if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
      paste("")
    } else{
      paste(input$qUniversity)
    }
  })
  
  output$textWarn3 <- renderText({
    if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
      paste("")
    } else{
      paste(input$qRail)
    }
  })
  
  output$tableUPT <- renderTable({
    upt <- data.frame(month = rep(1:12,2),
                      year = as.character(c(rep(2024,12),rep(2025,12))),
                      `12` = sample(500:1000, 24),
                      `20` = sample(500:1000, 24),
                      `30` = sample(5000:6000, 24),
                      `31` = sample(8000:10000, 24))
    names(upt) <- c("month","year","12","20","30","31")
    print(upt)
  }, bordered = TRUE)
  
  output$tableVRM <- renderTable({
    vrm <- data.frame(month = rep(1:12,2),
                      year = as.character(c(rep(2024,12),rep(2025,12))),
                      `12` = sample(500:1000, 24),
                      `20` = sample(500:1000, 24),
                      `30` = sample(5000:6000, 24),
                      `31` = sample(8000:10000, 24))
    names(vrm) <- c("month","year","12","20","30","31")
    print(vrm)
  })
  
  
  # 1. Use reactiveValues to store data that changes
  v <- reactiveValues(data = data.frame(Determinants = c("Service Provision",
                                                         "Gas Price"),
                                        Low = c(
                                          "1%",
                                          "-1%"
                                        ),
                                        Medium = c(
                                          "15%",     # e.g. 15% annual VRM growth
                                          "1%"
                                        ),
                                        High = c(
                                          "50%",
                                          "2%"
                                        )
  ))
  
  # 2. Render the table with editable = TRUE
  output$dtScenarios <- renderDT({
    datatable(v$data, editable = 'cell', selection = 'none')
  })
  
  # 3. Use a proxy to update the table without a full re-render
  proxy = dataTableProxy('dtScenarios')
  
  # 4. Observe the 'cell_edit' event
  observeEvent(input$dtScenarios_cell_edit, {
    info = input$dtScenarios_cell_edit
    
    # Extract row and column (Note: DT uses 0-based indexing for columns)
    i = info$row
    j = info$col + 1  # Add 1 because R is 1-indexed
    k = info$value
    
    # 5. Update the reactive data with the new value
    # Use coerceValue to ensure the new value matches the column's data type
    # v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
    
    # (Optional) Update the proxy to keep everything in sync
    replaceData(proxy, v$data, resetPaging = FALSE)
  })
  
  
  output$outputExample <- renderDT({
    data.frame(month = c(rep(1,10), rep(2,10)),
               year = rep(2026, 20),
               route_id = rep(12:21,2),
               upt = sample(5000:7000,20),
               vrm = sample(10000:15000,20),
               other_variables = sample(100:200,20))
  })
  
}

shinyApp(ui, server)
  

