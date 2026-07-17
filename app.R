library(shiny)
library(bslib)
library(gridlayout)
library(DT)

options(shiny.maxRequestSize = 30 * 1024^2, # 30 MB
        shiny.reactlog = TRUE)



#### UI ####
ui <- page_navbar(
  id = "main_nav",
  title = "TRiP App",
  selected = "pan_1",
  theme = bslib::bs_theme(),
##### 1. screening #####
  nav_panel(
    value = "pan_1",
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
              "Yes" = "Warning: Because your agency has implemented a system redesign within the last three years, historic route ridership data may not accurately reflect current or future service patterns. As a result, this tool may not provide reliable ridership forecasts for your system.",
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
          ),
          actionButton("compatibility_button", "Check Compatibility", class = "btn-primary")
        ),
        card_header(),
        card_body()
      ),
      grid_card(
        area = "area1",
        card_body(
          textOutput(outputId = "textWarn1"),
          textOutput(outputId = "api_test"),
          uiOutput(outputId = "screening_button_placeholder")
        )
      )
    )
  ),
##### 2. rider data #####
  nav_panel(
    value = "pan_2",
    title = "2. Ridership Data Upload",
    grid_container(
      layout = c(
        "area1 vrm",
        "area1 vrm"
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
        card_body(
          conditionalPanel(
                    condition = "input.has_fare_changes === 'yes'",
                    h5("Inputing Changes to Adult Base Fare"),
                    div(
                      style = "display:flex; gap:.5rem; flex-wrap:wrap;",
                      dateInput("new_date", "Date of change", width = "150px",
                                value = Sys.Date(),
                                min = Sys.Date() - 10000,
                                max = Sys.Date()),
                      numericInput("new_prev", "Previous fare", value = NA, min = 0, step = 0.25, width = "150px"),
                      numericInput("new_new", "New fare", value = NA, min = 0, step = 0.25, width = "150px")
                    ),
                    div(
                      style = "display:flex; gap:.5rem; flex-wrap:wrap;",
                      actionButton("add", "Submit fare increase", class = "btn-primary"),
                      actionButton("delete", "Delete selected fare row", class = "btn-danger")
                    )
                    ),
          conditionalPanel(
            condition = "input.has_fare_changes === 'yes'",
            DTOutput("tbl")
          )
        )
      ),
      grid_card(
        area = "area1",
        card_body(
          "Upload a properly formatted excel file with monthly Unlinked Passenger Trips (UPT) and Vehicle Revenue Miles (VRM) for each route.",
          "(seen example below for data format) \n",
          "Due to unusual ridership patterns during the COVID19 pandemic, it is recommended that your data not go back past the year 2023.",
          fileInput("upload_data", "",
                    accept = ".xlsx",
                    width = "100%"),
          hr(),
          h5("Input Data Example"),
          "The uploaded data must be an excel file formatted in this way. \n",
          "Note that route_id must match the route id in the GTFS files",
          tableOutput(outputId = "example_input"),
          hr(),
          "Just a few more questions before creating your model for forecasting.",
          radioButtons(
            inputId = "has_fare_changes",
            label = "Have you changed your adult base fare in the past 5 years?",
            choices = c("No" = "no", "Yes" = "yes"),
            selected = "no",
            inline = TRUE
          )
        )
      )
    )
  ),
##### 3. gtfs #####
  nav_panel(
    value = "pan_3",
    title = "3. GTFS Upload",
    grid_container(
      layout = c(
        "area1 plot1  ",
        "area1 plot1"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "0.5fr",
        "1.5fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "plot1",
        card_body(leafletOutput("route_map"))
      ),
      grid_card(
        area = "area1",
        card_header("Route Data Upload"),
        card_body(
          "Upload your agency's GTFS zip file",
          fileInput("upload_routes","GTFS Upload", accept = ".zip"),
          textOutput(outputId = "acs_description"),
          uiOutput(outputId = "acs_button_placeholder")
        )
      )
    )
  ),
##### 4. Create Model #####
  nav_panel(
    value = "pan_4",
    title = "4. Model Creation",
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
            selectizeInput(inputId = "variables_forced",
                           label = "Adding Variables (optional)",
                           choices = NULL,
                           selected = NULL,
                           multiple = TRUE,
                           width = "100%"
            ),
            input_task_button("run_model_forced", "Create Model with Forced Variables")
          ),
          grid_card(
            area = "area1",
            card_body(
              selectizeInput(
                inputId = "variables",
                label = "Variable Selection",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                width = "100%"
              ),

              input_task_button("run_model_stepwise", "Redo Stepwise Regression")
            )
          ),
          grid_card(
            area = "area2",
            card_body(
              input_task_button("use_this_model_button", "Continue With This Model"),
              DTOutput(outputId = "tbl_mod_stepwise")
            )
          ),
          grid_card(
            area = "area3",
            card_body(
              input_task_button("use_this_model_button_forced", "Continue With This Model"),
              DTOutput(outputId = "tbl_mod_forced")
            )
          )
        )
      )
    )
  ),
##### 5. Review Model #####
  nav_panel(
    value = "pan_5",
    title = "5. Model Review",
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
        card_body(
          "Let's review the model you have selected. ",
          gt_output("coefficients_review")
        ),
        card_body()
      ),
      grid_card(
        area = "area1",
        card_body(
          "Research has cited the following elasticities for select coefficients. If you would prefer to replace or add these coefficients to your model, you can check the coefficient. ",
          checkboxGroupInput(
            inputId = "forced_coef_checkbox",
            label = "Common Coefficient Values",
            choices = list(
              "VRM (0.45)" = "vrm",
              "Gas Price (0.15)" = "gas",
              "Fares (-0.3) ** only add if you plan to increase fares" = "fares",
              "BRT (-0.3) ** only add if you have current BRT routes or if you plan to convert a route to BRT" = "brt"
            )
          ),
          uiOutput(outputId = "brt_question_placeholder"),
          conditionalPanel(
            condition = "input.brt_question === 'yes'",
            h5("Inputing BRT Changes"),
            "Please select the route and the approximamte date that it will be converted to BRT",
            div(
              style = "display:flex; gap:.5rem; flex-wrap:wrap;",
              uiOutput(outputId = "brt_date"),
              uiOutput(outputId = "brt_routes"),
              div(
                style = "display:flex; gap:.5rem; flex-wrap:wrap;",
                actionButton("add_brt", "Submit BRT change", class = "btn-primary"),
                actionButton("delete_brt", "Delete selected BRT row", class = "btn-danger")
              )
            )
          ),
          conditionalPanel(
            condition = "input.brt_question === 'yes'",
            DTOutput("tbl_brt")
          ),
          "If you have reviewed the coefficients, added any forced coefficients you wanted, and decided this is the model you want to use, please click the button below.",
          actionButton(
            inputId = "proceed_to_forecast",
            label = "Proceed to Forecasting",
            class = "btn-primary"
          )
        ),
        card_body(),
        card_body()
      )
    )
  ),
##### 6. Forecasting #####
  nav_panel(
    value = "pan_6",
    title = "6. Forecasting Inputs",
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
        card_body(
          "The table below has been populated with the variables that you must predict to run the forecasts. Double click in each cell to replace it with your prediction.",
          # DTOutput(outputId = "dtScenarios", width = "100%"),
          radioButtons(
            "route_mode",
            "Apply scenario to:",
            choices = c("New routes (not yet saved)" = "new",
                        "Overwrite existing routes" = "overwrite"),
            selected = "new",
            inline = TRUE
          ),

          selectInput("route_selected", "Route", choices = NULL),

          actionButton("save_route_scenario", "Save", class = "btn-primary"),

          DTOutput("dtScenarios")
        )
      ),
      grid_card(
        area = "area1",
        card_body(
          input_task_button("buttonRun","Run Forecasts"),
          # plotOutput(outputId = "forcast_plot"),
          # Optional: show saved results
          DTOutput("dtSavedScenarios")
        )
      )
    )
  ),
##### 7. Visualize #####
  nav_panel(
    value = "pan_7",
    title = "7. Visualization",
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
            inputId = "input_route_to_plot",
            label = "Choose a route to plot",
            choices = "Waiting for forecast..."
          )
        )
      ),
      grid_card(area = "area1",
                plotOutput("viz_plot") #,
                # plotOutput("viz_plot_2")
    )
  )
  ),
##### 8. Export #####
  nav_panel(
    value = "pan_8",
    title = "8. Export",
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
          downloadButton("download_csv", "Download as CSV"),
          downloadButton("download_xlsx", "Download as Excel")
        )
      )
    )
  )
)

#### SERVER ####
server <- function(input, output, session) {

#### 1. SCREENING ####

  output$api_test <- renderText({
    key <- Sys.getenv("CENSUS_API_KEY")
    paste0("Your API key is [", key, "]")
  })

  observe({

    if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
      text_output <- paste("This app might be useful to your agency :)")
    } else if (input$qRedesign == "" & input$qUniversity == ""){
      text_output <- paste("Because this app was created to work with bus networks,
            this app may not meet your needs.
            If there have been no recent investments in rail transit,
            you might be able to use this app if you exclude non-bus routes.")
    } else if (input$qRedesign == "" & input$qRail == ""){
      text_output <- paste("Since most of your system ridership comes from a university or a single employer,
            the variables used in this app may not be the largest predictors of ridership,
            and, therefore, the app's predictions may not be accurate.
            Note that if you have your own data to capture these unique circumstances,
            there is an option to add those to the model.")
    } else if (input$qRail == "" & input$qUniversity == ""){
      text_output <- paste("Because your agency has implemented a system redesign within the last three years,
            historic route ridership data may not accurately reflect current or future service patterns.
            As a result, this tool may not provide reliable ridership forecasts for your system.")
    } else if (input$qRail == ""){
      text_output <- paste("Because your agency has implemented a system redesign within the last three years,
            historic route ridership data may not accurately reflect current or future service patterns.
            As a result, this tool may not provide reliable ridership forecasts for your system.
            Additionally, since most of your system ridership comes from a university or a single employer,
            the variables used in this app may not be the largest predictors of ridership,
            and, therefore, the app's predictions may not be accurate.
            Note that if you have your own data to capture these unique circumstances,
            there is an option to add those to the model.")
    } else if (input$qRedesign == ""){
      text_output <- paste("Since most of your system ridership comes from a university or a single employer,
            the variables used in this app may not be the largest predictors of ridership,
            and, therefore, the app's predictions may not be accurate.
            Note that if you have your own data to capture these unique circumstances,
            there is an option to add those to the model.
            Additionally, because this app was created to work with bus networks,
            this app may not meet your needs.
            If there have been no recent investments in rail transit,
            you might be able to use this app if you exclude non-bus routes.")
    } else if (input$qUniversity == ""){
      text_output <- paste("Because your agency has implemented a system redesign within the last three years,
            historic route ridership data may not accurately reflect current or future service patterns.
            As a result, this tool may not provide reliable ridership forecasts for your system.
            Additionally, because this app was created to work with bus networks,
            this app may not meet your needs.
            If there have been no recent investments in rail transit,
            you might be able to use this app if you exclude non-bus routes.")

    } else{
      text_output <- paste("There are a few reasons why this app may not meet your needs.
      (1) Because your agency has implemented a system redesign within the last three years,
            historic route ridership data may not accurately reflect current or future service patterns.
            As a result, this tool may not provide reliable ridership forecasts for your system.
            (2) Since most of your system ridership comes from a university or a single employer,
            the variables used in this app may not be the largest predictors of ridership,
            and, therefore, the app's predictions may not be accurate.
            Note that if you have your own data to capture these unique circumstances,
            there is an option to add those to the model.
            (3) Because this app was created to work with bus networks,
            this app may not meet your needs.
            If there have been no recent investments in rail transit,
            you might be able to use this app if you exclude non-bus routes.")

    }


    output$textWarn1 <- renderText(text_output)

    if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
      output$screening_button_placeholder <- renderUI({
        input_task_button("screening_button", "Continue")
      })
    } else{
      output$screening_button_placeholder <- renderUI({
        input_task_button("screening_button", "Continue Anyways")
      })
    }

  }) |>
    bindEvent(input$compatibility_button)

  observe({
    bslib::nav_select("main_nav", "pan_2")
  }) |>
    bindEvent(input$screening_button)

  # output$textWarn1 <- renderText({
  #   if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
  #     paste("This app might be useful to your agency :)")
  #   } else{
  #     paste(input$qRedesign)
  #   }
  # })
  #
  # output$textWarn2 <- renderText({
  #   if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
  #     paste("")
  #   } else{
  #     paste(input$qUniversity)
  #   }
  # })
  #
  # output$textWarn3 <- renderText({
  #   if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
  #     paste("")
  #   } else{
  #     paste(input$qRail)
  #   }
  # })

#### 2. RIDERSHIP DATA UPLOAD ####

  # Read and modify the uploaded data
  processed_data <- reactive({
    req(input$upload_data) # Ensure a file is uploaded
    df <- read_excel(input$upload_data$datapath)

    return(df)
  })

  # checking to make sure the data is formatted correctly
  check_names <- reactive({
    req(processed_data())
    df <- processed_data()

    needed_cols <- c("route_id", "month", "year", "upt", "vrm")
    # must_contain route_id, month,  year,   upt, and vrm columns
    check_names_long <- needed_cols %in% names(df)

    sum(check_names_long) == 5

  })

  check_numeric <- reactive({
    req(processed_data())
    df <- processed_data()

    # all columns except rout_id must be able to be numeric
    numeric_cols <- names(df)[names(df) != c("route_id")]
    for (col in numeric_cols){
      df[,col] <- tryCatch(
        {
          as.numeric(unlist(df[,col]))
        },
        warning = function(w) {
          return("warning")
        },
        error = function(e) {
          return("error")
        }
      )
    }

    check_numeric_long <- sapply(df, is.numeric)

    sum(check_numeric_long[-1]) == length(check_numeric_long[-1])
  })

  data_check <- reactive({
    req(check_names())
    req(check_numeric())

    check_names() == TRUE & check_numeric() == TRUE
  })


  # TODO: This doesn't seem to be working properly.
  # send a message if the data is not formatted correctly
  observe({
    req(processed_data())

    check_names <- check_names()
    check_numeric <- check_numeric()

    if (check_names == FALSE & check_numeric == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "It appears you are missing or have incorrectly spelled some of the required columns (route_id, month, year, upt, vrm). Additionally, you have some extra columns that can't be converted to numbers. Any extra columns in the data must be numeric."
        )
      )
    } else if(check_names == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "It appears you are missing or have incorrectly spelled some of the required columns (route_id, month, year, upt, vrm)."
        )
      )
    } else if(check_numeric == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "You have some extra columns that can't be converted to numbers. Any extra columns in the data must be numeric."
        )
      )
    } else if(check_numeric & check_names) {
      # # maybe I should put another notification saying the file looks good
      # showNotification("File Received and Processed",
      #                  type = "message")
    } else{
      showNotification("ERROR: There was an unknown error with your file. Please double check to make sure it follows the correct formatting",
                       type = "error",
                       duration = 15)
    }

  }) |>
    bindEvent(processed_data())

  output$example_input <- renderTable({
    upt <- read_excel("data/data_example.xlsx") |>
      mutate(month = as.character(month),
             year = as.character(year),
             upt = round(upt,1),
             vrm = round(vrm,1)) |>
      filter(month == 1) |>
      head()

    upt
  }, bordered = TRUE)


  addnl_vars <- reactive({
    req(processed_data())
    df <- processed_data()

    extra_variables <- names(df)[!names(df) %in% c("route_id","month","year","upt","vrm")]

    if (length(extra_variables) > 0){
      addnl_vars <- paste0("log_",extra_variables)
      names(addnl_vars) <- extra_variables
    } else{
      addnl_vars <- NULL
    }

    addnl_vars
  })


  ##### Copied AI code for fare change table #####


  # Current agency working table (no agency column yet; add on save)
  current <- reactiveVal(
    data.frame(
      change_date = as.Date(character()),
      prev_fare   = numeric(),
      new_fare    = numeric(),
      stringsAsFactors = FALSE
    )
  )

  observeEvent(input$add, {
    req(input$new_date)
    req(!is.na(input$new_prev), !is.na(input$new_new))

    df <- current()
    df <- rbind(df, data.frame(
      change_date = as.Date(input$new_date),
      prev_fare   = as.numeric(input$new_prev),
      new_fare    = as.numeric(input$new_new),
      stringsAsFactors = FALSE
    ))

    # Optional: keep sorted by date
    df <- df[order(df$change_date), ]

    current(df)
  })

  # Render current table (editable)
  output$tbl <- renderDT({
    datatable(
      current(),
      rownames = FALSE,
      selection = "multiple",
      editable = list(target = "cell", disable = list(columns = NULL)),
      options = list(dom = "t", paging = FALSE)
    ) |>
      formatStyle('change_date', backgroundColor = 'lightgrey') |>
      formatStyle('prev_fare', backgroundColor = 'lightgrey') |>
      formatStyle('new_fare', backgroundColor = 'lightgrey')
  })

  # Apply cell edits from DT to current()
  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit
    df <- current()

    i <- info$row
    j <- info$col + 1
    v <- info$value

    colname <- names(df)[j]

    if (colname == "change_date") {
      # Expect yyyy-mm-dd; coerce to Date
      v2 <- as.Date(v)
      if (is.na(v2)) return()  # ignore invalid edits
      df[i, j] <- v2
    } else {
      v2 <- suppressWarnings(as.numeric(v))
      if (is.na(v2)) return()
      df[i, j] <- v2
    }

    # Optional: re-sort after editing date
    df <- df[order(df$change_date), ]

    current(df)
  })

  # Delete selected rows
  observeEvent(input$delete, {
    sel <- input$tbl_rows_selected
    if (length(sel) == 0) return()
    df <- current()
    df <- df[-sel, , drop = FALSE]
    current(df)
  })

  observeEvent(input$has_fare_changes, {
    if (input$has_fare_changes == "no") {
      # clear your current fare-change table
      current(current()[0, , drop = FALSE])
      # optionally reset the add-row inputs too
      updateNumericInput(session, "new_prev", value = NA)
      updateNumericInput(session, "new_new",  value = NA)
      updateDateInput(session, "new_date", value = Sys.Date())
    }
  })

  fare_tbl <- reactive({
    req(current())

    fare_df <- current()

    if (nrow(fare_df) == 0){
      updated_fare_df <- NULL
    } else {
      updated_fare_df <- fare_df
    }

    updated_fare_df

  })


  # fare_tbl <- data.frame(change_date = "2024-06-20",
  #                        prev_fare = 2,
  #                        new_fare = 2.5)
  #
  # brt_tbl <- data.frame(change_date_brt = "2023-04-14",
  #                       routes_brt = "14")


#### 3. GTFS UPLOAD ####

  # getting routes sf from gtfs
  route_sf <- reactive({
    req(input$upload_routes) # Ensure a file is uploaded
    routes <- input$upload_routes$datapath

    tryCatch(
      {
        get_gtfs_routes(routes)
      },
      error = function(e) {
        return("error")
      }
    )

  })

  # display a message stating that the plot is rendering
  observe({
    showNotification(
      paste("Plotting routes. Please wait a moment."),
      type = "message",
      duration = 20
    )
  }) |>
    bindEvent(input$upload_routes)

  # getting the counties it touches
  county_sf <- reactive({
    req(!inherits(route_sf(), "character"))
    find_overlapping_counties(route_sf())
  })

  # once someone uploads the gtfs,
  observeEvent(input$upload_routes, {

    req(!inherits(route_sf(), "character"))

    output$route_map <- renderLeaflet({
      make_route_leaflet(route_sf(),county_sf())
    })

    output$acs_description <- renderText({
      "Your transit routes and the counties they cross are displayed to the right. If these are the routes and counties you expected, you are ready for the next step. Click the button below to pull the census data that will be used in the model."
    })

    # Once finished, render the button to pull acs data
    output$acs_button_placeholder <- renderUI({
      input_task_button("get_acs", "Get Census Data")
    })
  })

  # check to make sure it is a good file
  observe({
    req(route_sf())

    if (inherits(route_sf(), "character")){

      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "Unable to process file. Make sure it is a valid GTFS formatted file."
        )
      )

    } else{

    }

  }) |>
    bindEvent(input$upload_routes)


  ##### GET AND PREPARE CENSUS DATA #####

  acs_data <- reactiveVal(NULL)



  observeEvent(input$get_acs, {  #TODO: If they haven't input ridership data, this won't run, but in that case I need to put a popup to let users know that.
    req(data_check() == TRUE)

    vrm_data <- processed_data()

    year_start <- min(vrm_data$year, na.rm = T)
    month_start <- vrm_data |>
      dplyr::filter(year == year_start) |>
      dplyr::pull(month) |>
      min(na.rm = TRUE)
    month_start <- paste0(year_start, "-", month_start)


    year_end <- max(vrm_data$year, na.rm = T)
    month_end <- vrm_data |>
      dplyr::filter(year == year_end) |>
      dplyr::pull(month) |>
      max(na.rm = TRUE)
    month_end <- paste0(year_end, "-", month_end)

    # This prevents it from looking for ACS data above 2024.
    # TODO: It sill need to be updated to 2025 when the ACS 2025 data is available
    if(year_end > 2024){
      year_end_val <- 2024
    } else {
      year_end_val <- year_end
    }

    res <- withProgress(message = "Organizing Census Data...",
                        detail = "this could take a minute or two", value = 0, {

                          incProgress(0.05, detail = "Starting process")
                          route_geom  <- route_sf()
                          county_info <- county_sf()

                          state_fps  <- unique(county_info$STATEFP)
                          county_fps <- unique(county_info$COUNTYFP)
                          year_vals  <- (year_start-1):year_end_val

                          incProgress(0.20, detail = "Pulling in census tracts")
                          census_tract_geom <- get_tract_geometry(state_fps, county_fps, year_vals)

                          if (is.null(census_tract_geom)){
                            showModal(
                              modalDialog(
                                title = "ERROR",
                                easy_close = TRUE,
                                "Unable to retrieve census data.
                                If you are offline or there is a government shutdown, the data is unable to be accessed.
                                If you have good connection and www.census.gov seems to be working properly,
                                you might find success by simply trying this function again."
                              )
                            )
                          }

                          req(census_tract_geom)

                          incProgress(0.15, detail = "Finding tracts that intersect bus routes")
                          tract_buffer_data <- create_intersecting_tract_percentages(
                            census_tract_geom, route_geom
                          )

                          incProgress(0.30, detail = "Pulling ACS data")
                          pulled_acs <- pull_acs_data(county_sf = county_info, years = year_vals)

                          if ("errors" %in% names(pulled_acs)){
                            showModal(
                              modalDialog(
                                title = "ERROR",
                                easy_close = TRUE,
                                "Unable to retrieve census data.
                                If you are offline or there is a government shutdown, the data is unable to be accessed.
                                If you have good connection and www.census.gov seems to be working properly,
                                you might find success by simply trying this function again."
                              )
                            )
                          }

                          req(!"errors" %in% names(pulled_acs))

                          incProgress(0.05, detail = "Organizing ACS data")
                          organized_acs <- combine_acs_data(pulled_acs)

                          incProgress(0.20, detail = "Preparing data for model")
                          create_final_acs_data(
                            combined_acs_data   = organized_acs,
                            intersecting_tracts = tract_buffer_data,
                            start_month         = month_start,
                            end_month           = month_end
                          )
                        })

    acs_data(res)  # store result so outputs can use it

    bslib::nav_select("main_nav", "pan_4")
  })


#### 4. MODEL CREATION ####
  # get first model after user inputs the files
  first_model <- reactive({ # first model
    req(input$upload_data$datapath)
    req(acs_data())
    acs <- acs_data()
    # acs <- acs_data
    xl_data <- processed_data()
    vars <- c("[VRM]" = "log_vrm",
                  "[Month]" = "factor(month)",
                  "[Year]" = "year_cent",
                  "[Year Squared]" = "year_cent^2",
                  "[Gas Price]" = "log_gas_price",
                  "[% No Vehicle Households]" = "log_perc_hshlds_noveh",
                  "[% Workers Below Federal Poverty Line]" = "log_below_fpl",
                  "[% Commuting by Car]" = "log_perc_car",
                  "[% Commuting by Taxi]" = "log_perc_taxicab",
                  "[% Work From Home]" = "log_perc_wfh",
                  "[% Female Workers]" = "log_perc_female",
                  "[% Workers Between 100-150% of Federal Povery Level]" = "log_fpl_100_150",
                  "[% Workers in Renter Occupied Housing Units]" = "log_perc_renter_occupied",
                  "[Labor Participation Rate]" = "log_labor_part_rate",
                  "[Unemployment Rate]" = "log_unemp_rate",
                  "[Bus Rapid Transit]" = "brt",
                  "[Adult Base Fare]" = "log_fare",
             addnl_vars())


    create_regression_model(data_xlsx = xl_data,
                            acs_data = acs,
                            gas_csv = "data/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv",
                            variables = vars,
                            fare_df = fare_tbl()) # TODO: this doesn't seem to be working
  })

  output$tbl_mod_stepwise <- renderDT({
    created_model <- first_model() # first model

    name_key <- c("VRM" = "log_vrm",
                  "February" = "factor(month)2",
                  "March" = "factor(month)3",
                  "April" = "factor(month)4",
                  "May" = "factor(month)5",
                  "June" = "factor(month)6",
                  "July" = "factor(month)7",
                  "August" = "factor(month)8",
                  "September" = "factor(month)9",
                  "October" = "factor(month)10",
                  "November" = "factor(month)11",
                  "December" = "factor(month)12",
                  "Year" = "year_cent",
                  "Year Squared" = "I(year_cent^2)",
                  "Gas Price" = "log_gas_price",
                  "% No Vehicle Households" = "log_perc_hshlds_noveh",
                  "% Workers Below Federal Poverty Line" = "log_below_fpl",
                  "% Commuting by Car" = "log_perc_car",
                  "% Commuting by Taxi" = "log_perc_taxicab",
                  "% Work From Home" = "log_perc_wfh",
                  "% Female Workers" = "log_perc_female",
                  "% Workers Between 100-150% of Federal Povery Level" = "log_fpl_100_150",
                  "% Workers in Renter Occupied Housing Units" = "log_perc_renter_occupied",
                  "Labor Participation Rate" = "log_labor_part_rate",
                  "Unemployment Rate" = "log_unemp_rate",
                  "Is Bus Rapid Transit" = "brtTRUE",
                  "Fare" = "log_fare",
                  addnl_vars())

    var_table <- data.frame("Variable" = names(name_key),
                           "vars" = unname(name_key))

    coef_table <- data.frame("vars" = names(created_model$coefficients),
                        "Coeff" = round(created_model$coefficients,3),
                        "P.value" = round(fixest::pvalue(created_model), 3))

    table <- coef_table |>
      left_join(var_table, by = "vars") |>
      select("Variable", "Coeff", "P.value")

     table |>
      datatable(selection = "none",
                options = list(
                  dom = 't',         # Only show the Table (hides search, paging, etc.)
                  paging = FALSE,    # Show all data at once
                  ordering = FALSE,  # Disable column sorting
                  searching = FALSE,  # Remove the search box
                  rownames = FALSE
                ))
  })

  # Create the proxy handle for the output table
  proxy <- dataTableProxy("tbl_mod_stepwise")

  # once acs is uploaded, then it will update the selections for the model creation on the next page
  observeEvent(acs_data(),
               {
                 req(acs_data())
                 variables_used <- names(first_model()$coefficients) #first model

                 all_vars <- c("[VRM]" = "log_vrm",
                               "[Month]" = "factor(month)",
                               "[Year]" = "year_cent",
                               "[Year Squared]" = "year_cent^2",
                               "[Gas Price]" = "log_gas_price",
                               "[% No Vehicle Households]" = "log_perc_hshlds_noveh",
                               "[% Workers Below Federal Poverty Line]" = "log_below_fpl",
                               "[% Commuting by Car]" = "log_perc_car",
                               "[% Commuting by Taxi]" = "log_perc_taxicab",
                               "[% Work From Home]" = "log_perc_wfh",
                               "[% Female Workers]" = "log_perc_female",
                               "[% Workers Between 100-150% of Federal Povery Level]" = "log_fpl_100_150",
                               "[% Workers in Renter Occupied Housing Units]" = "log_perc_renter_occupied",
                               "[Labor Participation Rate]" = "log_labor_part_rate",
                               "[Unemployment Rate]" = "log_unemp_rate",
                               "[Bus Rapid Transit]" = "brt",
                               "[Adult Base Fare]" = "log_fare",
                               addnl_vars())

                 if (TRUE %in% grepl("month",variables_used)){
                   new_selected <- c(variables_used[grepl("month",variables_used) == FALSE], "factor(month)")
                 } else{
                 new_selected <- variables_used
               }
                 if (TRUE %in% grepl("year_cent^2",new_selected,fixed = TRUE)){
                   new_selected <- c(new_selected[grepl("year_cent^2",new_selected,fixed = TRUE) == FALSE], "year_cent^2")
                 }

                 new_vars <- all_vars[all_vars %in% new_selected]

                 updateSelectizeInput(inputId = "variables",
                                      choices = new_vars,
                                      selected = new_vars)

                 updateSelectizeInput(inputId = "variables_forced",
                                      choices = all_vars,
                                      selected = new_vars)

               })

  # value to make sure the model has been run
  model_ran <- reactiveVal(FALSE)

  # does the stepwise regression when the button is clicked
  model <- eventReactive(
    input$run_model_stepwise,
    ignoreNULL = TRUE,
    {
      model_ran(TRUE)
      req(input$upload_data$datapath)
      req(acs_data())
      create_regression_model(data_xlsx = processed_data(),
                              acs_data = acs_data(),
                              gas_csv = "data/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv",
                              variables = input$variables,
                              fare_df = fare_tbl())
    })

  # creates the regression model when the button is clicked (does not get rid of any coefficients)
  model_forced <- eventReactive(
    input$run_model_forced,
    ignoreNULL = TRUE,
    {
      req(input$upload_data$datapath)
      req(acs_data())
      create_regression_model_forced(data_xlsx = processed_data(),
                              acs_data = acs_data(),
                              gas_csv = "data/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv",
                              variables = input$variables_forced,
                              fare_df = fare_tbl())
    })

  # Use the proxy to swap in updated data frame
  observeEvent(input$run_model_stepwise, {

    created_model <- model()

    name_key <- c("VRM" = "log_vrm",
                  "February" = "factor(month)2",
                  "March" = "factor(month)3",
                  "April" = "factor(month)4",
                  "May" = "factor(month)5",
                  "June" = "factor(month)6",
                  "July" = "factor(month)7",
                  "August" = "factor(month)8",
                  "September" = "factor(month)9",
                  "October" = "factor(month)10",
                  "November" = "factor(month)11",
                  "December" = "factor(month)12",
                  "Year" = "year_cent",
                  "Year Squared" = "I(year_cent^2)",
                  "Gas Price" = "log_gas_price",
                  "% No Vehicle Households" = "log_perc_hshlds_noveh",
                  "% Workers Below Federal Poverty Line" = "log_below_fpl",
                  "% Commuting by Car" = "log_perc_car",
                  "% Commuting by Taxi" = "log_perc_taxicab",
                  "% Work From Home" = "log_perc_wfh",
                  "% Female Workers" = "log_perc_female",
                  "% Workers Between 100-150% of Federal Povery Level" = "log_fpl_100_150",
                  "% Workers in Renter Occupied Housing Units" = "log_perc_renter_occupied",
                  "Labor Participation Rate" = "log_labor_part_rate",
                  "Unemployment Rate" = "log_unemp_rate",
                  "Is Bus Rapid Transit" = "brtTRUE",
                  "Fare" = "log_fare",
                  addnl_vars())

    vars <- names(name_key[name_key %in% names(created_model$coefficients)])

    new_coeff_df <-  data.frame("Variable" = vars,
                                "Coeff" = round(created_model$coefficients, 3),
                                "P-value" = round(fixest::pvalue(created_model), 3))

    rownames(new_coeff_df) <- NULL

    replaceData(proxy, new_coeff_df, resetPaging = FALSE)
  })

  # show the coefficients that were generated from regression model function
  output$tbl_mod_forced <- renderDT({
    created_model <- model_forced()

    name_key <- c("VRM" = "log_vrm",
                  "February" = "factor(month)2",
                  "March" = "factor(month)3",
                  "April" = "factor(month)4",
                  "May" = "factor(month)5",
                  "June" = "factor(month)6",
                  "July" = "factor(month)7",
                  "August" = "factor(month)8",
                  "September" = "factor(month)9",
                  "October" = "factor(month)10",
                  "November" = "factor(month)11",
                  "December" = "factor(month)12",
                  "Year" = "year_cent",
                  "Year Squared" = "I(year_cent^2)",
                  "Gas Price" = "log_gas_price",
                  "% No Vehicle Households" = "log_perc_hshlds_noveh",
                  "% Workers Below Federal Poverty Line" = "log_below_fpl",
                  "% Commuting by Car" = "log_perc_car",
                  "% Commuting by Taxi" = "log_perc_taxicab",
                  "% Work From Home" = "log_perc_wfh",
                  "% Female Workers" = "log_perc_female",
                  "% Workers Between 100-150% of Federal Povery Level" = "log_fpl_100_150",
                  "% Workers in Renter Occupied Housing Units" = "log_perc_renter_occupied",
                  "Labor Participation Rate" = "log_labor_part_rate",
                  "Unemployment Rate" = "log_unemp_rate",
                  "Is Bus Rapid Transit" = "brtTRUE",
                  "Fare" = "log_fare",
                  addnl_vars())

    var_table <- data.frame("Variable" = names(name_key),
                            "vars" = unname(name_key))

    coef_table <- data.frame("vars" = names(created_model$coefficients),
                             "Coeff" = round(created_model$coefficients,3),
                             "P.value" = round(fixest::pvalue(created_model), 3))


    coef_table |>
      left_join(var_table, by = "vars") |>
      select("Variable", "Coeff", "P.value") |>
      datatable(selection = "none",
                options = list(
                  dom = 't',         # Only show the Table (hides search, paging, etc.)
                  paging = FALSE,    # Show all data at once
                  ordering = FALSE,  # Disable column sorting
                  searching = FALSE,  # Remove the search box
                  rownames = FALSE
                ))
  })

  selected_model <- reactiveVal(NULL)

  observeEvent(input$use_this_model_button, {
    req(first_model())

    if (model_ran() == FALSE){ # if the second model was not run, then use the first model
      m <- isolate(first_model())
    } else{
      m <- isolate(model())
    }

    selected_model(m)

    bslib::nav_select("main_nav", "pan_5")
  })

  observeEvent(input$use_this_model_button_forced, {
    fm <- isolate(model_forced())
    req(fm)
    selected_model(fm)
    bslib::nav_select("main_nav", "pan_5")
  })


#### 5. REVIEW MODEL ####
  output$coefficients_review <- render_gt({
    req(!is.null(selected_model()))
    check_coefficients(selected_model(),addnl_vars())
  })


  # getting a vector with the final coefficients that will be used in the model
  final_coefs <- reactive({
    req(selected_model())

    if (is.null(input$forced_coef_checkbox)){
      final_coefs <- coef(selected_model())
    } else{
      updated_coefs <- input$forced_coef_checkbox
      # updated_coefs <- c("vrm","gas","fares")

      new_coefs <- c(vrm = .45, gas = .15, fares = -0.3, brt = -0.3)
      new_coefs <- new_coefs[names(new_coefs) %in% updated_coefs]

      coefs_og <- coef(selected_model())

      # map coefs_og names -> new_coef names
      map <- c(log_vrm = "vrm",
               log_gas_price = "gas",
               fare = "fares",
               brt = "brt")
      new_names <- map[map %in% names(new_coefs)]
      names(new_coefs) <- names(new_names)

      # replace v2 values where mapping exists (and key exists in v1)
      idx <- !names(coefs_og) %in% names(new_coefs)
      final_coefs <- c(coefs_og[idx],new_coefs)
    }


    final_coefs

  })

  observe({
    req(first_model())

    if ("brt" %in% names(final_coefs())){
      output$brt_question_placeholder <- renderUI({
        radioButtons(
          inputId = "brt_question",
          label = "Do you plan to convert any of your routes to BRT?",
          choices = list("No" = "no","Yes" = "yes"),
          selected = "no",
          inline = TRUE
        )
      })
    }
  }) |>
    bindEvent(input$use_this_model_button)

  observe({
    req(first_model())

    if ("brt" %in% names(final_coefs())){
      output$brt_question_placeholder <- renderUI({
        radioButtons(
          inputId = "brt_question",
          label = "Do you plan to convert any of your routes to BRT?",
          choices = list("No" = "no","Yes" = "yes"),
          selected = "no",
          inline = TRUE
        )
      })
    }
  }) |>
    bindEvent(input$use_this_model_button_forced)

  # If BRT check is clicked, or if the model has a brt in it,
  # then ask if they plan to have any more brt conversions
  observe({
    if ("brt" %in% input$forced_coef_checkbox){
      output$brt_question_placeholder <- renderUI({
        radioButtons(
          inputId = "brt_question",
          label = "Do you plan to convert any of your routes to BRT?",
          choices = list("No" = "no","Yes" = "yes"),
          selected = "no",
          inline = TRUE
        )
      })
    }

  }) |>
    bindEvent(input$forced_coef_checkbox)


  ##### BRT CHNAGES CODE #####


  # add inputs for a brt change
  observeEvent(input$brt_question, {

    if (input$brt_question == "yes"){

      # render the date input
      output$brt_date <- renderUI({
        dateInput(
          inputId = "brt_change_date",
          label = "Date of change",
          value = Sys.Date(),
          min = Sys.Date() - 10000,
          max = Sys.Date(),
          width = "150px")
      })

      input_df <- processed_data()
      routes <- input_df$route_id

      if (!is.null(routes)){
        route_list <- as.list(routes)
      } else{
        route_list <- list("no","routes","found")
      }

      output$brt_routes <- renderUI({
        selectizeInput(
          "select_brt_routes",
          "Routes converted to BRT",
          route_list,
          multiple = TRUE,
          width = "200px"
        )
      })


    } else{
      # render the date input
      output$brt_date <- renderUI({
        ""
      })

      output$brt_routes <- renderUI({
        ""
      })



    }

  })


  ### COPIED AI RESPONSE FOR BRT STUFF ###

  # Current agency working table
  current_brt <- reactiveVal(
    data.frame(
      change_date_brt = as.Date(character()),
      routes_brt = character(),
      stringsAsFactors = FALSE
    )
  )

  observeEvent(input$add_brt, {
    req(input$brt_change_date)
    req(!is.na(input$select_brt_routes))

    df <- current_brt()
    df <- rbind(df, data.frame(
      change_date_brt = as.Date(input$brt_change_date),
      routes_brt   = paste(as.character(input$select_brt_routes), collapse = ","),
      stringsAsFactors = FALSE
    ))

    # Optional: keep sorted by date
    df <- df[order(df$change_date_brt), ]

    current_brt(df)
  })

  # Render current_brt table (editable)
  output$tbl_brt <- renderDT({
    datatable(
      current_brt(),
      rownames = FALSE,
      selection = "multiple",
      editable = list(target = "cell", disable = list(columns = NULL)),
      options = list(dom = "t", paging = FALSE)
    )  |>
      formatStyle('change_date_brt', backgroundColor = 'lightgrey') |>
      formatStyle('routes_brt', backgroundColor = 'lightgrey')
  })

  # Apply cell edits from DT to current_brt()
  observeEvent(input$tbl_brt_cell_edit, {
    info <- input$tbl_brt_cell_edit
    df <- current_brt()

    i <- info$row
    j <- info$col + 1
    v <- info$value

    colname <- names(df)[j]

    if (colname == "change_date_brt") {
      # Expect yyyy-mm-dd; coerce to Date
      v2 <- as.Date(v)
      if (is.na(v2)) return()  # ignore invalid edits
      df[i, j] <- v2
    }

    # Optional: re-sort after editing date
    df <- df[order(df$change_date_brt), ]

    current_brt(df)
  })

  # Delete selected rows
  observeEvent(input$delete_brt, {
    sel <- input$tbl_brt_rows_selected
    if (length(sel) == 0) return()
    df <- current_brt()
    df <- df[-sel, , drop = FALSE]
    current_brt(df)
  })

  observeEvent(input$brt_question, {
    if (input$brt_question == "no") {
      # clear your current_brt fare-change table
      current_brt(current_brt()[0, , drop = FALSE])
      # optionally reset the add-row inputs too
      updateDateInput(session, "brt_change_date", value = Sys.Date())
    }
  })



  brt_tbl <- reactive({
    req(current_brt())

    brt_df <- current_brt()

    if (nrow(brt_df) == 0){
      updated_brt_df <- NULL
    } else {
      df_list <- list()
      for (row_num in 1:nrow(brt_df)){
        new_df <- data.frame(change_date_brt = brt_df$change_date_brt[[row_num]], routes_brt = unlist(strsplit(brt_df$routes_brt[[row_num]],",")))
        df_list[[row_num]] <- new_df
      }

      updated_brt_df <- bind_rows(df_list)
    }

    updated_brt_df

  })


#### 6 FORECAST AND VIZUALIZATION ####

  routes <- reactive({
    req(input$upload_data$datapath)
    ridership_df <- processed_data()

    unique(ridership_df$route_id)
  })

  v <- reactiveValues(data = NULL)

  # NEW: store saved scenarios (all routes)
  saved <- reactiveValues(by_route = NULL)

  # NEW: for overwrite confirmation workflow
  pending <- reactiveValues(routes_to_save = NULL, mode = NULL)

  # NEW: your full set of routes (replace routes() with your real source)
  all_routes <- reactive({
    req(routes())     # routes() should return a character vector of route ids/names
    routes()
  })

  # NEW: which routes already have saved scenarios
  saved_routes <- reactive({
    if (is.null(saved$by_route)) character(0) else sort(unique(saved$by_route$Route))
  })

  # NEW: remaining (unsaved) routes
  unsaved_routes <- reactive({
    setdiff(all_routes(), saved_routes())
  })

  # Use an observer to get the model they choose to use
  # and create a data frame for the forecasting inputs
  observeEvent(input$proceed_to_forecast, {
    req(final_coefs())
    elasticities <- get_elasticity_varaibles(final_coefs(), addnl_vars())

    elast_table <- data.frame("Variable" = names(elasticities),
                              "Low" = "-1%",
                              "Mid" = "2%",
                              "High" = "5%")
    elast_table <- elast_table[,2:4]
    rownames(elast_table) <- names(elasticities)
    names(elast_table) <- c("Low Estimate","Mid Estimate","High Estimate")

    # Update the reactiveValues with the elasticities table
    v$data <- elast_table

    bslib::nav_select("main_nav", "pan_6")
  })

  # NEW: keep the route dropdown updated based on mode and what's already saved
  observe({
    req(input$route_mode)

    if (input$route_mode == "new") {
      rem <- unsaved_routes()
      choices <- c("All remaining routes" = "__ALL_REMAINING__", rem)

      updateSelectInput(
        session, "route_selected",
        choices = choices,
        selected = if (length(rem)) rem[1] else "__ALL_REMAINING__"
      )

    } else { # overwrite mode
      existing <- saved_routes()
      choices <- c(existing, "All saved routes" = "__ALL_SAVED__")

      updateSelectInput(
        session, "route_selected",
        choices = choices,
        selected = if (length(existing)) existing[1] else "__ALL_SAVED__"
      )
    }
  })

  # 2. Render the table with editable = TRUE
  output$dtScenarios <- renderDT({
    req(v$data)
    datatable(v$data, editable = 'cell', selection = 'none',
              options = list(
                dom = 't',         # Only show the Table (hides search, paging, etc.)
                paging = FALSE,    # Show all data at once
                ordering = FALSE,  # Disable column sorting
                searching = FALSE  # Remove the search box
              ))
  })

  # 3. Use a proxy to update the table without a full re-render
  proxy_scenarios <- dataTableProxy('dtScenarios')

  # 4. Observe the 'cell_edit' event
  observeEvent(input$dtScenarios_cell_edit, {
    info <- input$dtScenarios_cell_edit

    # Extract row and column (DT uses 0-based indexing for columns)
    i <- info$row
    j <- info$col # they say i need to have +1, but I don't htink I do
    k <- info$value

    v$data[i, j] <- DT::coerceValue(k, v$data[i, j])

    replaceData(proxy_scenarios, v$data, resetPaging = FALSE)
  })



  # NEW: helper that performs the save (overwrites only targeted routes)
  save_routes <- function(routes_to_save) {
    base <- data.frame(
      Variable = rownames(v$data),
      as.data.frame(v$data, stringsAsFactors = FALSE),
      row.names = NULL
    )

    to_save <- do.call(rbind, lapply(routes_to_save, function(rt) cbind(Route = rt, base)))

    if (is.null(saved$by_route)) {
      saved$by_route <- to_save
    } else {
      saved$by_route <- subset(saved$by_route, !(Route %in% routes_to_save))
      saved$by_route <- rbind(saved$by_route, to_save)
    }
  }

  # NEW: Save button handler (with confirmation when overwriting)
  observeEvent(input$save_route_scenario, {
    req(v$data, input$route_selected, input$route_mode)

    routes_to_save <- if (input$route_mode == "new") {
      if (identical(input$route_selected, "__ALL_REMAINING__")) unsaved_routes() else input$route_selected
    } else {
      if (identical(input$route_selected, "__ALL_SAVED__")) saved_routes() else input$route_selected
    }

    validate(need(length(routes_to_save) > 0, "No routes selected."))

    # Require confirmation for overwrite mode
    if (input$route_mode == "overwrite") {
      pending$routes_to_save <- routes_to_save
      pending$mode <- "overwrite"

      showModal(modalDialog(
        title = "Confirm overwrite",
        paste0(
          "You are about to overwrite saved scenarios for ",
          length(routes_to_save), " route(s):\n",
          paste(head(routes_to_save, 10), collapse = ", "),
          if (length(routes_to_save) > 10) ", ..." else ""
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_overwrite", "Yes, overwrite", class = "btn-danger")
        )
      ))
      return()
    }

    # New routes mode: save immediately
    save_routes(routes_to_save)
  })

  # NEW: confirm overwrite
  observeEvent(input$confirm_overwrite, {
    removeModal()
    req(pending$routes_to_save)
    save_routes(pending$routes_to_save)
    pending$routes_to_save <- NULL
    pending$mode <- NULL
  })

  # Optional: view saved scenarios
  output$dtSavedScenarios <- renderDT({
    req(saved$by_route)
    datatable(saved$by_route, options = list(pageLength = 25))
  })



  ## RUNNING FORECASTS ##

  forecast_df <- eventReactive(input$buttonRun, {

    req(final_coefs())
    req(acs_data())
    req(!is.null(v$data))
    req(saved$by_route)

    saved_predictions <- saved$by_route

    routes <- routes()

    coefs <- final_coefs()
    acs <- acs_data()

    processed_data <- processed_data()

    grouped_routes <- saved_predictions |>
      group_by(Route) |>
      summarize(low = paste(Low.Estimate, collapse = ", "),
                mid = paste(Mid.Estimate, collapse = ", "),
                high = paste(High.Estimate, collapse = ", ")) |>
      ungroup() |>
      group_by(low, mid, high) |>
      mutate(route_group_id = cur_group_id()) |>
      ungroup() |>
      select(Route, route_group_id)

    grouped_predictions <- saved_predictions |>
      left_join(grouped_routes, by = join_by(Route))


    if (length(unique(saved_predictions$Route)) == length(routes)){
      forecast_dfs <- list()
      for (group_id in unique(grouped_predictions$route_group_id)){
        filtered_grouped_predictions <- grouped_predictions |>
          filter(route_group_id == group_id)

        one_route_in_group <- filtered_grouped_predictions$Route[[1]]

        scenario_df <- grouped_predictions |>
          filter(Route == one_route_in_group)

        cat("scneario_df")
        print(scenario_df)

        df_unfiltered <- forecast_ridership(coefs = coefs,
                                 data_xlsx = processed_data,
                                 acs_data = acs,
                                 gas_csv = "data/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv",
                                 scenario_inputs_df = scenario_df,
                                 start_year = NULL,
                                 start_month = NULL) #,
                                 #fare_df = fare_tbl(), # TODO: make sure this works
                                # brt_df = brt_tbl())
        cat("head(df_unfiltered)")
        print(head(df_unfiltered))
        cat("tail(df_unfiltered)")
        print(tail(df_unfiltered))

        df <- df_unfiltered |>
          filter(route_id %in% unique(filtered_grouped_predictions$Route))

        cat("head(df)")
        print(head(filter(df, year == 2026)))
        cat("tail(df)")
        print(tail(filter(df, year == 2026)))

        forecast_dfs[[as.character(group_id)]] <- df
      }



      observeEvent(input$buttonRun, {
        bslib::nav_select("main_nav", "pan_7")
      })

      final_df <- bind_rows(forecast_dfs)

      cat("head(final_df)")
      print(head(filter(final_df, year == 2026)))
      cat("tail(final_df)")
      print(tail(filter(final_df, year == 2026)))

      df_all_routes <- final_df |>
        summarize(route_id = "all_routes",
                  avg_daily_upt = sum(avg_daily_upt, na.rm = T),
                  tot_weekday_upt = sum(tot_weekday_upt, na.rm = T),
                  .by = c(year, month, scenario)) |>
        mutate(date = ym(paste(year, month,sep = "/")))

      final_df <- bind_rows(final_df, df_all_routes)


    } else{
      final_df <- NULL
    }

    final_df

  })


  ## PLOT TO WORK WITH FORECASTS ##

  forcast_preview <- eventReactive(input$buttonRun, {
    req(forecast_df())
    plot_forecast(forecast_df())
  })


  # output$forcast_plot <- renderPlot({
  #   req(forecast_df())
  #   forcast_preview()
  # })


#### 7. VISUALIZATION PAGE ####

  observeEvent(input$buttonRun, {
    req(forecast_df())
    df <- forecast_df()
    updateSelectInput(
      session, "input_route_to_plot",
      choices = sort(unique(df$route_id)),
      selected = unique(df$route_id)[1]
    )
  }, ignoreInit = TRUE)

  output$viz_plot <- renderPlot({
    req(forecast_df())
    req(forcast_preview())
    df <- forecast_df()
    plot_forecast(df, route = input$input_route_to_plot)
  })

  # output$viz_plot_2 <- renderPlot({
  #   req(forecast_df())
  #   req(forcast_preview())
  #   df <- forecast_df()
  #   plot_forecast_facet(df)
  # })

#### 8. FINAL DOWNLOAD PAGE ####

  output_df <- reactive({
    forecast_df() |>
      select(route_id, year, month, avg_daily_upt, tot_weekday_upt, forecast, scenario, date)
  })

  # Show a preview of the output that is about to be downloaded
  output$outputExample <- renderDT({
    req(forecast_df())
    output_df <- output_df()
    output_df |>
      mutate(avg_daily_upt = round(avg_daily_upt,1),
             tot_weekday_upt = round(tot_weekday_upt,1))
  })

  # Handle the csv download
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("TRiP-forcasted-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(output_df(), file, row.names = FALSE)
    }
  )

  # Handle the xlsx download
  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste("TRiP-forcasted-data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(output_df(), file)
    }
  )

}

shinyApp(ui, server)


