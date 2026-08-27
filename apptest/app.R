library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(


  layout = c(
    "desc       model1 model2",
    "create_mod model1 model2",
    "create_mod model1 model2"
  ),
  row_sizes = c(
    "1.66fr",
    "0.67fr",
    "0.67fr"
  ),
  col_sizes = c(
    "550px",
    "1fr",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "desc",
    card_header("Description"),
    card_body(
      markdown(
        mds = c(
          "hello _world_"
        )
      )
    )
  ),
  grid_card(
    area = "create_mod",
    card_body(
      selectizeInput(inputId = "variables_forced",
                     label = "Varaible Selection",
                     choices = NULL,
                     selected = NULL,
                     multiple = TRUE,
                     width = "100%"
      ),
      div(
        style = "display:flex; gap:.5rem; flex-wrap:wrap;",
        input_task_button("run_model_stepwise", "Create Model (Stepwise)"),
        input_task_button("run_model_forced", "Create Model (Forced)")
      )
    )
  ),
  grid_card(
    area = "model2",
    card_header("Alternative Model Results"),
    card_body(
      input_task_button("use_this_model_button_forced", "Continue With This Model"),
      DTOutput(outputId = "tbl_mod_forced")
    )
  ),
  grid_card(
    area = "model1",
    card_header("Standard Model Results"),
    card_body(
      input_task_button("use_this_model_button", "Continue With This Model"),
      DTOutput(outputId = "tbl_mod_stepwise")
    )
  )


)


server <- function(input, output) {



  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })

  output$modtable1 <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)












