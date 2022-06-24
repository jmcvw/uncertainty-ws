source('global.R')

ui <- dashboardPage(dark = NULL, fullscreen = TRUE, title = 'Understanding Uncertainty',

  header = dashboardHeader(
    title = dashboardBrand(
      title = '',
      href = 'https://codeclan.com/courses/data-courses/',
      image = 'codeclan-logo-white.png'
    )
  ),

  dashboardSidebar(minified = FALSE, width = '200px',
    fluidRow(
      purrr::pmap(input_init, numericInput),
      actionButton('update_btn', 'New sample'),

      checkboxGroupInput('show_data', 'Show...', choices = c('Points', 'CI')),

      radioButtons('alpha', 'Set confidence level',
                   choiceNames = paste(c(99, 95, 90), '%'),
                   choiceValues = 1-c(0.01, 0.05, 0.1), selected = 1-0.05),
    )
  ),

  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = 'cc-style-bs4.css')),

    fluidRow(
      column(6,
             diffPlotUI('diff_plot')
      ),
      column(6,
             # plotOutput('distr_plot')
             distrPlotUI('distr_plot')
      )
    ),
    valueBoxUI('vb'),
    accordionUI('data_summary')

  )
)

server <- function(input, output, session) {

  ri <- reactive({
    list(nx  = input$n_x,
         smn = input$sample_mean,
         ssd = input$sample_sd,
         al  = input$alpha,
         ss  = input$show_data
    )
  })

  test_data <- reactive({
      rnorm(input$n_x, input$sample_mean, input$sample_sd)
  }) |>
    bindEvent(input$update_btn, ri()[1:3],
              ignoreNULL = FALSE)

  test_res <- reactive({
    req(input$sample_mean, input$sample_sd >= 1, input$n_x >= 2)

    res <- t.test(test_data(), mu = expected_mean,
                  conf.level = as.numeric(input$alpha))

    p <- res$p.value / 2
    list(estimate = res$estimate,
         tval   = res$statistic,
         pval   = res$p.val,
         alpha  = 1 - attr(res$conf.int, 'conf.level'),
         ci_lwr = res$conf.int[1],
         ci_upr = res$conf.int[2],
         dfun   = dnorm(p),
         qfun   = qnorm(p),
         dqfun  = dnorm(qnorm(p))
         )
  })

  diffPlotServer('diff_plot', test_data, test_res, ri)

  distrPlotServer('distr_plot', distr_data, test_res, ri)

  valueBoxServer('vb', test_res)

  accordionServer('data_summary', test_data, ri()$smn, ri()$ssd)

}

shinyApp(ui, server, options = list(port = 3388))
