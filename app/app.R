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
    inputUI('ui_mod')
  ),

  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = 'cc-style-bs4.css')),

    fluidRow(
      column(6, diffPlotUI('diff_plot')),
      column(6, distrPlotUI('distr_plot'))
    ),
    valueBoxUI('vb'),
    accordionUI('data_summary')
  )
)

server <- function(input, output, session) {

  test_data <- reactive({
    req(ri$smn(), ri$ssd() >= 1, ri$nx() >= 2)
    rnorm(ri$nx(), ri$smn(), ri$ssd())
  }) |>
    bindEvent(ri$btn(), ri$nx(), ri$smn(), ri$ssd(),
              ignoreNULL = FALSE)

  test_res <- reactive({
    res <- t.test(test_data(), mu = mean_init,
                  conf.level = as.numeric(ri$al()))

    p  <- res$p.value
    df <- res$parameter
    list(estimate = res$estimate,
         tval     = res$statistic,
         df       = df,
         pval     = res$p.val,
         alpha    = 1 - attr(res$conf.int, 'conf.level'),
         ci_lwr   = res$conf.int[1],
         ci_upr   = res$conf.int[2],
         pfun     = pt(res$statistic, df),
         dfun     = dt(res$statistic, df),
         qfun     = qt(p, df)
    )
  })

  distr_data <- reactive({

    xs <- seq(-4, 4, l = 100)

    list(
      xs = xs, dfun = dt(xs, test_res()$df), pfun = pt(xs, test_res()$df)
    )

  })

  ri <- inputServer('ui_mod')
  diffPlotServer('diff_plot', test_data, test_res, ri)
  distrPlotServer('distr_plot', distr_data, test_res, ri)
  valueBoxServer('vb', test_res)
  accordionServer('data_summary', test_data, ri$smn(), ri$ssd())

}

shinyApp(ui, server, options = list(port = 3388))
