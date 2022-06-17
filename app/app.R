# library(shiny)
# library(bs4Dash)
# source('global.R')

ui <- dashboardPage(dark = NULL, title = 'Understanding Uncertainty',

  header = dashboardHeader(#disable = TRUE,
    title = dashboardBrand(
      title = '',
      href = 'https://codeclan.com/courses/data-courses/',
      image = 'codeclan-logo-white.png'
    )
  ),

  dashboardSidebar(minified = FALSE, width = '200px',
    fluidRow(
      purrr::pmap(input_init, numericInput),
      actionButton('update_btn', 'New sample', ),
      checkboxInput('show_ci', 'Show error bars?'),
      radioButtons('alpha', 'Set Confidence level',
                   choiceNames = paste(c(99, 95, 90), '%'),
                   choiceValues = 1-c(0.01, 0.05, 0.1), selected = 1-0.05)
    )
  ),

  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = 'cc-style-bs4.css')),

    fluidRow(
      column(6,
             plotOutput('difference_plot')
      ),
      column(6,
             plotOutput('distr_plot')
             # distrPlotUI('distr_plot')
      )
    ),
    valueBoxUI('vb')
  )
)

server <- function(input, output, session) {

  test_data <- reactive({
    rnorm(input$n_x, input$sample_mean, input$sample_sd)
  }) %>%
    bindEvent(input$update_btn, input$n_x, ignoreNULL = FALSE)

  x <- reactive({
    req(input$sample_mean, input$sample_sd >= 1, input$n_x >= 2)

    res <- t.test(test_data(), mu = expected_mean, conf.level = as.numeric(input$alpha))

    list(estimate = res$estimate,
         tval     = res$statistic,
         pval     = res$p.val,
         ci_lwr   = res$conf.int[1],
         ci_upr   = res$conf.int[2],
         qfun     = qnorm(res$p.value),
         cdf      = pnorm(res$p.value),
         dqfun    = dnorm(qnorm(res$p.value)))
  })

  output$difference_plot <- renderPlot({
    validate(
      need(input$n_x >= 2, 'There must be at least 2 observations.'),
      need(input$sample_mean, 'Enter a value for the average.'),
      need(input$sample_sd >= 1, 'Enter a value greater than 1 for the spread.')
    )

    par(mar = c(2,6,1,1), bg = pal['students1'])

    plot(1, ylim = c(0, 150), axes = FALSE, type = 'n',
         xlab = '', ylab = '')
    axis(2, seq(0, 150, 50), las = 1, cex.axis = 2)
    abline(h = expected_mean, col = pal['partners1'], lwd = 3)
    text(.7, y = expected_mean, labels = 'Expected', col = pal['partners1'],
         cex = 1.5, pos = 3)
    text(1, y = x()$estimate, labels = 'Observed', col = pal['psd_blue'],
         cex = 1.5, pos = 4, offset = 2)
    if (input$show_ci) arrows(1, x()$ci_lwr, 1, x()$ci_upr, angle = 90, code = 3, lwd = 3)
    points(1, x()$estimate, pch = 21, col = pal['psd_blue1'], bg = pal['data1'], lwd = 4, cex = 3)
  })

  output$distr_plot <- renderPlot({
    sig_threshold <- qnorm((1 - as.numeric(input$alpha)) / 2)

    par(mar = c(3, 8, 1, 1), mfrow = c(2, 1),
        xpd = TRUE, bg = pal['students1'])

    plot_base(distr_data$xs, distr_data$dfun, FALSE,
              0:4/10, 'Probability\ndensity\n')
    abline(v = -4:4, col = 'grey90')
    add_p_curve(distr_data, 'd')
    abline(v = sig_threshold, col = pal['partners1'], lwd = 3)
    add_prob_lines(x()$qfun, x()$dqfun, -10)

    # --------------------------------------------- #

    plot_base(distr_data$xs, distr_data$pfun, TRUE,
              seq(0, 1, .25), 'Cumulative\ndistribution\n')
    segments(-4:4, rep(-.05, 9), -4:4, rep(2, 9), col = 'grey90')
    add_p_curve(distr_data, 'p')
    segments(sig_threshold, -.1, sig_threshold, 1.2,
             col = pal['partners1'], lwd = 3)
    add_prob_lines(x()$qfun, x()$pval, 10)

  })
  # distrPlotServer('distr_plot', x, distr_data, isolate(input$alpha), pal)

  valueBoxServer('vb', x)
}

shinyApp(ui, server, onStart = \() source('global.R'))


