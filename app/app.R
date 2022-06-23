source('global.R')

ui <- dashboardPage(dark = NULL, fullscreen = TRUE, title = 'Understanding Uncertainty',

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
      actionButton('update_btn', 'New sample'),

      checkboxGroupInput('show_stuff', 'Show...', choices = c('Points', 'CI')),
      # checkboxInput('show_ci', 'Show error bars?'),
      # checkboxInput('show_points', 'Show points?'),

      radioButtons('alpha', 'Set confidence level',
                   choiceNames = paste(c(99, 95, 90), '%'),
                   choiceValues = 1-c(0.01, 0.05, 0.1), selected = 1-0.05),
      # checkboxInput('debug', 'Debug?'),
    )
  ),

  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = 'cc-style-bs4.css')),

    fluidRow(
      column(6,
             # plotOutput('difference_plot')
             diffPlotUI('diff_plot')
      ),
      column(6,
             plotOutput('distr_plot')
             # distrPlotUI('distr_plot')
      )
    ),
    valueBoxUI('vb'),
    accordionUI('data_summary')

  )
)

server <- function(input, output, session) {

  test_data <- reactive({
      rnorm(input$n_x, input$sample_mean, input$sample_sd)
  }) |>
    bindEvent(input$update_btn, input$n_x, ignoreNULL = FALSE)

  test_res <- reactive({
    req(input$sample_mean, input$sample_sd >= 1, input$n_x >= 2)

    res <- t.test(test_data(),
                  mu = expected_mean, conf.level = as.numeric(input$alpha))

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

  ri <- reactive({
    list(nx = input$n_x,
         smn = input$sample_mean,
         ssd = input$sample_sd,
         al = input$alpha,
         ss = input$show_stuff
    )
  })


  diffPlotServer('diff_plot', test_data, test_res,
                 ri()$nx, ri()$smn, ri()$ssd, ri()$ss)

  # output$difference_plot <- renderPlot({
  #   validate(
  #     need(input$n_x >= 2, 'There must be at least 2 observations.'),
  #     need(input$sample_mean, 'Enter a value for the average.'),
  #     need(input$sample_sd >= 1, 'Enter a value greater than 1 for the spread.')
  #   )
  #
  #   par(mar = c(2,6,1,1), bg = pal['students1'])
  #
  #   plot(1, ylim = c(0, 150), axes = FALSE, type = 'n',
  #        xlab = '', ylab = '')
  #   axis(2, seq(0, 150, 50), las = 1, cex.axis = 2)
  #   abline(h = expected_mean, col = pal['partners1'], lwd = 3)
  #   text(.7, y = expected_mean, labels = 'Expected', col = pal['partners1'],
  #        cex = 1.5, pos = 3)
  #   text(1, y = test_res()$estimate, labels = (paste('Observed:\n', round(test_res()$estimate, 2))), col = pal['psd_blue'],
  #        cex = 1.5, pos = 4, offset = 2)
  #   if ('CI' %in% input$show_stuff) arrows(1, test_res()$ci_lwr, 1, test_res()$ci_upr, angle = 90, code = 3, lwd = 3)
  #   points(1, test_res()$estimate, pch = 21, col = pal['psd_blue1'], bg = pal['data1'], lwd = 4, cex = 3)
  #
  #   if ('Points' %in% input$show_stuff)
  #     points(jitter(rep(.9, input$n_x)), test_data(), pch = 20, col = pal['data1'], cex = 1)
  # })

  output$distr_plot <- renderPlot({
    sig_threshold <- qnorm((1 - as.numeric(input$alpha)) / 2)

    par(mar = c(3, 8, 1, 1), mfrow = c(2, 1),
        xpd = TRUE, bg = pal['students1'])

    plot_base(distr_data$xs, distr_data$pfun, FALSE,
              seq(0, 1, .25), 'Cumulative\ndistribution\n')
    abline(v = -4:4, col = 'grey90')
    add_p_curve(distr_data, 'p')
    abline(v = sig_threshold, col = pal['partners1'], lwd = 3)
    add_prob_lines(test_res()$qfun, test_res()$pval/2, -10)

    # --------------------------------------------- #

    plot_base(distr_data$xs, distr_data$dfun, TRUE,
              0:4/10, 'Probability\ndensity\n')
    segments(-4:4, rep(-.01, 9), -4:4, rep(.5, 9), col = 'grey90')
    add_p_curve(distr_data, 'd')
    segments(sig_threshold, -.01, sig_threshold, .5,
             col = pal['partners1'], lwd = 3)
    add_prob_lines(test_res()$qfun, test_res()$dqfun, 10)

  })
  # distrPlotServer('distr_plot', x, distr_data, isolate(input$alpha), pal)

  valueBoxServer('vb', test_res)


  accordionServer('data_summary', test_data, ri()$smn, ri()$ssd)

}

shinyApp(ui, server, options = list(port = 3388))
