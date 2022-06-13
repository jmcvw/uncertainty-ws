library(shiny)
library(bs4Dash)

pal <- c(students1  = "#38b0e3",
         students2  = "#50a3cd",
         students3  = "#e8f3f9",
         partners1  = "#fad350",
         data1      = "#e9415e",
         psd_blue1  = "#1b3445",
         partners2  = "#e3c375",
         data2      = "#fef0f2",
         psd_blue2  = "#0f252f")


expected_mean <- 100
mean_init <- sample(seq(90, 120, 5), 1)

res <- t.test(rnorm(100))
p <- res$p.value
alpha <- 0.05 / 2


xs <- seq(-4, 4, l = 100)
distr_data <- list(
  xs = xs, dfun = dnorm(xs), pfun = pnorm(xs)
)




ui <- dashboardPage(dark = NULL, title = 'Understanding Uncertainty',

  header = dashboardHeader(disable = TRUE,
    title = dashboardBrand(
      title = '',
      href = "https://codeclan.com",
      image = "codeclan-logo-white.png"
    )
  ),


  dashboardSidebar(
    fluidRow(
      # column(2,
             numericInput('sample_mean', label = 'Mean',
                          value = mean_init, min = 0, max = 100),
             numericInput('sample_sd', label = 'SD',
                          value = 10, min = 1, max = 20),
             numericInput('n_x', label = 'Number of observations',
                          value = 20, min = 10, max = 200),
             # numericInput('n_y', label = 'N y', value = 1e6, min = 100, max = 1e6)
      # )
      column(2,
             div(width = '200px',
             img(src = 'freeagent-brand-assets__logo--white/freeagent-logo-white.png'))
      )
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = 'cc-style-bs4.css')),

    fluidRow(
    column(6,
           plotOutput('plot1')
    ),
    column(6,
             plotOutput('plot2')
    )
    ),
    fluidRow(
             bs4ValueBoxOutput('vb_pval', width = 3),
             bs4ValueBoxOutput('vb_ci', width = 3),
             bs4ValueBoxOutput('vb_dqfun', width = 3),
             bs4ValueBoxOutput('vb_cdf', width = 3)
    )
  )
)

server <- function(input, output) {

  x <- reactive({
    res <- t.test(rnorm(input$n_x, input$sample_mean, input$sample_sd), mu = expected_mean)

    list(estimate = res$estimate,
         tval = res$statistic,
         pval = res$p.val,
         ci_lwr = res$conf.int[1],
         ci_upr = res$conf.int[2],
         qfun = qnorm(res$p.value),
         cdf = pnorm(res$p.value),
         dqfun = dnorm(qnorm(res$p.value)))
  })


  output$plot1 <- renderPlot({
    validate(need(input$sample_mean, 'Please enter a value for the mean'),
             need(input$sample_sd, 'Please enter a value for the Standard deviation'),
             need(input$n_x > 1, 'Please enter a value of 2 or more for the sample size'))

    par(mar = c(2,6,1,1), bg = pal['students1'])

    plot(1, ylim = c(0, 150), axes = FALSE, type = 'n',
         xlab = '', ylab = '', )
    axis(2, seq(0, 150, 50), las = 1, cex.axis = 2)
    abline(h = expected_mean, col = pal['partners1'], lwd = 3)
    text(.7, y = expected_mean, labels = 'Expected', col = pal['partners1'],
         cex = 1.5, pos = 3)
    text(1, y = x()$estimate, labels = 'Observed', col = pal['psd_blue'],
         cex = 1.5, pos = 4, offset = 2)
    arrows(1, x()$ci_lwr, 1, x()$ci_upr, angle = 90, code = 3, lwd = 3)
    points(1, x()$estimate, pch = 21, col = pal['psd_blue1'], bg = pal['data1'], lwd = 4, cex = 3)
  })

  output$plot2 <- renderPlot({
    req(input$sample_mean, input$sample_sd, input$n_x > 1)

    par(mar = c(3, 8, 1, 1), mfrow = c(2, 1),
        xpd = TRUE, bg = pal['students1'])

    plot(distr_data$xs, distr_data$dfun,
         las = 1, type = 'n',
         xlab = '', ylab = '',
         axes = F, cex = 4)

    mtext('Probability\ndensity\n', side = 2, line = 3,
          cex = 1.8, col = '#1b3445', outer = FALSE)

    abline(v = -4:4, col = 'grey90')
    axis(2, 0:4/10, las = 1, cex.axis = 2)

    lines(distr_data$xs, distr_data$dfun, lwd = 6, col = pal['psd_blue1'])

    points(0, dnorm(0), pch = '|', cex = 2, col = pal['psd_blue1'])

    points(x()$qfun, x()$dqfun,
           pch = 21, col = pal['psd_blue1'], bg = pal['data1'],
           lwd = 4, cex = 3)

    abline(v = qnorm(c(0.025, 0.975)), col = pal['partners1'], lwd = 2)

    segments(x()$qfun, x()$dqfun,
             x()$qfun, -10, col = pal['data1'], lwd = 2.5)

    # --------------------------------------------- #

    plot(distr_data$xs, distr_data$pfun, cex.axis = 2,
         las = 1, type = 'n', bty = 'n',
         xlab = '', ylab = '')
    # abline(v = -4:4, col = 'grey90')
    mtext('Cumulative\ndistribution\n', side = 2, line = 3,
          cex = 1.8, col = '#1b3445', outer = F)
    segments(-4:4, rep(-.05, 9), -4:4, rep(2, 9), col = 'grey90')

    lines(distr_data$xs, distr_data$pfun, lwd = 6, col = pal['psd_blue1'])

    # abline(v = qnorm(c(alpha, 1 - alpha)), col = 'pink')
    points(0, 0.5, pch = '|', cex = 2, col = pal['psd_blue1'])
    points(x()$qfun, x()$pval,
           pch = 21, col = pal['psd_blue1'], bg = pal['data1'],
           lwd = 4, cex = 3)
    segments(x()$qfun, x()$pval,
             x()$qfun, 10, col = pal['data1'], lwd = 2.5)
    segments(qnorm(c(0.025, 0.975)), -.1, qnorm(c(0.025, 0.975)), 1.2,
             col = pal['partners1'], lwd = 2)


  })

  output$vb_pval <- renderbs4ValueBox({
    req(input$sample_mean, input$sample_sd, input$n_x > 1)

    bs4ValueBox(
      value = if (x()$pval < 0.001) paste('< 0.001') else round(x()$pval, 3),
      # subtitle = 'Plausibility',
      subtitle = '',
      icon = icon('hat-wizard'),
      color = if(x()$pval < 0.05) 'success' else 'lightblue',
      footer = a('P value ', href = 'stats-definitions.html', target = 'blank')
      # href = 'plausibility.html'
    )
  })

  output$vb_ci <- renderbs4ValueBox({
    req(input$sample_mean, input$sample_sd, input$n_x > 1)

    bs4ValueBox(
      # title = 'Confidence interval',
      subtitle = '',
      value = paste(round(c(x()$ci_lwr, x()$ci_upr), 1), collapse = ' - '),
      icon = icon('arrows-alt-v'),
      color = if(x()$ci_lwr < expected_mean & x()$ci_upr > expected_mean) 'warning' else 'success',
      footer = a('Confidence interval ', href = 'stats-definitions.html', target = 'blank')
    )
  })

  output$vb_dqfun <- renderbs4ValueBox({
    req(input$sample_mean, input$sample_sd, input$n_x > 1)

    bs4ValueBox(
      value = round(x()$dqfun, 3),
      subtitle = "",
      icon = icon('info'),
      footer = a('Density function ', href = 'stats-definitions.html', target = 'blank')
    )
  })

  output$vb_cdf <- renderbs4ValueBox({
    req(input$sample_mean, input$sample_sd, input$n_x > 1)

    bs4ValueBox(
      value = round(x()$cdf, 3),
      subtitle = "",
      icon = icon('info'),
      footer = a('Distribution function ', href = 'stats-definitions.html', target = 'blank')
    )
  })
}


