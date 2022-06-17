library(shiny)
library(bs4Dash)

# Values / data -----------------------------------------------------------

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
mean_init <- 100#sample(seq(90, 120, 5), 1)

res <- t.test(rnorm(100))
p <- res$p.value
alpha <- 0.05 / 2


xs <- seq(-4, 4, l = 100)
distr_data <- list(
  xs = xs, dfun = dnorm(xs), pfun = pnorm(xs)
)


# Init --------------------------------------------------------------------

input_init <- list(inputId = c('n_x', 'sample_mean', 'sample_sd'),
                   label   = c('Number of observations', 'Average value', 'Spread of values'),
                   value   = c(2, mean_init, 20),
                   min     = c(10, 0, 10),
                   max     = c(200, 150, 20))


# Carousel ----------------------------------------------------------------

# carousel(indicators = FALSE,
#          id = "logoCarousel",
#          carouselItem(
#            img(src = 'freeagent-brand-assets__logo--white/freeagent-logo-white.png', width = 180)),
#          carouselItem(
#            p('Logos of other partners that we deliver this to?')
#          ),
#          carouselItem(
#            img(src = 'codeclan-logo-white.png',
#                href = 'https://codeclan.com/courses/data-courses/', width = 180))
# )
carouselUI <- function() {
  logo_paths <- dir('www', patt = 'png$')
# tagList(
  carousel(indicators = FALSE,
           id = "logoCarousel",
           # lapply(c(logo_paths[3:2]), \(.) carouselItem(img(src = ., width = 180)))
           # purrr::map(c(logo_paths[3:2]), ~carouselItem(img(src = ., width = 180)))
  )
  # )
}

# Modules -----------------------------------------------------------------


# Diff plot ------------------------------------------------------

diffPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns('diff_plot'))
  )
}

diffPlotServer <- function(id, x, pal, input) {
  moduleServer(
    id,
    function(input, output, session) {
      output$diff_plot <- renderPlot({
        validate(need(input$sample_mean, 'Please enter a value for the mean.'),
                 need(input$sample_sd >= 1, 'Enter a spread of at leat 1.'),
                 need(input$n_x >= 2, 'The sample size must be at least 2.'))

        par(mar = c(2,6,1,1), bg = pal['students1'])

        plot(1, ylim = c(0, 150), axes = FALSE, type = 'n',
             xlab = '', ylab = '')
        axis(2, seq(0, 150, 50), las = 1, cex.axis = 2)
        abline(h = expected_mean, col = pal['partners1'], lwd = 3)
        text(.7, y = expected_mean, labels = 'Expected', col = pal['partners1'],
             cex = 1.5, pos = 3)
        text(1, y = x()$estimate, labels = 'Observed', col = pal['psd_blue'],
             cex = 1.5, pos = 4, offset = 2)
        arrows(1, x()$ci_lwr, 1, x()$ci_upr, angle = 90, code = 3, lwd = 3)
        points(1, x()$estimate, pch = 21, col = pal['psd_blue1'], bg = pal['data1'], lwd = 4, cex = 3)
      })
    })
}

# Distribution plot ------------------------------------------------------

distrPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns('alpha'), 'Set Confidence level',
                 choiceNames = paste(c(99, 95, 90), '%'),
                 choiceValues = 1-c(0.01, 0.05, 0.1)),
    plotOutput(ns('distr_plot'))
  )
}

distrPlotServer <- function(id, x, distr_data, sig, pal) {
  moduleServer(
    id,
    function(input, output, session) {
      output$distr_plot <- renderPlot({
        sig_threshold <- qnorm((1 - as.numeric(sig)) / 2)

        par(mar = c(3, 8, 1, 1), mfrow = c(2, 1),
            xpd = TRUE, bg = pal['students1'])

        plot(distr_data$xs, distr_data$dfun,
             las = 1, type = 'n',
             xlab = '', ylab = '',
             axes = FALSE, cex = 4)

        mtext('Probability\ndensity\n', side = 2, line = 3,
              cex = 1.8, col = pal['psd_blue1'], outer = FALSE)

        abline(v = -4:4, col = 'grey90')
        axis(2, 0:4/10, las = 1, cex.axis = 2)

        lines(distr_data$xs, distr_data$dfun, lwd = 6, col = pal['psd_blue1'])
        points(0, dnorm(0), pch = '|', cex = 2, col = pal['psd_blue1'])

        abline(v = sig_threshold, col = pal['partners1'], lwd = 3)

        points(x()$qfun, x()$dqfun,
               pch = 21, col = pal['psd_blue1'], bg = pal['data1'],
               lwd = 4, cex = 3)
        segments(x()$qfun, x()$dqfun,
                 x()$qfun, -10, col = pal['data1'], lwd = 2.5)

        # --------------------------------------------- #

        plot(distr_data$xs, distr_data$pfun, cex.axis = 2,
             las = 1, type = 'n', bty = 'n',
             xlab = '', ylab = '')
        mtext('Cumulative\ndistribution\n', side = 2, line = 3,
              cex = 1.8, col = pal['psd_blue1'], outer = F)
        segments(-4:4, rep(-.05, 9), -4:4, rep(2, 9), col = 'grey90')

        lines(distr_data$xs, distr_data$pfun, lwd = 6, col = pal['psd_blue1'])
        points(0, 0.5, pch = '|', cex = 2, col = pal['psd_blue1'])

        segments(sig_threshold, -.1, sig_threshold, 1.2,
                 col = pal['partners1'], lwd = 3)

        points(x()$qfun, x()$pval,
               pch = 21, col = pal['psd_blue1'], bg = pal['data1'],
               lwd = 4, cex = 3)
        segments(x()$qfun, x()$pval,
                 x()$qfun, 10, col = pal['data1'], lwd = 2.5)


      })
    }
  )
}


# Valueboxes --------------------------------------------------------------

valueBoxUI <- function(id) {
  tagList(
    fluidRow(
      purrr::map(NS(id, c('pval', 'ci', 'dqfun', 'cdf')), width = 3, valueBoxOutput)
    )
  )
}


valueBoxServer <- function(id, x) {
  moduleServer(id,
               function(input, output, session) {
                 output$pval <- renderValueBox({
                   valueBox(
                     value = if (x()$pval < 0.001) paste('< 0.001') else round(x()$pval, 3),
                     subtitle = '',
                     icon = icon('hat-wizard'),
                     color = if(x()$pval < 0.05) 'success' else 'lightblue',
                     footer = a('P value ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })

                 output$ci <- renderValueBox({
                   footer_colour <- if (x()$ci_lwr > expected_mean) {
                     'success'
                   } else if (x()$ci_upr < expected_mean) {
                     'danger'
                   } else {
                     'warning'
                   }

                   valueBox(
                     subtitle = '',
                     value = paste(round(c(x()$ci_lwr, x()$ci_upr), 1), collapse = ' - '),
                     icon = icon('arrows-alt-v'),
                     color = footer_colour,
                     footer = a('Confidence interval ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })

                 output$dqfun <- renderValueBox({
                   valueBox(
                     value = round(x()$dqfun, 3),
                     subtitle = "",
                     icon = icon('info'),
                     footer = a('Probability density ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })

                 output$cdf <- renderValueBox({
                   valueBox(
                     value = round(x()$cdf, 3),
                     subtitle = "",
                     icon = icon('info'),
                     footer = a('Cumulative distribution ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })
               }
  )
}


plot_base <- function(xs, ys, xax = FALSE, yax, ylab) {
  plot(xs, ys, cex.axis = 2, axes = FALSE,
       las = 1, type = 'n', bty = 'n',
       xlab = '', ylab = '')

  if (xax) {
    xlabs <- seq(min(xs), max(xs), 1)
    axis(1, xlabs, cex.axis = 2)
  }

  axis(2, yax, las = 1, cex.axis = 2)

  mtext(ylab, side = 2, line = 3,
        cex = 1.8, col = pal['psd_blue1'], outer = FALSE)

}


add_p_curve <- function(d, type = c('d', 'p')) {
  type <- match.arg(type)

  if (type == 'd') {
    y <- dnorm(0)
    ys <- d$dfun
  } else {
    y <- 0.5
    ys <- d$pfun
  }
  lines(d$xs, ys, lwd = 6, col = pal['psd_blue1'])
  points(0, y, pch = '|', cex = 2, col = pal['psd_blue1'])
}

add_prob_lines <- function(x, y, y2) {
  if (x > -4) {
    points(x, y,
           pch = 21, col = pal['psd_blue1'], bg = pal['data1'],
           lwd = 4, cex = 3)
    segments(x, y,
             x, y2, col = pal['data1'], lwd = 2.5)
  }
}
