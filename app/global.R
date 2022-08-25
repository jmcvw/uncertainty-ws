library(shiny)
library(bs4Dash)

# Init Values / data -----------------------------------------------------------

ccpal <- c(students1  = '#38b0e3',
           students2  = '#50a3cd',
           students3  = '#e8f3f9',
           partners1  = '#fad350',
           data1      = '#e9415e',
           psd_blue1  = '#1b3445',
           partners2  = '#e3c375',
           data2      = '#fef0f2',
           psd_blue2  = '#0f252f')

mean_init <- 100#sample(seq(90, 120, 5), 1)

input_init <- list(inputId = c('sample_mean', 'n_x', 'sample_sd'),
                   label   = c('Average value', 'Number of sales', 'Spread of values'),
                   value   = c(mean_init, 5, 20),
                   min     = c(10, 5, 10),
                   max     = c(120, 1000, 50))
code <- setNames(c('c573', 'c6df', 'b67b', 'f120',
                   'd344','a3d2', 'cd5b', '9f3a', 'bd15'),
                 c(96:104)) # just md5 hash of value

# Modules -----------------------------------------------------------------

# UI module ---------------------------------------------------------------

inputUI <- function(id) {
  ns <- NS(id)
  input_init$inputId <- ns(input_init$inputId)

  tagList(
    fluidRow(
      actionButton(ns('sample_mean_btn'), 'Change customer base'),
      purrr::pmap(input_init, numericInput)[-1],
      actionButton(ns('update_btn'), 'New sample'),

      checkboxGroupInput(ns('show_data'), 'Show...', choices = c('Margin of error', 'Sales')),

      radioButtons(ns('alpha'), 'Set margin of error',
                   choiceNames = paste(c(99, 95, 90), '%'),
                   choiceValues = 1-c(0.01, 0.05, 0.1), selected = 1-0.05),
    )

  )
}

inputServer <- function(id) {
  moduleServer(
    id, function(input, output, session) {
      list(
        cg  = reactive({ input$sample_mean_btn }),
        nx  = reactive({ input$n_x }),
        smn = reactive({ input$sample_mean }),
        ssd = reactive({ input$sample_sd }),
        al  = reactive({ input$alpha }),
        ss  = reactive({ input$show_data }),
        btn = reactive({ input$update_btn })
      )
    })
}

# Diff plot ------------------------------------------------------

diffPlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns('difference_plot'))
}

diffPlotServer <- function(id, sdata, res, i) {
  moduleServer(
    id, function(input, output, session) {
      output$difference_plot <- renderPlot({
        validate(

          need(i$nx() >= 2, 'There must be at least 2 observations.'),
          # need(i$smn(), 'Enter a value for the average.'),
          need(i$ssd() >= 1, 'Enter a value greater than 1 for the spread.')
        )

        par(mar = c(2,6,1,1), bg = ccpal['students1'])

        plot(1, ylim = c(0, 150), axes = FALSE, type = 'n',
             xlab = '', ylab = '')
        axis(2, seq(0, 150, 50), las = 1, cex.axis = 2)

        abline(h = mean_init, col = ccpal['partners1'], lwd = 3)
        text(.7, y = mean_init, labels = 'Expected',
             col = ccpal['partners1'],
             cex = 1.5, pos = 3)
        text(1, y = res()$estimate,
             labels = (paste('Observed:\n', round(res()$estimate, 2))),
             col = ccpal['psd_blue'], cex = 1.5,
             pos = 4, offset = 2)

        if ('Margin of error' %in% i$ss())
          arrows(1, res()$ci_lwr, 1, res()$ci_upr, angle = 90,
                 code = 3, lwd = 3)

        if ('Sales' %in% i$ss()) {
          set.seed(Sys.Date())
          j <- jitter(rep(.9, i$nx()))
          set.seed(NULL)

          points(j, sdata(), pch = 20, col = ccpal['data1'], cex = 1)
        }

        points(1, res()$estimate, pch = 21,
               col = ccpal['psd_blue1'], bg = ccpal['data1'],
               lwd = 4, cex = 3)

      })
    })
}

# Distribution plots ------------------------------------------------------

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
        cex = 1.8, col = ccpal['psd_blue1'], outer = FALSE)

}

add_p_curve <- function(d, type = c('d', 'p'), df) {
  type <- match.arg(type)

  if (type == 'd') {
    y <- dt(0, df)
    ys <- d$dfun
  } else {
    y <- 0.5
    ys <- d$pfun
  }
  lines(d$xs, ys, lwd = 6, col = ccpal['psd_blue1'])
  points(0, y, pch = '|', cex = 2, col = ccpal['psd_blue1'])
}

add_prob_lines <- function(x, y, y2) {
  if (x > -4) {
    points(x, y,
           pch = 21, col = ccpal['psd_blue1'], bg = ccpal['data1'],
           lwd = 4, cex = 3)
    segments(x, y,
             x, y2, col = ccpal['data1'], lwd = 2.5)
  }
}

distrPlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns('distr_plot'))
}


distrPlotServer <- function(id, distr_data, res) {
  moduleServer(
    id, function(input, output, session) {
      output$distr_plot <- renderPlot({

        sig_threshold <- qt(res()$alpha / 2, res()$df)
        sig_threshold <- c(sig_threshold, -sig_threshold)

        par(mar = c(3, 8, 1, 1), mfrow = c(2, 1),
            xpd = TRUE, bg = ccpal['students1'])

        plot_base(distr_data()$xs, distr_data()$pfun, FALSE,
                  seq(0, 1, .25), 'Cumulative\ndistribution\n')
        abline(v = -4:4, col = 'grey90')
        add_p_curve(distr_data(), 'p', res()$df)
        abline(v = sig_threshold, col = ccpal['partners1'], lwd = 3)
        add_prob_lines(res()$tval, res()$pfun, -10)

        # --------------------------------------------- #

        plot_base(distr_data()$xs, distr_data()$dfun, TRUE,
                  0:4/10, 'Probability\ndensity\n')
        segments(-4:4, rep(-.01, 9), -4:4, rep(.5, 9), col = 'grey90')
        add_p_curve(distr_data(), 'd', res()$df)
        segments(sig_threshold, -.01, sig_threshold, .5,
                 col = ccpal['partners1'], lwd = 3)
        add_prob_lines(res()$tval, res()$dfun, 10)

      })
    })
}


# Valueboxes module --------------------------------------------------------------

valueBoxUI <- function(id) {
  fluidRow(
    purrr::map(NS(id, c('pval', 'ci', 'cdf', 'dfun')), width = 3, valueBoxOutput)
  )
}


valueBoxServer <- function(id, x, ci_is_ticked) {
  moduleServer(id,
               function(input, output, session) {
                 output$pval <- renderValueBox({
                   valueBox(
                     value = if (x()$pval < 0.001) paste('< 0.001') else round(x()$pval, 3),
                     subtitle = '',
                     icon = icon('hat-wizard', verify_fa = FALSE),
                     color = if(x()$pval < x()$alpha) 'success' else 'lightblue',
                     footer = a('P value ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })

                 output$ci <- renderValueBox({

                   val <- if ('Margin of error' %in% ci_is_ticked$ss()) {
                     value <- paste(round(c(x()$ci_lwr, x()$ci_upr), 1), collapse = ' - ')
                     footer_colour <- if (x()$ci_lwr > mean_init) {
                       'success'
                     } else if (x()$ci_upr < mean_init) {
                       'danger'
                     } else {
                       'warning'
                     }
                     list(value = value, footer_colour = footer_colour)
                   } else {
                     value <- '???'
                     list(value = value, footer_colour = 'warning')

                   }



                   valueBox(
                     subtitle = '',
                     value = val$value,
                     icon = icon('arrows-alt-v', verify_fa = FALSE),
                     color = val$footer_colour,
                     footer = a('Confidence interval ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })

                 output$cdf <- renderValueBox({
                   valueBox(
                     value = if (x()$pfun < 0.001) '< 0.001' else
                       if (x()$pfun > .999) '> 0.999' else
                         round(x()$pfun, 3),
                     subtitle = "",
                     icon = icon('info', verify_fa = FALSE),
                     footer = a('Cumulative distribution ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })

                 output$dfun <- renderValueBox({
                   valueBox(
                     value = if (x()$dfun < 0.001) '< 0.001' else
                       if (x()$dfun > 0.999) '> 0.999' else
                         round(x()$dfun, 3),
                     subtitle = "",
                     icon = icon('info', verify_fa = FALSE),
                     footer = a('Probability density ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })

               }
  )
}


# Accordion module -------------------------------------------------------

accordionUI <- function(id) {
  ns <- NS(id)
  column(11,
         accordion(
           id = "accordion_data_summary",
           accordionItem(
             title = "Data summary",
             status = "lightblue",
             collapsed = TRUE,
             fluidRow(
               column(6,
                      p(strong('Data distribution')),
                      plotOutput(ns('data_hist')),
                      textOutput(ns('truth'))
               ),
               column(2, offset = 1,
                      p(strong('Five number summary')),
                      tableOutput(ns('data_5num'))
               ),
               column(2, offset = 1,
                      p(strong('The data (max 100 values)')),
                      tableOutput(ns('data_table'))
               )
             )
           )
         )
  )

}

accordionServer <- function(id, sample_data, ri, truth) {

  moduleServer(id,
               function(input, output, session) {

                 output$data_5num <- renderTable({
                   s <- summary(sample_data())
                   as.matrix(s)
                 }, rownames = TRUE, colnames = FALSE, digits = 2, hover = TRUE)


                 output$data_hist <- renderPlot({

                   par(mar = c(5, 4, 1, 1))

                   x <- seq(min(sample_data()), max(sample_data()), l = 100)

                   hist(sample_data(), freq = FALSE,
                        main = '', xlab = 'Sample values')
                   mtext(c('Theoretical distribution', 'Empirical distribution'),
                         1, 2:3, adj = 1, col = c(4, 2))
                   lines(density(sample_data()), col = 2)
                   curve(dnorm(x, truth(), ri$ssd()), col = 4, type = 'l', add = TRUE)
                   curve(dt(x, length(sample_data()) - 1),
                         col = 3, type = 'l', add = TRUE)

                 })

                 output$truth <- renderText({

                   ans <- code[as.character(truth())]
                   paste('Actual sample mean: ', ans)
                 })

                 output$data_table <- renderTable({
                   sort(sample_data())[seq_len(min(length(sample_data()), 100))]
                 }, colnames = FALSE, digits = 2, hover = TRUE)
               })
}
