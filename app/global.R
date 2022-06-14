# library(shiny)
# library(bs4Dash)

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
mean_init <- sample(seq(90, 120, 5), 1)

res <- t.test(rnorm(100))
p <- res$p.value
alpha <- 0.05 / 2


xs <- seq(-4, 4, l = 100)
distr_data <- list(
  xs = xs, dfun = dnorm(xs), pfun = pnorm(xs)
)

# Modules -----------------------------------------------------------------


valueBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4ValueBoxOutput(ns('pval'), width = 3),
      bs4ValueBoxOutput(ns('ci'), width = 3),
      bs4ValueBoxOutput(ns('dqfun'), width = 3),
      bs4ValueBoxOutput(ns('cdf'), width = 3)
    )
  )
}


valueBoxServer <- function(id, x, sample_mean, sample_sd, n_x) {
  moduleServer(id,
               function(input, output, session) {
                 output$pval <- renderbs4ValueBox({
                   bs4ValueBox(
                     value = if (x()$pval < 0.001) paste('< 0.001') else round(x()$pval, 3),
                     subtitle = '',
                     icon = icon('hat-wizard'),
                     color = if(x()$pval < 0.05) 'success' else 'lightblue',
                     footer = a('P value ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })

                 output$ci <- renderbs4ValueBox({
                   footer_colour <- if (x()$ci_lwr > expected_mean) {
                     'success'
                   } else if (x()$ci_upr < expected_mean) {
                     'danger'
                   } else {
                     'warning'
                   }

                   bs4ValueBox(
                     subtitle = '',
                     value = paste(round(c(x()$ci_lwr, x()$ci_upr), 1), collapse = ' - '),
                     icon = icon('arrows-alt-v'),
                     color = footer_colour,
                     footer = a('Confidence interval ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })

                 output$dqfun <- renderbs4ValueBox({
                   bs4ValueBox(
                     value = round(x()$dqfun, 3),
                     subtitle = "",
                     icon = icon('info'),
                     footer = a('Density function ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })

                 output$cdf <- renderbs4ValueBox({
                   bs4ValueBox(
                     value = round(x()$cdf, 3),
                     subtitle = "",
                     icon = icon('info'),
                     footer = a('Distribution function ', href = 'stats-definitions.html', target = 'blank')
                   )
                 })
               }
  )
}
