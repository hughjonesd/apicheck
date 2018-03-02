## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(huxtable)

is_latex <- guess_knitr_output_format() == 'latex'
knitr::knit_hooks$set(
  barrier = function(before, options, envir) {
    if (! before && is_latex) knitr::asis_output('\\FloatBarrier')
  }
)

if (is_latex) knitr::opts_chunk$set(barrier = TRUE)

## ------------------------------------------------------------------------
data(diamonds, package = 'ggplot2')

lm1 <- lm(price ~ carat + depth, diamonds)
lm2 <- lm(price ~ depth + factor(color, ordered = FALSE), diamonds)
lm3 <- lm(log(price) ~ carat + depth, diamonds)

## ------------------------------------------------------------------------

huxreg(lm1, lm2, lm3)

## ------------------------------------------------------------------------
color_names <- paste0('factor(color, ordered = FALSE)', LETTERS[5:10])
names(color_names) <- paste('Color:', LETTERS[5:10])

huxreg(lm1, lm2, lm3, coefs = c('Carat' = 'carat', 'Depth' = 'depth', color_names))

## ------------------------------------------------------------------------
diamond_regs <- huxreg(lm1, lm2, lm3)
diamond_regs[seq(8, 18, 2), 1] <- paste('Color:', LETTERS[5:10])
diamond_regs

## ------------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
diamond_regs                                                         %>% 
      theme_article                                                  %>% 
      set_background_color(1:nrow(diamond_regs), evens, grey(.95)) %>% 
      set_font_size(final(), 1, 9)                                   %>% 
      set_bold(final(), 1, FALSE)                                    %>%
      set_top_border(final(), 1, 1)                                  %>%
      set_caption('Linear regressions of diamond prices')


## ------------------------------------------------------------------------
huxreg(lm1, lm3, error_pos = 'right')

## ------------------------------------------------------------------------
huxreg(lm1, lm3, error_pos = 'same')

## ------------------------------------------------------------------------
huxreg('Price' = lm1, 'Log price' = lm3)

## ------------------------------------------------------------------------
broom::glance(lm1)
huxreg(lm1, lm3, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'F statistic' = 'statistic',
  'P value' = 'p.value'))

## ------------------------------------------------------------------------
huxreg(lm1, lm3, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01)) # a little boastful?
huxreg(lm1, lm3, stars = NULL) 

## ------------------------------------------------------------------------
huxreg(lm1, lm3, error_format = '({statistic})')
huxreg(lm1, lm3, error_format = '({p.value})')

## ------------------------------------------------------------------------
huxreg(lm1, lm3, ci_level = .99, error_format = '{conf.low} to {conf.high}')

## ------------------------------------------------------------------------
huxreg(lm1, lm3, note = 'Linear regressions on diamond price. {stars}.')

## ------------------------------------------------------------------------
huxreg(lm1, lm3, number_format = 2)

## ------------------------------------------------------------------------
huxreg(lm1, lm3, bold_signif = 0.05)

