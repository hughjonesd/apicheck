## ----setup, echo = FALSE, warning = FALSE, message = FALSE---------------

library(knitr)
library(dplyr)
library(huxtable)

is_latex <- guess_knitr_output_format() == 'latex'
# is_latex <- TRUE
knitr::knit_hooks$set(
  barrier = function(before, options, envir) {
    if (! before && is_latex) knitr::asis_output('\\FloatBarrier')
  }
)

if (is_latex) knitr::opts_chunk$set(barrier = TRUE)


## ---- echo = FALSE-------------------------------------------------------
huxtable::hux_logo(latex = is_latex)

## ---- eval = FALSE, echo = FALSE-----------------------------------------
#  # PLAN
#  # Make single document work in notebook format (maybe with minimal changes)
#  # Installation
#  # Dplyr examples
#  # Examples with where and friends
#  # Different kinds of output
#  # Cookbook?
#  # Limitations
#  

## ---- eval = FALSE-------------------------------------------------------
#  install.packages('huxtable')

## ------------------------------------------------------------------------
library(huxtable)
ht <- hux(
        Employee     = c('John Smith', 'Jane Doe', 'David Hugh-Jones'), 
        Salary       = c(50000, 50000, 40000),
        add_colnames = TRUE
      )

## ------------------------------------------------------------------------
data(mtcars)
car_ht <- as_hux(mtcars)

## ---- results = 'markup'-------------------------------------------------
print_screen(ht)

## ------------------------------------------------------------------------
ht

## ------------------------------------------------------------------------
right_padding(ht) <- 10
left_padding(ht)  <- 10

## ------------------------------------------------------------------------
bold(ht)[1,]          <- TRUE
bottom_border(ht)[1,] <- 1

## ------------------------------------------------------------------------
align(ht)[,2] <- 'right'

## ------------------------------------------------------------------------
align(ht)[,'Salary'] <- 'right'

## ------------------------------------------------------------------------
ht

## ------------------------------------------------------------------------

caption(ht) <- 'Employee table'
ht


## ---- echo = FALSE-------------------------------------------------------
sides <- c('left_', 'right_', 'top_', 'bottom_')
props <- list()
props[['Cell_Text']] <- sort(c('font', 'text_color', 'wrap', 'bold', 'italic', 'font', 'font_size', 'na_string', 'pad_decimal', 'escape_contents', 'number_format', 'rotation'))

props[['Cell']] <- sort(c('align', 'valign', 'rowspan', 'colspan', 'background_color', paste0(sides, 'border'),
      paste0(sides, 'border_color'), paste0(sides, 'padding')))
props[['Row']]    <- 'row_height'
props[['Column']] <- 'col_width'
props[['Table']]  <- sort(c('width', 'height', 'position', 'caption', 'caption_pos', 'tabular_environment', 'label', 'latex_float'))

maxl <- max(sapply(props, length))
props <- lapply(props, function(x) c(x, rep('', maxl - length(x))))

ss_font <- if (guess_knitr_output_format() == 'latex') 'cmhv' else 'arial'

prop_hux <- hux(as.data.frame(props))                     %>% 
      add_colnames                                        %>% 
      {foo <- .; foo[1,] <- gsub('_', ' ', foo[1,]); foo} %>% 
      set_font(-1, everywhere, ss_font)                   %>% 
      set_bold(1, everywhere, TRUE)                       %>% 
      set_width(0.9)                                      %>% 
      set_background_color(everywhere, evens, grey(.9))   %>% 
      set_left_border(everywhere, 1, 1)                   %>% 
      set_right_border(everywhere, final(), 1)            %>% 
      set_top_border(1, everywhere, 1)                    %>% 
      set_bottom_border(1, everywhere, 1)                 %>% 
      set_bottom_border(final(), everywhere, 1)           %>% 
      set_top_padding(2)                                  %>% 
      set_bottom_padding(4)                               %>% 
      set_caption('Huxtable properties')                  %>% 
      set_position('left') %>% 
      set_col_width(c(.2, .25, .15, .15, .25))

prop_hux

## ------------------------------------------------------------------------

library(dplyr)
hux(
        Employee     = c('John Smith', 'Jane Doe', 'David Hugh-Jones'), 
        Salary       = c(50000, 50000, 40000),
        add_colnames = TRUE
      )                               %>% 
      set_bold(1, 1:2, TRUE)          %>% 
      set_bottom_border(1, 1:2, 1)    %>%
      set_align(1:4, 2, 'right')      %>%
      set_right_padding(10)           %>%
      set_left_padding(10)            %>% 
      set_caption('Employee table')
  


## ---- results = 'markup'-------------------------------------------------
italic(ht)
position(ht)

## ---- results = 'markup'-------------------------------------------------
bottom_border(ht)[1:2,]
bold(ht)[,'Salary']

## ------------------------------------------------------------------------
# Select columns by name:
cars_mpg <- car_ht[, c('mpg', 'cyl', 'am')] 
# Order by number of cylinders:
cars_mpg <- cars_mpg[order(cars_mpg$cyl),]

cars_mpg <- cars_mpg                          %>% 
      huxtable::add_rownames(colname = 'Car') %>% 
      huxtable::add_colnames()

cars_mpg[1:5,]

## ------------------------------------------------------------------------
car_ht <- car_ht                                          %>%
      huxtable::add_rownames(colname = 'Car')             %>%
      slice(1:10)                                         %>% 
      select(Car, mpg, cyl, hp)                           %>% 
      arrange(hp)                                         %>% 
      filter(cyl > 4)                                     %>% 
      rename(MPG = mpg, Cylinders = cyl, Horsepower = hp) %>% 
      mutate(kml = MPG/2.82)                               


car_ht <- car_ht                               %>% 
      set_number_format(1:7, 'kml', 2)         %>% 
      set_col_width(c(.35, .15, .15, .15, .2)) %>% 
      set_width(.6)                            %>% 
      huxtable::add_colnames() 

car_ht

## ------------------------------------------------------------------------
ht <- insert_row(ht, 'Hadley Wickham', '100000', after = 3)
ht <- add_footnote(ht, 'DHJ deserves a pay rise')
ht

## ------------------------------------------------------------------------
pointy_ht <- hux(c('Do not pad this.', 11.003, 300, 12.02, '12.1 **'))
pointy_ht <- set_all_borders(pointy_ht, 1)
width(pointy_ht) <- .2

number_format(pointy_ht) <- 3
pointy_ht

## ------------------------------------------------------------------------
pad_decimal(pointy_ht)[2:5, ] <- '.' # not the first row
align(pointy_ht) <- 'right'
pointy_ht

## ------------------------------------------------------------------------
code_ht <- if (is_latex) hux('Here is some maths: $a^b$') else 
      hux('The code for a copyright symbol is &copy;')
code_ht

## ------------------------------------------------------------------------
escape_contents(code_ht)[1, 1] <- FALSE
code_ht

## ------------------------------------------------------------------------
width(ht) <- 0.35
col_width(ht) <- c(.7, .3)
ht

## ------------------------------------------------------------------------
ht[5, 1] <- 'David Arthur Shrimpton Hugh-Jones'
ht 

## ------------------------------------------------------------------------
ht_wrapped <- ht
wrap(ht_wrapped) <- TRUE
ht_wrapped

## ------------------------------------------------------------------------
as_hux(mtcars[1:4, 1:4])                           %>% 
      huxtable::add_rownames(colname = 'Car name') %>% 
      huxtable::add_colnames()

## ------------------------------------------------------------------------
cars_mpg <- cbind(car_type = rep("", nrow(cars_mpg)), cars_mpg)
cars_mpg$car_type[1] <- 'Four cylinders'
cars_mpg$car_type[13] <- 'Six cylinders'
cars_mpg$car_type[20] <- 'Eight cylinders'
rowspan(cars_mpg)[1, 1] <- 12
rowspan(cars_mpg)[13, 1] <- 7
rowspan(cars_mpg)[20, 1] <- 14

cars_mpg <- rbind(c('', 'List of cars', '', '', ''), cars_mpg)
colspan(cars_mpg)[1, 2] <- 4
align(cars_mpg)[1, 2] <- 'center'

# a little more formatting:

cars_mpg <- set_all_padding(cars_mpg, 2)
cars_mpg <- set_all_borders(cars_mpg, 1)
valign(cars_mpg)[1,] <- 'top'
col_width(cars_mpg) <- c(.4 , .3 , .1, .1, .1)
number_format(cars_mpg)[, 4:5] <- 0
bold(cars_mpg)[1:2, ] <- TRUE
bold(cars_mpg)[, 1] <- TRUE
if (is_latex) font_size(cars_mpg) <- 10
cars_mpg

## ------------------------------------------------------------------------
theme_striped(cars_mpg[14:20,], stripe = 'bisque1', header_col = FALSE, header_row = FALSE)

## ------------------------------------------------------------------------
car_ht                                                 %>% 
      set_background_color(evens, everywhere, 'wheat') %>% 
      set_background_color(odds, everywhere, grey(.9)) %>% 
      set_bold(1, everywhere, TRUE)

## ------------------------------------------------------------------------
library(psych)
data(attitude)
att_corr <- corr.test(as.matrix(attitude))

att_hux <- as_hux(att_corr$r)                                           %>% 
      # selects cells with p < 0.05:
      set_background_color(where(att_corr$p < 0.05), 'yellow')          %>% 
      # selects cells with p < 0.01:
      set_background_color(where(att_corr$p < 0.01), 'orange')          %>% 
      set_text_color(where(row(att_corr$r) == col(att_corr$r)), 'grey') 


att_hux <- att_hux                                                      %>% 
      huxtable::add_rownames()                                          %>% 
      huxtable::add_colnames()                                          %>%
      set_caption('Correlations in attitudes among 30 departments')     %>% 
      set_bold(1, everywhere, TRUE)                                     %>% 
      set_bold(everywhere, 1, TRUE)                                     %>% 
      set_all_borders(1)                                                %>%
      set_number_format(2)                                              %>% 
      set_position('left')

att_hux


## ------------------------------------------------------------------------
m <- matrix(c('dog', 'cat', 'dog', 'dog', 'cat', 'cat', 'cat', 'dog'), 4, 2)
m
where(m == 'dog') # m is equal to 'dog' in cells (1, 1), (3, 1), (4, 1) and (4, 2):

## ------------------------------------------------------------------------

color_demo <- matrix('text', 7, 7)
rainbow <- c('red', 'orange', 'yellow', 'green', 'blue', 'turquoise', 'violet')
color_demo <- as_hux(color_demo)                  %>% 
      set_text_color(rainbow)                     %>% # text rainbow goes down columns
      set_background_color(rainbow, byrow = TRUE) %>% # background color rainbow goes along rows
      set_all_borders(1)                          %>% 
      set_all_border_colors('white')
color_demo


## ------------------------------------------------------------------------
data(diamonds, package = 'ggplot2')

lm1 <- lm(price ~ carat, diamonds)
lm2 <- lm(price ~ depth, diamonds)
lm3 <- lm(price ~ carat + depth, diamonds)

huxreg(lm1, lm2, lm3)

## ---- results = 'markup'-------------------------------------------------
print_screen(ht)

