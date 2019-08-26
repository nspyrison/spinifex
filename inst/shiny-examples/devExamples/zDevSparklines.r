# devtools::install_github("rstudio/gt")
library(tidyverse)
#> Warning: package 'tibble' was built under R version 3.5.2
library(gt)
library(purrr)

# make a function for creating a plot
# of a group
plot_group <- function(name, df) {
  plot_object <-
    ggplot(data = df,
           aes(x = hp, y = trq,
               size = msrp)) +
    geom_point(color = "blue") +
    theme(legend.position = "none")
  return(plot_object)
}

# make a plot of each mfr
head(gtcars) %>%
  group_by(mfr) %>%
  nest() %>%
  mutate(plot = map2(mfr, data, plot_group)) %>%
  select(-data) %>% 
  # Create empty column (a placeholder for the images)
  mutate(ggplot = NA) ->
  tibble_plot

# Minor changes to this code
tibble_plot %>%
  gt() %>%
  text_transform(
    locations = cells_data(columns = vars(ggplot)), # use empty cell as location
    fn = function(x) {
      # Insert each image into each empty cell in `ggplot`
      map(.$plot, ggplot_image, height = px(200))
    }
  )


## NS:
# devtools::install_github("rstudio/gt")
library(tidyverse)
#> Warning: package 'tibble' was built under R version 3.5.2
library(gt)
library(purrr)
library(spinifex)
flea_std <- tourr::rescale(tourr::flea[, 1:6])

rb <- basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
sshow <- array2df(array = mtour, data = flea_std)

dat <- sshow$data_slides[1:100,]

# make a function for creating a plot
# of a group
plot_group <- function(name, df) {
  plot_object <-
    ggplot(data = df,
           aes(x = V1, y = V2),
           size=1) +
    geom_point(color = "blue") +
    theme(legend.position = "none")
  return(plot_object)
}

# make a plot of each mfr
dat %>%
  group_by(slide) %>%
  nest() %>%
  mutate(plot = map2(slide, data, plot_group)) %>%
  select(-data) %>% 
  # Create empty column (a placeholder for the images)
  mutate(ggplot = NA) ->
  tibble_plot


# Minor changes to this code
tibble_plot %>%
  gt() %>%
  text_transform(
    locations = cells_data(columns = vars(ggplot)), # use empty cell as location
    fn = function(x) {
      # Insert each image into each empty cell in `ggplot`
      map(.$plot, ggplot_image, height = px(200))
    }
  )



