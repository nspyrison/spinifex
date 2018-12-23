#scratchpadd (gg)animate

# as of: 23/12/2018:
install.packages("animate") # not in 3.5.1
install.packages("gganimate") # not in 3.5.1
# MAKE SURE TO RESTART AFTER DL STRINGI AND RLANG.
devtools::install_github("thomasp85/gganimate", dependancies=T)  

### Try from the below example. ?animate not working for me 23/12/2018.
# from:https://github.com/thomasp85/gganimate
library(ggplot2)
library(gganimate)

ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
  


# Now with the spinifex:
  data(flea)
  flea_std <- tourr::rescale(flea[,1:6])
  
  rb <- tourr::basis_random(n = ncol(flea_std))
  mtour <- manual_tour(basis = rb, manip_var = 4)
  sshow <- create_slides(tour = mtour, data = flea_std)
  
  
  dat <- sshow$data_slides
  bas <- sshow$bases_slides
  
  gga1 <- 
    ggplot(dat, aes(V1, V2)) + 
    geom_point() + 
    transition_states(
      slide,
      transition_length = 2,
      state_length = 1
    ) +
    enter_fade() + 
    exit_shrink() +
    ease_aes('sine-in-out')
  #gga1
  
  # bare bones
  gg <- 
    ggplot(dat) + 
    geom_point(aes(V1, V2)) 
  
  gg + geom_segment(data = bas, aes(V1, V2, xend=0, yend=0)) +
    transition_states(
      slide,
      transition_length = 0,
      state_length = 1
    )
  
  