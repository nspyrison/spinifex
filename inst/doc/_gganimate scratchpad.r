#scratchpadd (gg)animate

# as of: 23/12/2018:
install.packages("animate") # not in 3.5.1
install.packages("gganimate") # not in 3.5.1
# MAKE SURE TO RESTART AFTER DL STRINGI AND RLANG.
devtools::install_github("thomasp85/gganimate", dependancies=T)  

### Try from the below example. ?animate not workign for me 23/12/2018.
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
