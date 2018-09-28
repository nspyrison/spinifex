data(diamonds)
dat <- as_tibble(diamonds[1:1000, 1:6])

# class(dat$carat)
# is.numeric(dat$cut)
# a <- as.integer(1)
# class(a)
# is.numeric(a)

# class_vec <- apply(dat, 2, class)
# is_numeric_vec <- apply(class_vec, 1, is.numeric)

is_numeric_vec <- 
  apply(dat, 2, function(col) class(col) )#%in% c("numeric", "integer")) #doesn't recognize numeric as such...

