install.packages("animation")
library(animation)

## set some options first
ani.options(interval = 0.2, nmax = 10)
## use a loop to create images one by one
for (i in 1:ani.options('nmax')) {
  plot(rnorm(30))
  ani.pause()   ## pause for a while ('interval')
}


##SPINIFEX:
data(flea)
flea_std <- tourr::rescale(flea[,1:6])

rb <- tourr::basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
sshow <- create_slides(tour = mtour, data = flea_std)

dat <- sshow$data_slides
bas <- sshow$bases_slides


ani.options(interval = 0.2, nmax = max(dat$slide))
for (i in 1:ani.options('nmax')) {
  da <- dat[dat$slide==i, ]
  plot(da$V1, da$V2)
  ani.pause()   ## pause for a while ('interval')
}

message("doesn't look like animation want's to work with ggplot2 though..")

ani.options(interval = 0.5, nmax = max(dat$slide))
for (i in 1:ani.options('nmax')) {
  da <- dat[dat$slide==i, ]
  (ggplot() + geom_point( aes(da$V1, da$V2)) )
  ani.pause()   ## pause for a while ('interval')
  message(i)
}


## also not keen on ggplot.
a<-list()
for (i in 1:max(dat$slide)) {
  da <- dat[dat$slide==i, ]
  a[[i]] <-(ggplot() + geom_point( aes(da$V1, da$V2)) )
  Sys.sleep(.5)
  message(i)
}
