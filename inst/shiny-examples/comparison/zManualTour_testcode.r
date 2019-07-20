#test code
library(spinifex)
#spinifex::run_app()

?play_manual_tour
flea_std <- tourr::rescale(tourr::flea[,1:6])
rb <- tourr::basis_random(n = ncol(flea_std))


play_manual_tour(data = flea_std, basis = rb, manip_var = 6,
                 col = col_of(flea$species), axes = "bottomleft",
                 theta = 0)

#basically want a plot_frame(basis, mvar, theta, phi) as a ggplot.
##opening play_radial_tour: manual_tour > array2df > render; look at manual tour

