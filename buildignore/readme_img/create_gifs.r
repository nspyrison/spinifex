### Create gifs for readme
if(F)
  ?ggtour

library(spinifex)
dat           <- scale_sd(penguins_na.rm[, 1:4])
colnames(dat) <- c("bl", "bd", "fl", "bm")
clas          <- penguins_na.rm$species
gt_path       <- save_history(dat, tour_path = grand_tour(), max_bases = 8)

ggt <- ggtour(basis_array = gt_path, angle = .15) +
  proto_default(aes_args = list(color = clas, shape = clas),
                identity_args = list(alpha = .8))
anim <- animate_gganimate(ggt, fps = 4, height = 6, width = 10, 
                          units = "cm", ## "px", "in", "cm", or "mm."
                          res = 200)

gganimate::anim_save("penguins_gt.gif",
                     animation = anim,
                     path = "./buildignore/readme_img/")

bas     <- basis_olda(dat, clas)
mt_path <- manual_tour(bas, 1, data = dat)

ggt <- ggtour(basis_array = mt_path, angle = .15) +
  proto_default(aes_args = list(color = clas, shape = clas),
                identity_args = list(alpha = .8))
anim <- animate_gganimate(ggt, fps = 8, height = 6, width = 10, 
                          units = "cm", ## "px", "in", "cm", or "mm."
                          res = 200)

gganimate::anim_save("penguins_mt.gif",
                     animation = anim,
                     path = "./buildignore/readme_img/")
