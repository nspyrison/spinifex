## saving a img (.png)
library(spinifex)

std_flea <- tourr::rescale(flea[,1:6])
rb <- basis_random(ncol(std_flea))

oblique_frame(std_flea, rb, 4, 0, 0)
ggplot2::ggsave("myTestImg.png")

rv_gg <- oblique_frame(std_flea, rb, 4, 0, 0)
ggplot2::ggsave("myTestImg.png", rv_gg)


## saving a anim (.gif)
play_radial_tour(data = flea_std, basis = rb, manip_var = 6, 
                 render_type = render_gganimate, 
                 col = col_of(flea$species), 
                 pch = pch_of(flea$species), axes = "bottomleft")

gganimate::anim_save("myAnim.gif")

rv_anim <- play_radial_tour(flea_std, rb, 6, render_gganimate, 
                            col_of(flea$species), pch_of(flea$species))

gganimate::anim_save("myAnim.gif", rv_anim)

