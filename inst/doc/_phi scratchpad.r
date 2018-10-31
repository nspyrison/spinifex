data(flea)
flea_std <- tourr::rescale(flea[,1:6])

rb <- tourr::basis_random(n=ncol(flea_std))
  phi_start <- acos(sqrt(rb[4, 1]^2 + rb[4, 2]^2))
mtour <- manual_tour(basis = rb, manip_var = 4,
                     phi_min = -1*phi_start/(.5*pi),
                     phi_max = ((pi*.5) - phi_start)/(.5*pi)
)

sshow <- create_slideshow(data = flea_std, m_tour = mtour)
render_slideshow(slide_deck = sshow)
