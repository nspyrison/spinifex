library(spinifex)

data(flea)
flea_std <- tourr::rescale(flea[,1:6])

rb <- tourr::basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4, phi_max = 1 * pi)
sshow <- create_slideshow(data = flea_std, m_tour = mtour)
render_slideshow(slide_deck = sshow)

## other checks
tourr::is_orthonormal(rb) # good
cbind(rb,as.matrix(colnames(flea_std), ncol=1)) #good

manip_var = 4
(theta <- atan(rb[manip_var, 2] / rb[manip_var, 1]) * (360/(2*pi)) )

segments(x1=rb[manip_var, 1], y1=rb[manip_var, 2], x0=0, y0=0)


b_sldes <- sshow[[2]][sshow[[2]]$lab_abbr=="ad1",]
b_sldes$deltaV1 <- c(tail(b_sldes$V1, -1) - head(b_sldes$V1, -1), NA)
b_sldes$deltaV2 <- c(tail(b_sldes$V2, -1) - head(b_sldes$V2, -1), NA)
b_sldes$nrom <- sqrt(b_sldes$V1^2 + b_sldes$V2^2)
b_sldes

rb <- tourr::basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4, phi_max = .5 * pi)
sshow <- create_slideshow(data = flea_std, m_tour = mtour)
render_slideshow(slide_deck = sshow)

mtour <- manual_tour(basis = rb, manip_var = 4, phi_max = 2 * pi)
sshow <- create_slideshow(data = flea_std, m_tour = mtour)
render_slideshow(slide_deck = sshow)

# ## old spinifex::is_orthonormal # returning TRUE on 23/10/2018.
# mat <- rb
# mat_t <- t(mat)
# tol =.01
# ans <- all.equal(mat_t %*% mat, diag(ncol(basis)), tol = tol)
# if (ans != TRUE) {
#   message(paste0("FALSE. At tol = ", tol, " basis^t %*% basis is:"))
#   mat_t %*% mat
# } else ans

