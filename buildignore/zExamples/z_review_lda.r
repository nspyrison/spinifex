library(Rdimtools); library(spinifex); library(tourr);
dat  <- tourr::rescale((wine[,2:14]))
clas <- wine$Type

m_lda <- MASS::lda(dat, grouping = clas)
m_lda_bas <- m_lda$scaling
o_m_lda_bas <- tourr::orthonormalise(m_lda_bas)
lda  <- do.lda(dat, clas)
lda_bas <- lda$projection
olda <- do.olda(dat, clas)
olda_bas <- olda$projection
odp <-do.odp(dat, clas)
odp_bas <- odp$projection
lda_pp_bas <- basis_guided(data = wine[, 2:14], index_f = tourr::holes())
animate(dat, guided_tour(lda_pp(clas)), display_xy())


spinifex::is_orthonormal(m_lda_bas)
spinifex::is_orthonormal(o_m_lda_bas)
spinifex::is_orthonormal(lda_bas)
spinifex::is_orthonormal(olda_bas)
spinifex::is_orthonormal(odp_bas)
spinifex::is_orthonormal(lda_pp_bas)

oblique_frame(basis = o_m_lda_bas, data = dat, manip_var = 1)
oblique_frame(basis = lda_bas, data = dat, manip_var = 1)
oblique_frame(basis = olda_bas, data = dat, manip_var = 1)
oblique_frame(basis = odp_bas, data = dat, manip_var = 1)
oblique_frame(basis = lda_pp_bas, data = dat, manip_var = 1)

