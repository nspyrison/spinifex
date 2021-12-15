### Exploring Phi issue:
library(spinifex)

## NA) correct, -x -----
dat_std <- scale_sd(wine[, 2:6])
clas <- wine$Type
bas <- basis_pca(dat_std)
mv <- manip_var_of(bas, rank = 3)
bas <- -1 * bas
## Animating with ggtour() & proto_*
mt <- manual_tour(basis = bas, manip_var = 5)
ggt <- ggtour(mt, dat_std, angle = .1) +
    proto_basis()
animate_plotly(ggt)

## 3) incorrect, mv_x = 0 -----
## Going back and looking for an example that in spinifex_study/www/images/
examp_nm <- "EEE_p6_0_1_rep3.rda"
fp <- paste0("../spinifex_study/apps_supplementary/data/", examp_nm)
load(fp) ## creates obj `EEE_p6_0_1_rep3`
dat_fail <- EEE_p6_0_1_rep3
str(dat_fail)
clas_fail <- attr(dat_fail, "cluster")
bas_fail <- basis_half_circle(dat_fail)

## Check x = 0, y = 1
mt_path <- manual_tour(basis = bas_fail, manip_var = 1)
ggt <- ggtour(mt_path, data = dat_fail, angle = .2) + 
  proto_basis()
animate_plotly(ggt)

## 4) correct, -1*bas -----
## check x = 0, y = -1
mt_path <- manual_tour(basis = -bas_fail, manip_var = 1)
ggt <- ggtour(mt_path, data = dat_fail, angle = .2) +
  proto_basis()
animate_plotly(ggt)


