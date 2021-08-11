### Exploring Phi issue:
library(spinifex)
library(tidyverse)
library(tictoc)

## Going back and looking for an example that in spinifex_study/www/images/
examp_nm <- "EEE_p6_0_1_rep3.rda"
fp <- paste0("../spinifex_study/apps_supplementary/data/", examp_nm)
load(fp) ## creates obj `EEE_p6_0_1_rep3`
dat_fail <- EEE_p6_0_1_rep3
str(dat_fail)
clas_fail <- attr(dat_fail, "cluster")

bas_fail <- basis_half_circle(dat_fail)
str(bas_fail)

## Old api.
play_manual_tour(basis = bas_fail, data = dat_fail, manip_var = 1) ## Fails here, why not blue? goes past...
bas_fail_alt <- spinifex::basis_pca(tourr::flea[, -7])
play_manual_tour(basis = bas_fail_alt, data = dat_fail, manip_var = 1) ## Works here.

## New api
mt_path <- manual_tour(basis = bas_fail, manip_var = 1)
ggt <- ggtour(mt_path, data = dat_fail) + proto_default()
animate_plotly(ggt)
