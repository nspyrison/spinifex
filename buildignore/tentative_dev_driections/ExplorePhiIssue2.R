library(spinifex)
## The message is in manual_tour not interpolate_manual_tour
# if(phi_start > pi / 2L){
#   message("phi_start > pi / 2; phi_start <- phi_start - pi & phi_max <- -phi_max")
#   phi_start <- phi_start - pi
#   phi_max   <- phi_max - pi
# }
# if(phi_start < -pi / 2L){
#   message("phi_start < -pi / 2; phi_start <- phi_start + pi")
#   phi_start <- phi_start + pi
# }


## 1) correct, mv_x is negative----
dat     <- scale_sd(penguins[, 1:4])
clas    <- penguins$species
bas     <- basis_pca(dat)
mt_path <- manual_tour(basis = bas, manip_var = 2, data = dat)

ggt <- ggtour(mt_path, angle = .3) +
  proto_basis()
animate_plotly(ggt)


## 2) incorrect, mt message, -x -----

require(cheem)
toy_class_ls <- readRDS(
  "../cheem/inst/shiny_apps/cheem_initial/data/2preprocess_toy_classification.rds")
dat <- toy_class_ls$decode_df[, colnames(toy_class_ls$attr_df)]
bas <- cheem::basis_attr_df(toy_class_ls$attr_df, 18)
bas_comp <- cheem::basis_attr_df(toy_class_ls$attr_df, 111)
.diff <- abs(bas - bas_comp)
mv <- which(.diff == max(.diff)) #~2

mt_path <- manual_tour(basis = bas, manip_var = 1, data = dat)
ggt <- ggtour(mt_path, angle = .3) +
  proto_basis1d()
animate_plotly(ggt)

# debugonce(view_manip_space)
# view_manip_space(bas, 1) ## doesn't work for 1d basis.


## ~2.5~ and it's -1*bas, works ----
mt_path <- manual_tour(basis = -1 *bas, manip_var = 1, data = dat)
ggt <- ggtour(mt_path, angle = .3) +
  proto_basis1d()
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




## iterating trough interpolate_manual_tour ----

# debugonce(interpolate)
# tourr::interpolate(set) ## now interpolate(tour()) complains not orthonormal 

mt_path_ls <- list(NULL)
for(i in 1:ncol(dat)){ ## MAKE LIST
  mt_path_ls[[i]] <- manual_tour(basis = bas, manip_var = i, data = dat)
}
length(mt_path_ls)


# for(i in dim(set)[3]:1){ ## VIEW LIST
#   print(gg_ls[[i]])
#   Sys.sleep(.5)
# }



# from <- bas[[i]]
# to <- bas[[i - 1]]
# 
# tourr::interpolate(bas)
# dim(interpolate(t1, 0.05))