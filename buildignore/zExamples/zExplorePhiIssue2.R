## Base workflow ----
library(spinifex)
dat     <- scale_sd(penguins[, 1:4])
clas    <- penguins$species
bas     <- basis_pca(dat)
mt_path <- manual_tour(basis = bas, manip_var = 2, data = dat)

ggt <- ggtour(mt_path, angle = .3) +
  proto_basis()
  #proto_default(aes_args = list(color = clas, shape = clas))
animate_plotly(ggt)

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


## Case 1: penguins----

mt_path_ls <- list(NULL)
for(i in 1:ncol(dat)){ ## MAKE LIST
  mt_path_ls[[i]] <- manual_tour(basis = bas, manip_var = i, data = dat)
}
length(mt_path_ls)

#not just -x in the manip var; penguins, pca mv=2




## Case 2: toy_class----
require(cheem)
toy_class_ls <- readRDS(
  "../cheem/inst/shiny_apps/cheem_initial/data/2preprocess_toy_classification.rds")
dat <- toy_class_ls$decode_df[, colnames(toy_class_ls$attr_df)]
bas <- cheem::basis_attr_df(toy_class_ls$attr_df, 18)
bas_comp <- cheem::basis_attr_df(toy_class_ls$attr_df, 111)
.diff <- abs(bas - bas_comp)
mv <- which(.diff == max(.diff)) #~2
## Good; expected message;
#phi_start > pi / 2; phi_start <- phi_start - pi & phi_max <- -phi_max
## 1
mt_path <- manual_tour(basis = bas, manip_var = 1, data = dat)

ggt <- ggtour(mt_path, angle = .3) +
  proto_basis1d()
animate_plotly(ggt)


debugonce(spinifex:::interpolate_manual_tour)
interp <- spinifex:::interpolate_manual_tour(.mt_path, angle =.3)




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