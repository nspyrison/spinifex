## reprex ----
require(spinifex)
?ggtour

dat     <- scale_sd(penguins[, 1:4])
clas    <- penguins$species
bas     <- basis_pca(dat)
mv      <- manip_var_of(bas)
mt_path <- manual_tour(bas, manip_var = mv)

## d = 2 case
debugonce(proto_point)
ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
  proto_default(aes_args = list(color = clas, shape = clas),
                identity_args = list(size = 1.5, alpha = .8))
# Error: Aesthetics must be either length 1 or the same as the data (56): colour and size
animate_plotly(ggt)

## test different protos ----
## same
ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
  proto_default()
animate_plotly(ggt)

##
ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
  proto_origin()
## Error in p$x$layout$sliders[[which(isAniSlider)]] <- modify_list(p$x$layout$sliders[[which(isAniSlider)]],  : 
#attempt to select less than one element in OneIndex 
animate_plotly(ggt)

## does data change the number? -----
dat     <- scale_sd(flea[, 1:4])
clas    <- flea$species
bas     <- basis_pca(dat)
mv      <- manip_var_of(bas)
mt_path <- manual_tour(bas, manip_var = mv)

ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
  proto_default(aes_args = list(color = clas, shape = clas),
                identity_args = list(size = 1.5, alpha = .8))
# Warning messages:
# 1: In FUN(X[[i]], ...) :
#   .lapply_rep_len: `color` not of length 1 or data; liable to cause cycling issues. Should it be of length 1 or data?
animate_plotly(ggt)

##  Roll back changes???? ------
