require(ggplot2)

g <- ggplot(mtcars, aes(x=disp, y=mpg, color=factor(cyl), shape=factor(cyl)), alpha= .9) +
  geom_point(data = mtcars)
names(g)
names(g$data)    ## needs subset
names(g$facet)   ## potentially subset
names(g$mapping) ## will need to be resized here
## where would identity args go?

require(spinifex)
dat  <- scale_sd(penguins_na.rm[, 1:4])
clas <- penguins_na.rm$species
bas  <- basis_pca(dat)
mv   <- manip_var_of(bas)

## d = 2 case
debugonce(proto_point)
mt_path <- manual_tour(bas, manip_var = mv)
ggt <- ggtour(mt_path, dat, angle = .3) +
  proto_point(list(color = clas, shape = clas),
              list(size = 1.5)) +
  proto_basis()
fs <- filmstrip(ggt)
names(fs$data)    ## needs subset
names(fs$mapping)   ## potentially subset
names(fs$mapping) ## will need to be resized here
