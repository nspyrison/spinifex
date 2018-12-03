### Phys Example
#load("data/PhysDat_6PC.rda")
load("data/fGT_6d.rda")
phys.end <- matrix(as.numeric(fGT_6d[,,dim(fGT_6d)[3]]),ncol=2)

col <- PhysDat$disID
pch <- PhysDat$disID

