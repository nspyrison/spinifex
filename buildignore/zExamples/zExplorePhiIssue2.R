library(spinifex);
library(tourr)
dat = tourr::flea[, 1:6];
bas = basis_pca(dat);
sw_hist <- stepwise_path(basis = bas, data = dat)
set <- sw_hist$basis_set
str(set)

# debugonce(interpolate)
# tourr::interpolate(set) ## now interpolate(tour()) complains not orthonormal 

gg_ls <- list(NULL)
for(i in dim(set)[3]:1){ ## MAKE LIST\
  print(spinifex::is_orthonormal(set[,,i]))
  tourr::is_orthonormal(set[,,i])
  gg_ls[[i]] <- view_manip_space(set[,,i], i)
}

for(i in dim(set)[3]:1){ ## VIEW LIST
  print(gg_ls[[i]])
  Sys.sleep(.5)
}



# from <- bas[[i]]
# to <- bas[[i - 1]]
# 
# tourr::interpolate(bas)
# dim(interpolate(t1, 0.05))