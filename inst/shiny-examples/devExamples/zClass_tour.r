### Gap tour 12/02/2020
#Nicholas Spyrison
library(spinifex)
library(ggplot2)

dat <- tourr::rescale(tourr::flea[, 1:6])
if(F) 
  verbose <- T


#levels(tourr::flea$species) # Note: class names are odd.
class_tour <- function(data = tourr::rescale(tourr::flea[, 1:6]), 
                       basis = prcomp(data)$rotation[, 1:2], 
                       class = tourr::flea$species, 
                       level_nums = c(1, 2),
                       top_n_var = 3,
){
  stopifnot(is.matrix(data) & is.numeric(data))
  stopifnot(is.matrix(basis) & is.numeric(basis))
  stopifnot(is_orthonormal(basis))


  if(length(which(class == levels(class)[level_nums])) < nrow(data) / 10){
    n <- nrow(data)
    obs_in_lvls <- length(which(class == levels(class)[level_nums]))
    warning(paste0("Number of obervations (", obs_in_lvls ,
                   ") is less than 10% of data rows (", n,")."))
  }
    
  # init 
  supervised_dat <- data.frame(data, class = class)
  dat_lda <- MASS::lda(class~., data = supervised_dat)
  lvl_means <- dat_lda$means
  
  lvl_abs_mean_diff <- abs(lvl_means[level_nums[1], ] - lvl_means[level_nums[2], ])
  if (verbose) cat("this is the direction (linear combination of var) of class difference \n")
  if (verbose) print(lvl_abs_mean_diff)
  lvl_var_rank <- rank(-rank(lvl_abs_mean_diff))
  if (verbose) cat("this is the order of var to add")
  if (verbose) print(lvl_var_rank)
  
  mod_nums <- NULL
  #i <- 1
  for (i in 1:top_n){
    mod_nums <- c(mod_nums, which(lvl_var_rank == i))
    if (verbose) cat("this is the manip_var_numbers \n")
    # WILL NEED TO HANDLE MULTIPLE SELECTIONS 
    oblique_frame(basis, data, mod_nums)
  
  }
  
avg_lin_comb <- function(basis = prcomp(data)$rotation[, 1:2],
                         var_nums = 1:2){
  stopifnot(is.matrix(basis) & is.numeric(basis))
  stopifnot(is_orthonormal(basis))
  
  start <- basis
  replaced <- basis[var_nums, ]
  for(i in 1:ncol(basis)){
    basis[var_nums, i] <- sqrt(sum(basis[, i])) / length(var_nums)
  }
  view_basis()
  view_basis(basis)
  view_basis(orthonormalise(basis))
  
}

