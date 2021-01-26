### Exploring Phi issue:
library(spinifex)
library(tidyverse)
library(tictoc)

dat_pass <- data.frame(
  x1 = rnorm(100, 3, 1),
  x2 = rnorm(100, 3, 1),
  x3 = rnorm(100, 0, 1),
  x4 = rnorm(100, 0, 1)
) %>% scale_sd()
str(dat)

bas_pass <- matrix(c(1,  1,
               1, -1,
               -1, -1,
               -1, 1),
             nrow = 4, ncol = 2, byrow = TRUE) %>% 
  tourr::orthonormalise()
  
bas_pass
is_orthonormal(bas_pass)

if(F)
  ?spinifex::play_manual_tour()
{
  radial_list <- list(NULL)
  tic()
  for(i in 1:4){
    radial_list[[i]] <-
      play_manual_tour(basis = bas_pass, data = dat_pass, manip_var = i)
  }
  toc()
}
radial_list[[1]]
radial_list[[2]]
radial_list[[3]]
radial_list[[4]]
print("toy data passes, for all mvars.")

## Going back and looking for an example that in spinifex_study/www/images/
examp_nm <- "EEE_p6_0_1_rep3.rda"
fp <- paste0("../spinifex_study/apps_supplementary/data/", examp_nm)
load(fp) ## creates obj `EEE_p6_0_1_rep3`
dat_fail <- EEE_p6_0_1_rep3
str(dat_fail)
clas_fail <- attr(dat_fail, "cluster")

.ang <- seq(0, 2 * pi, length.out = 7)[-7]
bas_fail <- data.frame(x = sin(.ang),
                       y = cos(.ang)) %>% 
  as.matrix(nrow = 6, ncol = 2) %>% 
  tourr::orthonormalise()
str(bas_fail)
bas_fail_alt <- spinifex::basis_pca(tourr::flea[, -7]) ## alternative basis also fails.

play_manual_tour(basis = bas_fail, data = dat_fail, manip_var = 1)
print("this case is still wrong dispite being a full clock the error does not appear to be a function of the basis.")

manual_tour <- function(basis,
                        manip_var,
                        theta   = NULL,
                        phi_min = 0L,
                        phi_max = .5 * pi,
                        angle   = .05,
                        ...){
  ## Assumptions
  basis <- as.matrix(basis)
  if(length(manip_var) != 1L | manip_var < 1L | manip_var > nrow(basis))
    stop("manip_var expected as a single integer between 1 and nrow(basis).")
  if(spinifex::is_orthonormal(basis) == FALSE){
    warning("Basis was not orthonormal. Coereced to othronormal with tourr::orthonormalise(basis).")
    basis <- tourr::orthonormalise(basis)
  }
  ## Initialize
  p <- nrow(basis)
  d <- 2L ## d fixed to 2 atm.
  phi_start <- acos(sqrt(basis[manip_var, 1L]^2L + basis[manip_var, 2L]^2L))
  if((phi_min < phi_start) == FALSE)
    stop("Phi is currently less than phi_min, please set phi_min below ", phi_start)
  if((phi_max > phi_start) == FALSE)
    stop("Phi is currently greather than phi_max, please set phi_max above ", phi_start)
  xArgs <- list(...) ## Terminate args meant for `render_()` also passed in `play_manual_tour()`.
  if(is.null(theta))
    theta <- atan(basis[manip_var, 2L] / basis[manip_var, 1L])
  
  ## Find the values of phi for each 'leg'/walk (direction of motion)
  phi_segment  <- function(start, end){
    ## Initialize
    mvar_xsign   <- ifelse(basis[manip_var, 1L] < 0L, -1L, 1L)
    mvar_ysign   <- ifelse(basis[manip_var, 2L] < 0L, -1L, 1L)
    start_signed <- mvar_xsign * (start - phi_start)
    end_signed   <- mvar_xsign * (end   - phi_start)
    dist         <- abs(end - start)
    remainder    <- dist %% angle
    direction    <- ifelse(end_signed > start_signed, 1L, -1L)
    ## Define segments
    segment <- seq(from = start_signed, 
                   to = end_signed - remainder,
                   by = direction * angle)
    ## Add remaining partial step to the end if needed.
    if(remainder != 0L) segment <- c(segment, unname(end_signed))
    ### DEBUGGING START
    z <- function(start, end){
      a <- round(mvar_xsign * (start - phi_start) * 180/pi, 1)
      b <- round(mvar_xsign * (end   - phi_start) * 180/pi, 1)
      paste0("start: ", a, ". end: ", b)
    }
    message("theta, phi_start: ", round(theta,1), ", ", round(phi_start,1))
    message("mvar_xsign, mvar_ysign, direction: ", mvar_xsign, ", ", mvar_ysign, ", ", direction)
    message("step 1, 'to 0') ", z(phi_start, phi_max))
    message("step 2, 'to 1') ", z(phi_max,   phi_min))
    message("step 3, 'to start') ", z(phi_min, phi_start))
   #browser()
    # stop()
    ### DEBUGGING END
    ## Return
    return(segment)
  }
  
  ## Find the phi values for the animation frames
  step1 <- phi_segment(start = phi_start, end = phi_max)
  step2 <- phi_segment(start = phi_max,   end = phi_min)
  step3 <- phi_segment(start = phi_min,   end = phi_start)
  

  #roll(step1)
  phi_path <- c(phi_segment(start = phi_start, end = phi_max),
                phi_segment(start = phi_max,   end = phi_min),
                phi_segment(start = phi_min,   end = phi_start))
  browser()
  phi_path <- step1
  ## Make projected basis array
  n_frames <- length(phi_path)
  m_sp <- create_manip_space(basis = basis, manip_var = manip_var)
  tour_array <- array(NA, dim = c(p, d, n_frames))
  mute_for <- sapply(1L:n_frames, function(i){ ## Vectorization of for, does behave slightly diff than for loop.
    thisProj <- rotate_manip_space(manip_space = m_sp, theta = theta, phi = -phi_path[i])
    ## adding - to phi path worked, but why do we not see consistency in checks then?, when -? when +?
    tour_array[,, i] <<- thisProj[, 1L:2L]
    phi <- acos(sqrt(thisProj[1, 1L]^2L + thisProj[1, 2L]^2L))
    message("i, phi_degrees: ", i, ", ", round(phi*180/pi,1))
    print(view_frame(thisProj[,1:2]))
    Sys.sleep(.2)
  })
  attr(tour_array, "manip_var") <- manip_var
  
  return(tour_array)
}
if(F)
  play_manual_tour(basis = bas_fail, data = dat_fail, manip_var = 1,
                    aes_args = list(color = clas_fail, shape = clas_fail))
mt_fail <- manual_tour(basis = bas_fail, manip_var = 1)
for(i in 1:length(mt_fail)){
  phi <- acos(sqrt(mt_fail[1, 1L, i]^2L + mt_fail[1, 2L, i]^2L))
  message("i, phi_degrees: ", i, ", ", round(phi*180/pi,1))
  
}
## fail case:
# theta, test_theta, phi_start: 1.6, 1.6, 1
# mvar_xsign, mvar_ysign, direction: 1, 1, -1
# step 1, 'to 0') start: 0. end: -54.7
# step 2, 'to 1') start: -54.7. end: 35.3
# step 3, 'to start') start: 35.3. end: 0
mt_fail_alt <- manual_tour(basis = bas_fail_alt, manip_var = 1)
## fail ALT case:
# theta, test_theta, phi_start: -0.3, 0.3, 0.2
# mvar_xsign, mvar_ysign, direction: -1, 1, 1
# step 1, 'to 0') start: 0. end: 9.7
# step 2, 'to 1') start: 9.7. end: -80.3
# step 3, 'to start') start: -80.3. end: 0
mt_pass <- manual_tour(basis = bas_pass, manip_var = 1)
## pass case:
# theta, test_theta, phi_start: 0.8, 0.8, 0.8
# mvar_xsign, mvar_ysign, direction: 1, 1, -1
# step 1, 'to 0') start: 0. end: -45
# step 2, 'to 1') start: -45. end: 45
# step 3, 'to start') start: 45. end: 0