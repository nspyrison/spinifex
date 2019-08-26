# Initialize ----
library(spinifex)
#?oblique_frame

flea_std  <- tourr::rescale(tourr::flea[,1:6])
basis     <- tourr::basis_random(n = ncol(flea_std))
manip_var <- 4

m_sp  <- create_manip_space(basis, manip_var)
mv_sp <- m_sp[manip_var,]
print(oblique_frame(data = flea_std, basis, manip_var, 0, 0))

### radius (magnitude) motion,  ----
theta <- atan(mv_sp[2] / mv_sp[1])
phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))

print(oblique_frame(data = flea_std, basis, manip_var, 0, 0)) #baseline
phi_seq <- c(0, -phi_start*.33, -phi_start*.66, -phi_start, 
             (pi/2-phi_start)*.33, (pi/2-phi_start)*.66, pi/2-phi_start)
phi_seq <- phi_seq * -sign(mv_sp[1])
for (i in phi_seq){
  print(oblique_frame(data = flea_std, basis, manip_var, theta, i))
  print(i)
  Sys.sleep(1.5)
}
# radius(phi)|theta; theta = atan(mv_sp[2] / mv_sp[1])
# radius[0,1], phi[-phi_start, pi/2-phi_start]
# r = cos(phi+phi_start)
# phi = acos(r) - phi_start
(r_seq <- cos(phi_seq+phi_start)) #radius: start, 1, 0

### angular motion ----
theta_start <- atan(mv_sp[2] / mv_sp[1])
theta_i <- theta_start + pi/2
phi <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))

print(oblique_frame(data = flea_std, basis, manip_var,0,0)) #baseline
theta_seq <- seq(0,10,by=.5)#+ theta_start 
for (i in theta_seq) {
  print(oblique_frame(data = flea_std, basis, manip_var, i, -phi)) 
  print(i)
  Sys.sleep(1.5)
}

for (i in theta_seq){
  .bas <- oblique_basis(basis, manip_var, i, -phi)
  .mv_sp <- .bas[manip_var, ]
  .phi <- acos(sqrt(.mv_sp[1]^2 + .mv_sp[2]^2))
  print(view_manip_space(.bas, manip_var, labels = colnames(flea_std)))
  print(.phi)
  Sys.sleep(1.5)
}


