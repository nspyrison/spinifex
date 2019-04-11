# 3D example from the top.
#` dontrun{
print("starting from radial_tour() and working down.")
print("Printing from /R/3D_scratchpad.R will need format into package.")
# Extended radial_tour() to 4D.

# @example
if (F)
{
  flea_std <- tourr::rescale(tourr::flea[,1:6])
  rb3 <- basis_random(n = ncol(flea_std), d = 3)
  # rad <- radial_tour(basis = rb3, manip_var = 4) # pass
  # slides <- array2df(rad, flea_std) # pass
  
  x11()
  play_radial_tour(data = flea_std, basis = rb3, manip_var = 6, 
                   col = col_of(flea$species), axes = "bottomleft")
}