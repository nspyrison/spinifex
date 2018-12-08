#' Render display of a manual tour of the passed data
#'
#' A wrapper function for manual_tour(), create_slides(), and 
#' render_slideshow(). Allows the user to go from data to plotly object of 
#' manual tour in one function. For use with other tour paths see play_tour().
#' 
#' @param data A [n, p] dim data to project, consisting of 
#'   only numeric variables (for coercion into matrix).
#' @param basis A [p, d] dim orthonormal numeric matrix. If it's left null, a
#'   random basis will be created.
#' @param manip_var Integer column number or string exact column name of the.
#'   variable to manipulate. Required, no default.
#' @param manip_type String of the type of manipulation to use. 
#'   Defaults to "radial". Alternatively accepts "horizontal" or "vertical". 
#'   Yields to `theta` if set. Must set either `manip_type` or `theta`.
#' @param theta Angle in radians of "in-plane" rotation, on the XY plane of the 
#'   reference frame. Typically set from manip_type in proj_data(). Supersedes 
#'   `manip_type`. Must set either `manip_type` or `theta`.
#' @param phi_min Minimun value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the referce frame. 
#'   Required, defaults to 0.
#' @param phi_max Maximum value phi should move to. Phi is angle in radians of 
#'   the "out-of-plane" rotation, the z-axis of the referce frame. 
#'   Required, defaults to 2 * pi.
#' @param n_slides The number of slide-interpolations to make for the tour.
#' @param disp_type The graphics system to use. Defaults to 'plotly'.
#' @param col String of the colo(u)r to highlight the `manip_var`.
#' @param init_rescale_data When TRUE will apply tourr::rescale() on the data.
#'   Defaults to FALSE.
#' @param init_rand_basis When TRUE create a random basis with 
#'   tourr::basis_random(). Yields to basis, defaults to FALSE.
#' @param ... Optionally pass addition arguments to the `disp_type` options.
#' @return An animation in `disp_type` graphics of the interpolated data and 
#'   the corrisponding reference frame.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' 
#' (pss <- spinifex(data = flea_std, basis = rb, manip_var = 4))
#' 
#' (pss2 <- spinifex(data = flea[,1:6], manip_var = 4,
#'                   init_rescale_data = TRUE, init_rand_basis = TRUE))
spinifex <- function(data,
                     basis      = NULL,
                     manip_var,
                     manip_type = "radial",
                     theta      = NULL,     # [radians]
                     phi_min    = 0,        # [radians]
                     phi_max    = .5 * pi,  # [radians]
                     n_slides   = 20,
                     disp_type  = "plotly",
                     col        = "blue",   # colo(u)r of manip_var
                     init_rescale_data = FALSE,
                     init_rand_basis   = FALSE,
                     ...
) {
  if (init_rescale_data) data <- tourr::rescale(data)
  if (!is.null(basis) & init_rand_basis) message("Non null basis passed with init_rand_basis = TRUE. Using the passed basis.")
  if (is.null(basis) & init_rand_basis) 
    basis <- tourr::basis_random(n = ncol(data))
  
  manual_tour <- manual_tour(basis = basis, manip_var = manip_var, 
                             manip_type = manip_type, theta = theta, 
                             phi_min = phi_min, phi_max = phi_max, 
                             n_slides = n_slides)
  
  slides <- create_slides(tour = manual_tour, data = data)
  
  slideshow <- render_slideshow(slides = slides, disp_type = disp_type, 
                                col = col, ...)
  
  return(slideshow)
}