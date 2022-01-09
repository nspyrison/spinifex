#' spinifex
#'
#' `spinifex` is a package that extends the package `tourr`. 
#' It builds the functionality for manual tours and allows other
#' tours to be rendered by `plotly` or `gganimate`. Tours are a class of
#' dynamic linear (orthogonal) projections of numeric multivariate data from 
#' `p` down to `d` dimensions that are viewed as an animation as `p`-space is 
#' rotated. Manual tours manipulate a selected variable, exploring how they 
#' contribute to the sensitivity of the structure in the projection. This is
#' particularly useful after finding an interesting basis, perhaps via a 
#' guided tour optimizing the projection for some objective function.
#' 
#' 
#' GitHub: \url{https://github.com/nspyrison/spinifex}
#' 
#' @name spinifex
#' @docType package
#' @seealso [manual_tour()] [ggtour()] [proto_default()]
NULL

## Print message -----
#### prints upon first attaching the package
.onAttach <- function(...){
  packageStartupMessage("--------------------------------------------------------")
  packageStartupMessage("spinifex --- version ", utils::packageVersion("spinifex"))
  packageStartupMessage("Please share bugs, suggestions, and feature requests at:")
  packageStartupMessage("https://github.com/nspyrison/spinifex/issues/")
  packageStartupMessage("--------------------------------------------------------")
}

## Exports ------
#' @importFrom magrittr `%>%`
#### as of v0.3.1: giving Warning upon devtools::document() and downstream:
#### Warning message: In setup_ns_exports(path, export_all, export_imports) :
## Import pipe, and some tourr functions
# #' @importFrom magrittr `%>%`
# ## tourr work functions
# #' @importFrom tourr grand_tour guided_tour holes cmass lda_pp local_tour
# #' @importFrom tourr little_tour dependence_tour basis_random basis_init
# #' @importFrom tourr sphere_data
# ## @export tourr::save_history() ## don't import made a mute wraper for it.
# ## tourr data sets
# #### Error: object 'flea' is not exported by 'namespace:tourr'
# #' @importFrom tourr flea olive ozone places ratcns laser tao flea

## Manual tour globals ------
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(
    "phi_min",
    "phi_max",
    "manip_col",
    "n_frames",
    "theta",
    "angle"))
  ## ggproto globals:
  utils::globalVariables(c(
    ".df_data",
    ".df_basis",
    ".map_to",
    ".n_frames",
    ".nrow_df_data",
    ".n",
    ".p",
    ".d",
    ".manip_var",
    "rownum_index",
    ".facet_var",
    ".is_faceted",
    "facet_var",
    ".df_data_bkg",
    ".bkg_aes_args",
    ".bkg_identity_args"))
  ## ggplot aes globals:
  utils::globalVariables(c(
    "x",
    "y",
    "z",
    "xend",
    "yend",
    "x_end",
    "y_end",
    "label",
    "..scaled..",
    "..ndensity..",
    "frame",    ## Animation frame
    "tooltip")) ## plotly tooltip
}
