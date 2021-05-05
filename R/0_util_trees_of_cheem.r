#' ## 0_util_trees_of_cheem
#' 
#' 
#' #' Scree table for local attribution
#' #' 
#' #' Create an ordered scree table (df) for the local attribution from a 
#' #' DALEX::predict_parts return object. Also see iBreakDown:::print.break_down_uncertainty.
#' #'
#' #' @param x A return from DALEX::predict_parts().
#' ### NOT @export
#' #' @examples
#' #' ## Setup
#' #' dat_std <- scale_sd(tourr::flea[, 1:6])
#' #' clas <- tourr::flea[, 7]
#' #' t1 <- tourr::save_history(dat_std, max = 3)
#' #' ls_args <- list(col = clas, shape = clas)
#' #' tgt_len <- nrow(dat_std) * dim(t1)[3]
#' #' 
#' #' spinifex:::rep_arg_ls(ls_args, tgt_len)
#' ### Remade from: iBreakDown:::print.break_down_uncertainty
#' df_scree_local_attr <- function(x){ ## x should be a predict_parts() return
#'   ret <- data.frame(
#'     label = tapply(x$label, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
#'     variable_name = tapply(x$variable_name, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
#'     variable_value = tapply(x$variable_value, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE), ## oos variable value
#'     median_local_attr = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), median, na.rm = TRUE)
#'   )
#'   ## Reorder
#'   ret <- ret[order(abs(ret$median_local_attr), decreasing = TRUE), ]
#'   ## Add cumsum_rate
#'   ret$cumsum_rate_abs_median_local_attr <-
#'     cumsum(abs(ret$median_local_attr)) / sum(abs(ret$median_local_attr))
#'   return(ret) ## NOTE: order is in desc magnitude not order of the colnames.
#' }
#' 
#' #' The basis from the SHAP values & the first component of a selected 
#' #' basis function
#' #' 
#' #' The basis from the SHAP values of of a randomForest model with respect to a 
#' #' new observation. The second vector is the first component the basis generated
#' #' from `basis_type`. The result is then orthonormalized.
#' #' 
#' #' @param data Numeric matrix or data.frame of the observations.
#' #' @param class Vector of classification of observations.
#' #' @param new_observation Numeric matrix or data.frame of a new obersevation to
#' #' find the local attribution of.
#' #' @param prediction_class_level the level of the class the random forest 
#' #' predicts for.
#' #' @param parts_type The `type` of variable attribution, 
#' #' expects: "shap", "oscillations", "oscillations_uni", "oscillations_emp", 
#' #' "break_down" or "break_down_interactions".
#' #' @param parts_B #TODO:XXX CONTINUE HERE after meeting with Przemyslaw
#' #' @param parts_N
#' #' 
#' #' 
#' #' @return An orthonormal, numeric matrix basis
#' #' @export
#' #' @family basis identifiers
#' #' @examples
#' #' scaled_full <- spinifex::scale_sd(flea[, 1:6])
#' #' dat <- scaled_full[-10, 1:6]
#' #' oos_obs <- scaled_full[10,, drop = FALSE] ## drop = FALSE retains data.frame rather than coerce to vector.
#' #' clas <- flea$species[-10]
#' #' oos_clas <- flea$species[10]
#' #' 
#' #' basis_cheem(dat, clas, oos_obs, oos_clas)
#' #' 
#' #' basis_cheem(dat, clas, oos_obs, oos_clas,
#' #'             parts_type = "break_down", parts_B = 15, parts_N = 100, basis_type = "pca")
#' basis_cheem <- function(data, class, new_observation, prediction_class_level,
#'                         parts_type = "shap", 
#'                         parts_B = 10, 
#'                         parts_N = if (substr(type, 1, 4) == "osci") 500 else NULL, 
#'                         basis_type = "olda", 
#'                         ...){
#'   if(basis_type != "olda") message("olda is the only basis implemented.")
#'   class <- as.factor(class)
#'   prediction_class_level <- as.factor(prediction_class_level)
#'   new_observation <- as.data.frame(new_observation)
#'   .clas_test <- class == prediction_class_level
#'   .rf <- randomForest::randomForest(.clas_test~., data = data.frame(data, .clas_test))
#'   .ex_rf <- DALEX::explain(model = .rf,
#'                            data = data,
#'                            y = .clas_test,
#'                            label = "Random Forest")
#'   
#'   .parts <- DALEX::predict_parts(explainer = .ex_rf, ## ~ 10 s @ B=25
#'                                  new_observation = new_observation,
#'                                  type = parts_type,
#'                                  N = parts_N,
#'                                  B = parts_B,
#'                                  ...)
#'   
#'   .scree_la <- df_scree_local_attr(.parts) ## 1 shap for each class :/...
#'   ## TODO: Does aggregating across class make sense? do we need to generalize to multi class.
#'   
#'   .bas <- switch(basis_type,
#'                  olda = basis_olda(data, class),
#'                  pca  = basis_pca(data),
#'                  odp  = basis_odp(data, class),
#'                  onpp = basis_onpp(data), ## Hardcoded defaults
#'                  stop("basis_type expects 'olda', 'pca', 'odp' or 'onpp'.")
#'   )
#'   
#'   ## reorder scree table back to data colname order
#'   .r_idx <- order(match(.scree_la$variable_name, rownames(.bas)))
#'   .scree_la <- .scree_la[.r_idx,]
#'   
#'   .cbind <- cbind(.scree_la$median_local_attr, .bas)[, 1L:2L]
#'   .cheem_bas <- as.matrix(tourr::orthonormalise(.cbind))
#'   colnames(.cheem_bas) <- .cn <- c(parts_type, paste0(basis_type, "1"))
#'   
#'   #attr(.cheem_bas, "class") <- c("matrix", "cheem basis")
#'   attr(.cheem_bas, "data") <- as.matrix(data)
#'   attr(.cheem_bas, "data_class") <- class
#'   attr(.cheem_bas, "new_observation") <- as.matrix(new_observation)
#'   attr(.cheem_bas, "new_observation_class") <- new_observation_class
#'   attr(.cheem_bas, "predict_parts") <- .parts
#'   
#'   return(.cheem_bas)
#' }
#' 
#' #' @example
#' #' scaled_full <- spinifex::scale_sd(flea[, 1:6])
#' #' dat <- scaled_full[-10, 1:6]
#' #' oos_obs <- scaled_full[10,, drop = FALSE] ## drop = FALSE retains data.frame rather than coerce to vector.
#' #' clas <- flea$species[-10]
#' #' oos_clas <- flea$species[10]
#' #' bas_cheem <- basis_cheem(dat, clas, oos_obs, oos_clas)
#' #' 
#' #' view_cheem(bas_cheem)
#' #' 
#' #' view_cheem(bas_cheem, show_boxplots = FALSE, max_features = 4)
#' #' 
#' #' if(F)
#' #'   ggsave("PoC_view_cheem.pdf", device ="pdf", width = 6, height = 3, units="in")
#' view_cheem <- function(cheem_basis, show_parts = TRUE, ...){
#'   .data <- attributes(cheem_basis)$data
#'   .class <- attributes(cheem_basis)$data_class
#'   .new_obs <- attributes(cheem_basis)$new_observation
#'   .new_obs_class <- attributes(cheem_basis)$new_observation_class
#'   .cn <- colnames(cheem_basis)
#'   
#'   .proj_new_obs <- data.frame(.new_obs %*% cheem_basis)
#'   
#'   gg <- view_frame(cheem_basis, data = .data,
#'                    aes_args = list(color = .class, shape = .class),
#'                    axes = "left") +
#'     theme(legend.position = "off", 
#'           axis.title = element_text()) +
#'     labs(x = paste0(.cn[1L], ", normalized local attr"),
#'          y = .cn[2L]) + 
#'     geom_point(aes_string(x = .cn[1L], y = .cn[2L]), 
#'                .proj_new_obs, color = "red", size = 5L, shape = 8L)
#'   
#'   if(show_parts == TRUE){
#'     require("cowplot")
#'     .parts <- attributes(cheem_basis)$predict_parts
#'     gg <- cowplot::plot_grid(gg, plot(.parts, ...), nrow = 1L, rel_widths = c(1.2, 1L))
#'   }
#'   return(gg)
#' }
#' 
#' #' @examples 
#' #' ## Setup
#' #' dat_std <- tourr::rescale(wine[, 2:14])
#' #' clas <- wine$Type
#' #' bas <- basis_pca(dat_std)
#' #' mv <- manip_var_of(bas)
#' #' 
#' #' ## Radial tour array to long df, as used in play_manual_tour()
#' #' tour_array <- manual_tour(basis = bas, manip_var = mv)
#' ggbasis_array <- function(basis_array,
#'                           data = NULL,
#'                           label = NULL,
#'                           axes = "center",
#'                           manip_col = "blue",
#'                           line_size = 1L,
#'                           text_size = 5L,
#'                           aes_args = list(),
#'                           identity_args = list(),
#'                           ggproto = list(theme_spinifex())){
#'   ls_df <- spinifex::array2df(tour_array, data = data)
#'   ## see spinifex::render_
#'   #### Initialize
#'   basis_frames  <- ls_df$basis_frames
#'   n_frames      <- length(unique(basis_frames$frame))
#'   p             <- nrow(basis_frames) / n_frames
#'   d             <- 2L ## Hard-coded assumption for 2D display
#'   aes_args      <- as.list(aes_args)
#'   identity_args <- as.list(identity_args)
#'   ggproto       <- as.list(ggproto)
#'   
#'   ## If data exists; fix arg length and plot MUST COME BEFORE AXES
#'   data_frames <- frames$data_frames ## May be null.
#' }
#' 
#' if(F){
#'   spinifex::array2df
#'   ?spinifex::array2df
#' }
#' 
#' 
#' ### EMA paper examples, recreating source ----
#' #' @examples 
#' #' ## Working form source examples:
#' #' if(F) ## Working from: 
#' #'   browseURL("http://ema.drwhy.ai/shapley.html#SHAPRcode")
#' #' 
#' #' titanic_imputed <- archivist::aread("pbiecek/models/27e5c")
#' #' titanic_rf <- archivist::aread("pbiecek/models/4e0fc")
#' #' henry <- archivist::aread("pbiecek/models/a6538")
#' #' 
#' #' library("randomForest")
#' #' library("DALEX")
#' #' 
#' #' ## Make a DALEX "explainer" of in smaple data
#' #' explain_rf <- DALEX::explain(model = titanic_rf,  
#' #'                              data = titanic_imputed[, -9],
#' #'                              y = titanic_imputed$survived == "yes", 
#' #'                              label = "Random Forest")
#' #' ## Predict a single out of sample observation, "Henry"?
#' #' predict(explain_rf, henry)
#' #' 
#' #' tictoc::tic("shap_henry")
#' #' shap_henry <- predict_parts(explainer = explain_rf,  ## ~ 10 s @ B=25
#' #'                             new_observation = henry, 
#' #'                             type = "shap",
#' #'                             B = 10)
#' #' tictoc::toc()
#' #' plot(shap_henry, show_boxplots = FALSE)
#' #' 
#' #' print("note that iBreakDown:::print.break_down prints an agg tbl, not the 11 perms tested and desplayed when coerced to tibble.")
#' #' tib_shap_henry <- tibble::as.tibble(shap_henry) ## Note that SHAP is already showing only 7 of 77 branches.
#' #' hist(tib_shap_henry$contribution)
#' #' 
#' #' 
#' #' print("why isn't it showing the 7 largest contributions though??")
#' #' library("dplyr")
#' #' tib_shap_henry <- tib_shap_henry %>% arrange(desc(abs(contribution)))
#' #' tib_shap_henry
#' #' unique(tib_shap_henry$variable)
#' #' 
#' #' 
#' #' df_local_attr <- df_scree_local_attr(shap_henry)
#' #' v <- tourr::normalise(df_local_attr$median_local_attr)
