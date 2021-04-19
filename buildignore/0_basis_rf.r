require("DALEX")
require("randomForest")
require("Rdimtools")
require("spinifex")

### Ema example ----
if(interactive() == F){
  if(F) ## Working from: 
    browseURL("http://ema.drwhy.ai/shapley.html#SHAPRcode")
  
  titanic_imputed <- archivist::aread("pbiecek/models/27e5c")
  titanic_rf <- archivist::aread("pbiecek/models/4e0fc")
  henry <- archivist::aread("pbiecek/models/a6538")
  
  ## Make a DALEX "explainer" of in sample data
  explain_rf <- DALEX::explain(model = titanic_rf,  
                               data = titanic_imputed[, -9],
                               y = titanic_imputed$survived == "yes", 
                               label = "Random Forest")
  ## Predict a single out of sample observation, "Henry"?
  predict(explain_rf, henry)
  
  tictoc::tic("shap_henry")
  shap_henry <- predict_parts(explainer = explain_rf, ## ~10 s @ B=25
                              new_observation = henry,
                              type = "shap",
                              B = 10)
  tictoc::toc()
  plot(shap_henry, show_boxplots = FALSE)
  
  print("note that iBreakDown:::print.break_down prints an agg tbl, not the 11 perms tested and desplayed when coerced to tibble.")
  tib_shap_henry <- tibble::as.tibble(shap_henry) ## Note that SHAP is already showing only 7 of 77 branches.
  hist(tib_shap_henry$contribution)
  
  print("why isn't it showing the 7 largest contributions though??")
  library("dplyr")
  tib_shap_henry <- tib_shap_henry %>% arrange(desc(abs(contribution)))
  tib_shap_henry
  unique(tib_shap_henry$variable)
}

## Remade from: iBreakDown:::print.break_down_uncertainty
## Create the scree df for the local attribution from a DALEX::predict_parts return.
## !!may have overlap with iBreakDown:::plot.break_down_uncertainty.
#' @examples 
#' require("DALEX")
#' require("randomForest")
#' require("spinifex")
#' 
#' dat <- scale_sd(flea[, 1:6]) %>% as.data.frame()
#' clas <- flea$species
#' 
#' .obs_r <- 10
#' x <- dat[-.obs_r, ]
#' x_clas <- clas[-.obs_r]
#' oos <- dat[.obs_r, ]
#' 
#' this_rf <- randomForest::randomForest(x_clas~., data = data.frame(x, x_clas))
#' this_expl <- DALEX::explain(model = this_rf,
#'                             data = x,
#'                             y = x_clas,
#'                             label = "Random Forest")
#' this_parts <- DALEX::predict_parts(explainer = this_expl,
#'                                    new_observation = oos,
#'                                    type = "shap",
#'                                    B = 10)
#' df_scree_local_attr(this_parts)
df_scree_local_attr <- function(x){
  raw_scree_ALL_Y_LVLS <- data.frame(
    variable_name = tapply(x$variable_name, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
    oos_value = tapply(x$variable_value, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
    local_attr = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), median, na.rm = TRUE)
  )
  ## Reorder aggregate mean(abs(local_attr))
  agg_scree <-
    aggregate(local_attr~., data = raw_scree_ALL_Y_LVLS,
              function(c){mean(abs(c))})
  ## Reorder for cumsum
  .orig_la_order <- agg_scree$local_attr
  ret <- local_attr[order(local_attr$local_attr, decreasing = TRUE), ]
  ## Add cumsum_rate
  ret$cumsum_local_attr <- cumsum(ret$local_attr) / sum(ret$local_attr)
  return(ret)
}
if(F){
  df_local_attr <- df_scree_local_attr(shap_henry)
  v <- tourr::normalise(df_local_attr$median_local_attr)
}


##### basis_rf_importance -----
#' @examples
#' require("spinifex")
#' dat <- scale_sd(wine[, 2:14])
#' clas <- wine$Type
#' bas <- basis_olda_rf_imp(dat, clas) ## Notice that rf_imp != olda1:2.
#' 
#' ## Visualizing
#' view_frame(bas, dat,
#'            aes_args = list(color = clas, shape = clas))
#' 
#' bas2 <- basis_olda(dat, clas)
#' view_frame(bas2, dat,
#'            aes_args = list(color = clas, shape = clas))
#'            
#' bas3 <- basis_pca(dat)
#' view_frame(bas3, dat,
#'            aes_args = list(color = clas, shape = clas))
basis_olda_rf_imp <- function(data, class, d = 2L, imp_type = NULL, ...){
  ## OLDA space
  olda_obj <- Rdimtools::do.olda(X = as.matrix(data),
                                 label = as.factor(class),
                                 ndim = ncol(dat) - 1L)
  rf_mod <-
    randomForest::randomForest(class~., data = data.matrix(olda_obj$Y, class))
  imp <- randomForest::importance(rf_mod, type = imp_type, ...)
  ## Which olda basis columns to use.
  olda_idx <- order(imp, decreasing = TRUE)[1L:(d - 1L)]
  bas <- olda_obj$projection[, olda_idx]
  rownames(bas) <- colnames(data)
  colnames(bas) <- paste0("olda", olda_idx)
  return(bas)
}

##### basis_olda_local_attr -----
#' @examples
#' dat <- as.data.frame(spinifex::scale_sd(flea[, 1:6]))
#' clas <- flea$species
#' 
#' .obs_r <- 10
#' x <- dat[-.obs_r, ]
#' x_clas <- clas[-.obs_r]
#' oos <- dat[.obs_r, ]
#' 
#' basis_olda_local_attr(x, x_clas, oos)
basis_olda_local_attr <- function(data, class, new_obs, d = 2L, predict_type = "shap", ...){
  dat <- as.matrix(data)
  clas <- as.factor(class)
  ## Olda space
  olda_obj <- Rdimtools::do.olda(X = dat,
                                 label = clas,
                                 ndim = ncol(dat) - 1L)
  ## RF model, explain, parts, scree_la
  this_rf <- randomForest::randomForest(class~., data = data.matrix(dat, clas))
  this_expl <- DALEX::explain(model = this_rf,
                              data = dat,
                              y = clas,
                              label = "Random Forest")
  this_parts <- DALEX::predict_parts(explainer = this_expl,
                                     new_observation = new_obs,
                                     type = predict_type, ...)
  this_scree <- df_scree_local_attr(this_parts)
  ## Reorder scree to orig, grab local attr (la)
  .cn <- colnames(data)
  this_la <- this_scree[match(.cn, this_scree$variable_name), 3L]
  ## Get to basis
  bas <- tourr::orthonormalise(cbind(this_la, olda_obj$projection[, 1L:(d - 1L)]))
  colnames(bas) <- c("local_attr", paste0("olda", 1L:(d - 1L)))
  rownames(bas) <- colnames(data)
  return(bas)
}



#### Bringing it all together -----
## Trying to go to a toy example
if(interactive() == TRUE){
  require("DALEX")
  require("randomForest")
  require("spinifex")
  require("tourr")
  
  dat <- as.data.frame(scale_sd(flea[, 1:6]))
  clas <- flea$species
  
  .obs_r <- 10
  x <- dat[-.obs_r, ]
  x_clas <- clas[-.obs_r]
  oos <- dat[.obs_r, ]
  
  bas <- basis_olda_local_attr(x, x_clas, oos)
  TMP_plot_cheem <- function(basis, data, class, new_obs){
    proj <- as.matrix(data) %*% basis
    proj_new_obs <- data.frame(as.matrix(new_obs) %*% basis)
    view_frame(basis, data,
               aes_args = list(color = class, shape = class)) +
      geom_point(aes(x = local_attr, y = olda1), proj_new_obs, ## Red * at oos obs.
                 color = "red", size = 5, shape = 8) +
      theme(axis.title = element_text(),
            legend.position = "bottom",
            legend.direction = "horizontal", legend.box = "vertical") +
      xlab("SHAP (local attribution relative to new observation)") + ylab("oLD1") +
      ggtitle("Linear projection -- SHAP and oLD1 are X and Y axis respectively", "steps: scale_sd, hold out 1 obs, RF, SHAP, OLDA, orthonormalize(SHAP, LD1)")
  }
  TMP_plot_cheem(bas, x, x_clas, oos)
  ggsave("./buildignore/PoC_cheem_proj.png", width = 8, height = 8, units = "in")
}

