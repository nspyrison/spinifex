
# #devtools::install_github('ModelOriented/treeshap')
# ## uses an algorithm to compute SHAP values for tree ensemble models, in polynomial-time, Lundberg et.al (2018)

require("DALEX")
require("randomForest")
require("Rdimtools")
require("spinifex")

if(F) ## Working from: 
  browseURL("http://ema.drwhy.ai/shapley.html#SHAPRcode")

message(interactive())
if(interactive() == F){
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
  shap_henry <- predict_parts(explainer = explain_rf,  ## ~ 10 s @ B=25
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
df_scree_local_attr <- function(x, ...){
  result <- data.frame(
    label = tapply(x$label, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
    variable_name = tapply(x$variable_name, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
    variable_value = tapply(x$variable_value, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
    median_local_attr = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), median, na.rm = TRUE)
  )
  ## Reorder
  result <- result[order(abs(result$median_local_attr), decreasing = TRUE), ]
  ## Add cumsum_rate
  result$cumsum_rate_abs_median_local_attr <- cumsum(abs(result$median_local_attr)) / sum(abs(result$median_local_attr))
  return(result)
}
df_local_attr <- df_scree_local_attr(shap_henry)

v <- tourr::normalise(df_local_attr$median_local_attr)

####
## Trying to go to a toy example
library(tourr)
library(spinifex)
library(ggplot2)
scaled_full <-  scale_sd(flea[, 1:6])
f <- scaled_full[-10, 1:6]
clas <- flea$species[-10]
my_y <- clas == "Concinna "
oos_obs <- scaled_full[10, 1:6]

f_rf <- randomForest(my_y~., data = data.frame(f, my_y))
ex_f_rf <- DALEX::explain(model = f_rf,
                          data = f,
                          y = my_y,
                          label = "Random Forest")
shap_10 <- predict_parts(explainer = ex_f_rf, ## ~ 10 s @ B=25
                         new_observation = oos_obs, 
                         type = "shap",
                         B = 10)
plot(shap_10)#, show_boxplots = FALSE)

f_local_attr <- df_scree_local_attr(shap_10) ## 1 shap for each class :/...
f_local_attr_heikert <- f_local_attr[f_local_attr$label == "Random Forest.Heikert. ",]
v <- tourr::normalise(f_local_attr_heikert$median_local_attr[-7]) ## wants to be ordered by orig data order.
olda <- basis_olda(f, my_y)

v.olda <- cbind(as.data.frame(v), olda)
colnames(v.olda) <- c("local_attr", "ld1", "ld2")
bas <- as.matrix(tourr::orthonormalise(v.olda)[, 1:2])

proj <- f %*% bas
proj_oos_obs <- data.frame(matrix(oos_obs, nrow=1) %*% bas)

view_frame(bas, f,
           aes_args = list(color = clas, shape = clas)) + 
  geom_point(aes(x = local_attr, y = ld1), proj_oos_obs, color = "red", size = 5, shape = 4) +
  theme(axis.title = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal", legend.box = "vertical") +
  xlab("SHAP/normalized local attr") + ylab("LD1") +
  ggtitle("Linear proj of Flea", "scale_sd, rm 1 obs, RF, SHAP, OLDA, orthonormalize(SHAP, LD1)")





##### basis_rf_importance -----
basis_olda_rf_imp <- function(data, class, d = 2L, type, ...){
  ## OLDA space 
  olda_obj <- Rdimtools::do.olda(X = as.matrix(data),
                                label = as.factor(class),
                                ndim = ncol(dat) - 1L)
  rf_mod <- randomForest(class~., data = data.matrix(olda_obj$Y, class))
  imp <- importance(rf_mod, type = type, ...)
  ## Which olda basis columns to use.
  olda_idx <- order(imp, decreasing = TRUE)[1L:d]
  ret <- olda_obj$projection[, olda_idx]
  rownames(ret) <- colnames(data)
  colnames(ret) <- paste0("OLDA", olda_idx)
  return(ret)
}

##### basis_olda_local_attr -----
## TODO: How to handle new_obs, as a hold out number in rows? expect a new vector?
basis_olda_local_attr <- function(data, class, new_obs, d = 2L, type, ...){
  dat <- as.matrix(data)
  clas <- as.factor(class)
  ## OLDA space 
  olda_obj <- Rdimtools::do.olda(X = dat,
                                 label = clas,
                                 ndim = ncol(dat) - 1L)
  rf_mod <- randomForest(class~., data = data.matrix(olda_obj$Y, clas))
  ## Explainer
  expl <- DALEX::explain(model = rf_mod,
                         data = dat,
                         y = clas,
                         label = "Random Forest")
  shap <- predict_parts(explainer = expl,
                        new_observation = new_obs,
                        type = "shap", ...)
  ##TODO continue here
  plot(shap_10)#, show_boxplots = FALSE)
  f_local_attr <- df_scree_local_attr(shap_10) ## 1 shap for each class :/...

  
  rownames(ret) <- colnames(data)
  colnames(ret) <- paste0("OLDA", olda_idx)
  return(ret)
}