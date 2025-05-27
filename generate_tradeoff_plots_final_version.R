
GrabAdditionalAUCs <- function(k) {
  dataset_names <- c("abalone",
                     "bank_marketing",
                     "california_housing_original",
                     "credit",
                     "diabetes_130US",
                     "electricity",
                     "eye_movement",
                     "house_16h",
                     "magic_telescope",
                     "pol")
  ## assumes that the outputs from synthcity's Benchmarks
  ## function has been stored in a folder structure:
  ## "~/outputs/<dataset name>/benchmarks_stats/"
  additional_path1 <- "~/outputs/"
  additional_path2 <- "/benchmarks_stats/"
  
  ## get aucs for DDPM, ARF, TVAE, CTGAN, BayesNets
  additional_benchmark_files <- c("abalone_optuna_mean1.csv",
                                  "bank_marketing_mean1.csv",
                                  "california_housing_original_optuna_mean1.csv",
                                  "credit_mean1.csv",
                                  "diabetes_130US_mean1.csv",
                                  "electricity_mean1.csv",
                                  "eye_movement_mean1.csv",
                                  "house_16h_mean1.csv",
                                  "magic_telescope_mean1.csv",
                                  "pol_mean1.csv")
  out_path <- paste0(additional_path1, 
                     dataset_names[k], 
                     additional_path2,
                     additional_benchmark_files[k])
  aux_stats <- read.csv(out_path)
  add_aucs <- aux_stats[which(aux_stats$X == "detection.detection_xgb.mean"), -1]
  add_aucs <- add_aucs[c("ddpm", "arf", "tvae", "ctgan", "bayesnet")]
  
  ## add aucs for ADSGAN and PATEGAN
  additional_benchmark_files_dp <- c("abalone_optuna_mean_dp.csv",
                                     "bank_marketing_mean_dp.csv",
                                     "california_housing_original_optuna_mean_dp.csv",
                                     "credit_mean_dp.csv",
                                     "diabetes_130US_mean_dp.csv",
                                     "electricity_mean_dp.csv",
                                     "eye_movement_mean_dp.csv",
                                     "house_16h_mean_dp.csv",
                                     "magic_telescope_mean_dp.csv",
                                     "pol_mean_dp.csv")
  dp_path <- paste0(additional_path1, 
                    dataset_names[k], 
                    additional_path2,
                    additional_benchmark_files_dp[k])
  dp_stats <- read.csv(dp_path)
  dp_aucs <- dp_stats[which(dp_stats$X == "detection.detection_xgb.mean"), -1]
  dp_aucs <- dp_aucs[c("adsgan", "pategan")]
  add_aucs <- unlist(c(add_aucs, dp_aucs))
  
  ## add aucs for SMOTE
  smote_path <- "~/synthetic_privacy/tabular/SynTabSJPPDS/outputs/smote_more/"
  smote_path <- paste0(smote_path, 
                       paste0("mean_tuning_param_selection_", dataset_names[k], ".csv"))
  smote_stats <- read.csv(smote_path)
  smote_aucs <- smote_stats[which(smote_stats$X == "detection.detection_xgb.mean"), -1]
  smote_aucs <- smote_aucs[c("smote_20", "smote_5")]
  add_aucs <- unlist(c(add_aucs, smote_aucs))
  
  return(add_aucs)
}


TradeoffPlotDCR <- function(k,
                            my_pch,
                            methods_names_1,
                            methods_names_2,
                            main = "",
                            max_dcr = NULL,
                            dataset_names,
                            tabsds_dcr_path,
                            tabsds_dcr_files,
                            tabsds_benchmark_path,
                            dcrs,
                            my_cex = 1.75,
                            add_test_set_line = FALSE,
                            tabsds_nc,
                            leg_cex = 0.85,
                            legend_position = "bottomright",
                            add_legend = TRUE) {
  
  num_methods <- length(methods_names_1)
  
  dataset_tabsds_dcrs_path <- paste0(tabsds_dcr_path, tabsds_dcr_files[k])
  tabsds_dcrs <- read.csv(dataset_tabsds_dcrs_path)  
  tabsds_dcrs <- apply(tabsds_dcrs, 2, median)
  
  test_set_dcr <- tabsds_dcrs[1]
  tabsds_dcrs <- tabsds_dcrs[-1] ## remove test set
  
  additional_dcrs <- apply(dcrs[[k]], 2, median)
  additional_dcrs <- additional_dcrs[methods_names_1]
  
  dataset_tabsds_auc_path <- paste0(tabsds_benchmark_path, 
                                    paste0("mean_tuning_param_selection_", dataset_names[k], ".csv"))
  
  tabsds_stats <- read.csv(dataset_tabsds_auc_path)
  tabsds_aucs <- tabsds_stats[which(tabsds_stats$X == "detection.detection_xgb.mean"), -1]
  
  additional_aucs <- GrabAdditionalAUCs(k = k)
  additional_aucs <- additional_aucs[methods_names_2]
  
  nc_grid <- unlist(lapply(strsplit(names(tabsds_aucs), "_"), function(x) x[2]))
  idx <- which(nc_grid == tabsds_nc[k])
  
  aucs_lim <- c(min(tabsds_aucs, additional_aucs),
                max(tabsds_aucs, additional_aucs))
  
  dcrs_lim <- c(min(tabsds_dcrs, additional_dcrs),
                max(tabsds_dcrs, additional_dcrs))
  
  if (!is.null(max_dcr)) {
    dcrs_lim <- c(dcrs_lim[1], min(dcrs_lim[2], max_dcr))
  }
  
  plot(as.numeric(tabsds_dcrs), 
       as.numeric(tabsds_aucs),
       ylab = "Detection test AUC (smaller, better)",
       xlab = "DCR",
       ylim = aucs_lim, 
       xlim = dcrs_lim, 
       type = "b",
       col = "black",
       main = main)
  axis(side = 4, at = as.numeric(tabsds_aucs), labels = nc_grid)
  mtext("n_c", side = 4, line = 1.5, cex = 0.75)
  
  if (add_test_set_line) {
    abline(v = test_set_dcr, col = "red")
  }
  
  for (i in seq(num_methods)) {
    points(additional_dcrs[i], additional_aucs[i], col = "purple", 
           pch = my_pch[i], cex = my_cex)
  }
  
  points(as.numeric(tabsds_dcrs)[idx], 
         as.numeric(tabsds_aucs)[idx], 
         pch = 19, col = "red", cex = my_cex)
  
  if (add_legend) {
    legend(legend_position, 
           legend = c("TabSDS",
                      "sel TabSDS",
                      methods_names_1),
           text.col = c("black", "red", rep("purple", num_methods)),
           pch = c(1, 19, my_pch),
           col = c("black", "red", rep("purple", num_methods)),
           bty = "n",
           cex = leg_cex)
  }
  
}


TradeoffPlotDBRL <- function(k,
                             my_pch,
                             methods_names_1,
                             methods_names_2,
                             main = "",
                             my_cex = 1.75,
                             tabsds_nc,
                             leg_cex = 0.85,
                             legend_position = "topright",
                             add_legend = TRUE) {
  num_methods <- length(methods_names_1)
  
  obj_name_tabsds <- paste0(dbrl_sdid_names[k], "_tabsds")
  tabsds_dbrl <- get(obj_name_tabsds)$output$DBRL
  nc_grid <- get(obj_name_tabsds)$output$tuning_par
  idx <- which(nc_grid == tabsds_nc[k])
  
  obj_name_additional <- paste0(dbrl_sdid_names[k], "_additional")
  additional_output <- get(obj_name_additional)$output
  additional_output <- additional_output[additional_output$generator %in% methods_names_1,]
  additional_output <- additional_output[match(additional_output$generator, methods_names_1),] 
  additional_dbrl <- additional_output$DBRL
  
  dataset_tabsds_auc_path <- paste0(tabsds_benchmark_path, 
                                    paste0("mean_tuning_param_selection_", dataset_names[k], ".csv"))
  tabsds_stats <- read.csv(dataset_tabsds_auc_path)
  tabsds_aucs <- tabsds_stats[which(tabsds_stats$X == "detection.detection_xgb.mean"), -1]
  
  additional_aucs <- GrabAdditionalAUCs(k = k)
  additional_aucs <- additional_aucs[methods_names_2]
  
  aucs_lim <- c(min(tabsds_aucs, additional_aucs),
                max(tabsds_aucs, additional_aucs))
  
  dbrl_lim <- c(min(tabsds_dbrl, additional_dbrl),
                max(tabsds_dbrl, additional_dbrl))
  
  plot(as.numeric(tabsds_dbrl), 
       as.numeric(tabsds_aucs),
       ylab = "Detection test AUC (smaller, better)",
       xlab = "Disclosure risk in SDBRL (smaller, better)",
       ylim = aucs_lim, 
       xlim = dbrl_lim, 
       type = "b",
       col = "black",
       main = main)
  axis(side = 4, at = as.numeric(tabsds_aucs), labels = nc_grid)
  mtext("n_c", side = 4, line = 1.5, cex = 0.75)
  
  for (i in seq(num_methods)) {
    points(additional_dbrl[i], additional_aucs[i], col = "purple", 
           pch = my_pch[i], cex = my_cex)
  }
  
  points(as.numeric(tabsds_dbrl)[idx], 
         as.numeric(tabsds_aucs)[idx], 
         pch = 19, col = "red", cex = my_cex)
  
  if(add_legend) {
    legend(legend_position, 
           legend = c("TabSDS",
                      "sel TabSDS",
                      methods_names_1),
           text.col = c("black", "red", rep("purple", num_methods)),
           pch = c(1, 19, my_pch),
           col = c("black", "red", rep("purple", num_methods)),
           bty = "n",
           cex = leg_cex)
  }
  
}


TradeoffPlotSDID <- function(k,
                             my_pch,
                             methods_names_1,
                             methods_names_2,
                             main = "",
                             my_cex = 1.75,
                             tabsds_nc,
                             leg_cex = 0.85,
                             legend_position = "topright",
                             add_legend = TRUE) {
  num_methods <- length(methods_names_1)
  
  obj_name_tabsds <- paste0(dbrl_sdid_names[k], "_tabsds")
  tabsds_sdid <- get(obj_name_tabsds)$output$SDID
  nc_grid <- get(obj_name_tabsds)$output$tuning_par
  idx <- which(nc_grid == tabsds_nc[k])
  
  obj_name_additional <- paste0(dbrl_sdid_names[k], "_additional")
  additional_output <- get(obj_name_additional)$output
  additional_output <- additional_output[additional_output$generator %in% methods_names_1,]
  additional_output <- additional_output[match(additional_output$generator, methods_names_1),] 
  additional_sdid <- additional_output$SDID
  
  dataset_tabsds_auc_path <- paste0(tabsds_benchmark_path, 
                                    paste0("mean_tuning_param_selection_", dataset_names[k], ".csv"))
  tabsds_stats <- read.csv(dataset_tabsds_auc_path)
  tabsds_aucs <- tabsds_stats[which(tabsds_stats$X == "detection.detection_xgb.mean"), -1]
  
  additional_aucs <- GrabAdditionalAUCs(k = k)
  additional_aucs <- additional_aucs[methods_names_2]
  
  aucs_lim <- c(min(tabsds_aucs, additional_aucs),
                max(tabsds_aucs, additional_aucs))
  
  sdid_lim <- c(min(tabsds_sdid, additional_sdid),
                max(tabsds_sdid, additional_sdid))
  
  plot(as.numeric(tabsds_sdid), 
       as.numeric(tabsds_aucs),
       ylab = "Detection test AUC (smaller, better)",
       xlab = "Disclosure risk in SSDID (smaller, better)",
       ylim = aucs_lim, 
       xlim = sdid_lim, 
       type = "b",
       col = "black",
       main = main)
  axis(side = 4, at = as.numeric(tabsds_aucs), labels = nc_grid)
  mtext("n_c", side = 4, line = 1.5, cex = 0.75)
  
  for (i in seq(num_methods)) {
    points(additional_sdid[i], additional_aucs[i], col = "purple", 
           pch = my_pch[i], cex = my_cex)
  }
  points(as.numeric(tabsds_sdid)[idx], 
         as.numeric(tabsds_aucs)[idx], 
         pch = 19, col = "red", cex = my_cex)
  
  if (add_legend) {
    legend(legend_position, 
           legend = c("TabSDS",
                      "sel TabSDS",
                      methods_names_1),
           text.col = c("black", "red", rep("purple", num_methods)),
           pch = c(1, 19, my_pch),
           col = c("black", "red", rep("purple", num_methods)),
           bty = "n",
           cex = leg_cex)
  }
  
}




TabSDSvsNoiseDCR <- function(k,
                             main = "",
                             tabsds_dcr_path,
                             tabsds_dcr_files,
                             noise_dcr_path,
                             noise_dcr_files) {
  dataset_tabsds_dcrs_path <- paste0(tabsds_dcr_path, tabsds_dcr_files[k])
  dataset_noise_dcrs_path <- paste0(noise_dcr_path, noise_dcr_files[k])
  
  tabsds_dcrs <- read.csv(dataset_tabsds_dcrs_path)  
  tabsds_dcrs <- apply(tabsds_dcrs, 2, median)
  
  noise_dcrs <- read.csv(dataset_noise_dcrs_path)  
  noise_dcrs <- apply(noise_dcrs, 2, median)
  
  dataset_tabsds_auc_path <- paste0(tabsds_benchmark_path, 
                                    paste0("mean_tuning_param_selection_", dataset_names[k], ".csv"))
  dataset_noise_auc_path <- paste0(noise_benchmark_path,
                                   paste0("mean_tuning_param_selection_", dataset_names[k], ".csv"))
  
  tabsds_stats <- read.csv(dataset_tabsds_auc_path)
  tabsds_aucs <- tabsds_stats[which(tabsds_stats$X == "detection.detection_xgb.mean"), -1]
  
  noise_stats <- read.csv(dataset_noise_auc_path)
  noise_aucs <- noise_stats[which(noise_stats$X == "detection.detection_xgb.mean"), -1]
  ## for pol only (we have NAs for some of the last values)
  if (dataset_names[k] == "pol") {
    noise_aucs[is.na(noise_aucs)] <- 1
  }
  
  
  nc_grid <- unlist(lapply(strsplit(names(tabsds_aucs), "_"), function(x) x[2]))
  
  aucs_lim <- c(min(tabsds_aucs, noise_aucs),
                max(tabsds_aucs, noise_aucs))
  
  dcrs_lim <- c(min(tabsds_dcrs, noise_dcrs),
                max(tabsds_dcrs, noise_dcrs))
  
  plot(as.numeric(noise_dcrs)[-1], 
       as.numeric(noise_aucs),
       ylab = "Detection test AUC (smaller, better)",
       xlab = "DCR",
       ylim = aucs_lim, 
       xlim = dcrs_lim, 
       type = "b",
       col = "blue",
       main = main)
  
  axis(side = 4, at = as.numeric(tabsds_aucs), labels = nc_grid)
  mtext("n_c", side = 4, line = 1.5, cex = 0.75)
  
  points(tabsds_dcrs[-1], tabsds_aucs, col = "black", type = "b")
  
  legend("bottomright", 
         legend = c("TabSDS grid", "Additive noise grid"),
         text.col = c("black", "blue"),
         pch = c(1, 1),
         col = c("black", "blue"),
         bty = "n")
}



TabSDSvsNoiseDBRL <- function(k,
                              main = "",
                              dbrl_sdid_names,
                              dataset_names,
                              tabsds_benchmark_path,
                              noise_benchmark_path) {
  obj_name_tabsds <- paste0(dbrl_sdid_names[k], "_tabsds")
  tabsds_dbrl <- get(obj_name_tabsds)$output$DBRL
  nc_grid <- get(obj_name_tabsds)$output$tuning_par
  
  obj_name_noise <- paste0(dbrl_sdid_names[k], "_noise")
  noise_dbrl <- get(obj_name_noise)$output$DBRL
  
  dataset_tabsds_auc_path <- paste0(tabsds_benchmark_path, 
                                    paste0("mean_tuning_param_selection_", dataset_names[k], ".csv"))
  dataset_noise_auc_path <- paste0(noise_benchmark_path,
                                   paste0("mean_tuning_param_selection_", dataset_names[k], ".csv"))
  
  tabsds_stats <- read.csv(dataset_tabsds_auc_path)
  tabsds_aucs <- tabsds_stats[which(tabsds_stats$X == "detection.detection_xgb.mean"), -1]
  
  noise_stats <- read.csv(dataset_noise_auc_path)
  noise_aucs <- noise_stats[which(noise_stats$X == "detection.detection_xgb.mean"), -1]
  ## for pol only (we have NAs for some of the last values)
  if (dbrl_sdid_names[k] == "pol") {
    noise_aucs[is.na(noise_aucs)] <- 1
  }
  
  aucs_lim <- c(min(tabsds_aucs, noise_aucs),
                max(tabsds_aucs, noise_aucs))
  
  dbrl_lim <- c(min(tabsds_dbrl, noise_dbrl),
                max(tabsds_dbrl, noise_dbrl))
  
  plot(as.numeric(noise_dbrl), 
       as.numeric(noise_aucs),
       ylab = "Detection test AUC (smaller, better)",
       xlab = "Disclosure risk in SDBRL (smaller, better)",
       ylim = aucs_lim, 
       xlim = dbrl_lim, 
       type = "b",
       col = "blue",
       main = main)
  
  axis(side = 4, at = as.numeric(tabsds_aucs), labels = nc_grid)
  mtext("n_c", side = 4, line = 1.5, cex = 0.75)
  
  points(tabsds_dbrl, tabsds_aucs, col = "black", type = "b")
  
  legend("bottomright", 
         legend = c("TabSDS grid", "Additive noise grid"),
         text.col = c("black", "blue"),
         pch = c(1, 1),
         col = c("black", "blue"),
         bty = "n")
  
}


TabSDSvsNoiseSDID <- function(k,
                              main = "",
                              dbrl_sdid_names,
                              dataset_names,
                              tabsds_benchmark_path,
                              noise_benchmark_path) {
  obj_name_tabsds <- paste0(dbrl_sdid_names[k], "_tabsds")
  tabsds_sdid <- get(obj_name_tabsds)$output$SDID
  nc_grid <- get(obj_name_tabsds)$output$tuning_par
  
  obj_name_noise <- paste0(dbrl_sdid_names[k], "_noise")
  noise_sdid <- get(obj_name_noise)$output$SDID
  
  dataset_tabsds_auc_path <- paste0(tabsds_benchmark_path, 
                                    paste0("mean_tuning_param_selection_", dataset_names[k], ".csv"))
  dataset_noise_auc_path <- paste0(noise_benchmark_path,
                                   paste0("mean_tuning_param_selection_", dataset_names[k], ".csv"))
  
  tabsds_stats <- read.csv(dataset_tabsds_auc_path)
  tabsds_aucs <- tabsds_stats[which(tabsds_stats$X == "detection.detection_xgb.mean"), -1]
  
  noise_stats <- read.csv(dataset_noise_auc_path)
  noise_aucs <- noise_stats[which(noise_stats$X == "detection.detection_xgb.mean"), -1]
  ## for pol only (we have NAs for some of the last values)
  if (dbrl_sdid_names[k] == "pol") {
    noise_aucs[is.na(noise_aucs)] <- 1
  }
  
  aucs_lim <- c(min(tabsds_aucs, noise_aucs),
                max(tabsds_aucs, noise_aucs))
  
  sdid_lim <- c(min(tabsds_sdid, noise_sdid),
                max(tabsds_sdid, noise_sdid))
  
  plot(as.numeric(noise_sdid), 
       as.numeric(noise_aucs),
       ylab = "Detection test AUC (smaller, better)",
       xlab = "Disclosure risk in SSDID (smaller, better)",
       ylim = aucs_lim, 
       xlim = sdid_lim, 
       type = "b",
       col = "blue",
       main = main)
  
  axis(side = 4, at = as.numeric(tabsds_aucs), labels = nc_grid)
  mtext("n_c", side = 4, line = 1.5, cex = 0.75)
  
  points(tabsds_sdid, tabsds_aucs, col = "black", type = "b")
  
  legend("bottomright", 
         legend = c("TabSDS grid", "Additive noise grid"),
         text.col = c("black", "blue"),
         pch = c(1, 1),
         col = c("black", "blue"),
         bty = "n")
  
}


TabSDSvsTabSJPPDSDCR <- function(k,
                                 main = "",
                                 tabsds_dcr_path,
                                 tabsds_dcr_files,
                                 tabsjppds_dcr_path,
                                 tabsjppds_dcr_files) {
  
  dataset_tabsds_dcrs_path <- paste0(tabsds_dcr_path, tabsds_dcr_files[k])
  dataset_tabsjppds_dcrs_path <- paste0(tabsjppds_dcr_path, tabsjppds_dcr_files[k])
  
  tabsds_dcrs <- read.csv(dataset_tabsds_dcrs_path)  
  tabsds_dcrs <- apply(tabsds_dcrs, 2, median)[-1]
  
  tabsjppds_dcrs <- read.csv(dataset_tabsjppds_dcrs_path)  
  tabsjppds_dcrs <- apply(tabsjppds_dcrs, 2, median)[-1]
  
  nc_grid <- names(tabsds_dcrs)
  nc_grid <- as.numeric(substring(nc_grid, first = 2))
  
  my_ylim <- c(min(tabsds_dcrs, tabsjppds_dcrs),
               max(tabsds_dcrs, tabsjppds_dcrs))
  
  plot(nc_grid, tabsds_dcrs, type = "b", col = "black", 
       xlab = "n_c", ylab = "DRC", ylim = my_ylim, main = main)
  lines(nc_grid, tabsjppds_dcrs, type = "b", col = "brown")
  legend("topright", legend = c("TabSDS", "TabSJPPDS"),
         text.col = c("black", "brown"), bty = "n")
}



TabSDSvsTabSJPPDSDBRL <- function(k,
                                  main = "",
                                  dbrl_sdid_names) {
  obj_name_tabsds <- paste0(dbrl_sdid_names[k], "_tabsds")
  dataset_tabsds <- get(obj_name_tabsds)$output
  
  obj_name_tabsjppds <- paste0(dbrl_sdid_names[k], "_tabsjppds")
  dataset_tabsjppds <- get(obj_name_tabsjppds)$output 
  
  my_ylim <- c(min(dataset_tabsds$DBRL, dataset_tabsjppds$DBRL),
               max(dataset_tabsds$DBRL, dataset_tabsjppds$DBRL))
  
  plot(dataset_tabsds$tuning_par, dataset_tabsds$DBRL, 
       type = "b", col = "black", xlab = "n_c", ylim = my_ylim,
       ylab = "SDBRL", main = main)
  lines(dataset_tabsjppds$tuning_par, dataset_tabsjppds$DBRL, 
        type = "b", col = "brown")
  legend("bottomright", legend = c("TabSDS", "TabSJPPDS"),
         text.col = c("black", "brown"), bty = "n")
}


TabSDSvsTabSJPPDSSDID <- function(k,
                                  main = "",
                                  dbrl_sdid_names) {
  obj_name_tabsds <- paste0(dbrl_sdid_names[k], "_tabsds")
  dataset_tabsds <- get(obj_name_tabsds)$output
  
  obj_name_tabsjppds <- paste0(dbrl_sdid_names[k], "_tabsjppds")
  dataset_tabsjppds <- get(obj_name_tabsjppds)$output 
  
  my_ylim <- c(min(dataset_tabsds$SDID, dataset_tabsjppds$SDID),
               max(dataset_tabsds$SDID, dataset_tabsjppds$SDID))
  
  plot(dataset_tabsds$tuning_par, dataset_tabsds$SDID, 
       type = "b", col = "black", xlab = "n_c", ylim = my_ylim,
       ylab = "SSDID", main = main)
  lines(dataset_tabsjppds$tuning_par, dataset_tabsjppds$SDID, 
        type = "b", col = "brown")
  legend("bottomright", legend = c("TabSDS", "TabSJPPDS"),
         text.col = c("black", "brown"), bty = "n")
}



###################################################
###################################################
###################################################

tabsds_nc <- c(20,
               100,
               200,
               1000,
               35,
               20,
               20,
               1000,
               25,
               15)

dataset_names <- c("abalone",
                   "bank_marketing",
                   "california_housing_original",
                   "credit",
                   "diabetes_130US",
                   "electricity",
                   "eye_movement",
                   "house_16h",
                   "magic_telescope",
                   "pol")

dbrl_sdid_names <- c("abalone",
                     "bank_marketing",
                     "california",
                     "credit",
                     "diabetes",
                     "electricity",
                     "eye_movement",
                     "house_16h",
                     "magic_telescope",
                     "pol")


## path for the folder containing the outputs of the
## tabsds_dcr_evaluations_for_icml_2025.ipynb Jupyter notebook
tabsds_dcr_path <- ""

## path for the folder containing the outputs of the
## benchmark_tabsds_for_icml_2025.ipynb Jupyter notebook
tabsds_benchmark_path <- ""

tabsds_dcr_files <- c("syn_tab_sjppds_dcr_AB.csv",
                      "syn_tab_sjppds_dcr_BM.csv",
                      "syn_tab_sjppds_dcr_CH.csv",
                      "syn_tab_sjppds_dcr_CR.csv",
                      "syn_tab_sjppds_dcr_DI.csv",
                      "syn_tab_sjppds_dcr_EL.csv",
                      "syn_tab_sjppds_dcr_EM.csv",
                      "syn_tab_sjppds_dcr_HO.csv",
                      "syn_tab_sjppds_dcr_MT.csv",
                      "syn_tab_sjppds_dcr_PO.csv")


## model DCRs (output generated by the "compute_model_dcrs.R" script)
load("dcrs_additional_methods_with_dp_methods.RData")

## model SDBRLs and SSDIDs (output generated by the "compute_SDBRL_SSDID.R" script)
load("dbrl_sdid_outputs.RData")


dataset_names2 <- c("Abalone (AB)",
                    "Bank marketing (BM)",
                    "California housing (CH)",
                    "Credit (CR)",
                    "Diabetes 130US (DI)",
                    "Electricity (EL)",
                    "Eye movement (EM)",
                    "House 16H (HO)",
                    "Magic telescope (MT)",
                    "Pol (PO)")

fig_path <- ""


###################################################
## main text figure
###################################################

methods_names_1 <- c("DDPM", "ARF", "TVAE", "CTGAN", "BayesNet",
                     "SMOTE_k20", "SMOTE_k5")

methods_names_11 <- c("DDPM", "ARF", "TVAE", "CTGAN", "BayesNet",
                      "SMOTE_20", "SMOTE_5")

methods_names_2 <- c("ddpm", "arf", "tvae", "ctgan", "bayesnet",
                     "smote_20", "smote_5")

my_pch <- c(15, 17, 18, 8, 7, 9, 10)

figname <- paste0(fig_path, "tradeoff_plots_auc_vs_dcr_dbrl_sdid_main.pdf")
pdf(figname, width = 4.5, height = 6)
par(mfrow = c(3, 2), mar = c(2.75, 3, 1.1, 2.75), mgp = c(1.5, 0.5, 0))
TradeoffPlotDCR(k = 1,
                my_pch = my_pch,
                methods_names_1 = methods_names_1,
                methods_names_2 = methods_names_2,
                main = dataset_names2[1],
                max_dcr = 500,
                dataset_names = dataset_names,
                tabsds_dcr_path = tabsds_dcr_path,
                tabsds_dcr_files = tabsds_dcr_files,
                tabsds_benchmark_path = tabsds_benchmark_path,
                dcrs = dcrs,
                add_test_set_line = TRUE,
                tabsds_nc = tabsds_nc,
                leg_cex = 1)
mtext("(a)", side = 3, adj = 0)
####
TradeoffPlotDCR(k = 2,
                my_pch = my_pch,
                methods_names_1 = methods_names_1,
                methods_names_2 = methods_names_2,
                main = dataset_names2[2],
                max_dcr = 500,
                dataset_names = dataset_names,
                tabsds_dcr_path = tabsds_dcr_path,
                tabsds_dcr_files = tabsds_dcr_files,
                tabsds_benchmark_path = tabsds_benchmark_path,
                dcrs = dcrs,
                add_test_set_line = TRUE,
                tabsds_nc = tabsds_nc,
                legend_position = "bottomleft",
                add_legend = FALSE)
mtext("(b)", side = 3, adj = -0.15)
####
TradeoffPlotDBRL(k = 1,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[1],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext("(c)", side = 3, adj = 0)
####
TradeoffPlotDBRL(k = 2,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[2],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext("(d)", side = 3, adj = -0.15)
####
TradeoffPlotSDID(k = 1,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[1],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext("(e)", side = 3, adj = 0)
####
TradeoffPlotSDID(k = 2,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[2],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext("(f)", side = 3, adj = -0.15)
dev.off()



###################################################
## appendix figures
###################################################

methods_names_1 <- c("DDPM", "ARF", "TVAE", "CTGAN", "BayesNet",
                     "ADSGAN", "PATEGAN", "SMOTE_k20", "SMOTE_k5")

methods_names_11 <- c("DDPM", "ARF", "TVAE", "CTGAN", "BayesNet",
                      "ADSGAN", "PATEGAN", "SMOTE_20", "SMOTE_5")

methods_names_2 <- c("ddpm", "arf", "tvae", "ctgan", "bayesnet",
                     "adsgan", "pategan", "smote_20", "smote_5")


my_pch <- c(15, 17, 18, 8, 7, 6, 5, 9, 10)


jj <- 1
figname <- paste0(fig_path, "tradeoff_plots_auc_vs_dcr_sdbrl_ssdid_part1.pdf")
pdf(figname, width = 8, height = 11.75)
par(mfrow = c(5, 3), mar = c(2.75, 3, 1.1, 2.75), mgp = c(1.5, 0.5, 0))
TradeoffPlotDCR(k = 1,
                my_pch = my_pch,
                methods_names_1 = methods_names_1,
                methods_names_2 = methods_names_2,
                main = dataset_names2[1],
                max_dcr = 500,
                dataset_names = dataset_names,
                tabsds_dcr_path = tabsds_dcr_path,
                tabsds_dcr_files = tabsds_dcr_files,
                tabsds_benchmark_path = tabsds_benchmark_path,
                dcrs = dcrs,
                add_test_set_line = TRUE,
                tabsds_nc = tabsds_nc,
                leg_cex = 1.1)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
TradeoffPlotDBRL(k = 1,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[1],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
TradeoffPlotSDID(k = 1,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[1],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
for (k in seq(2, 5)) {
  TradeoffPlotDCR(k = k,
                  my_pch = my_pch,
                  methods_names_1 = methods_names_1,
                  methods_names_2 = methods_names_2,
                  main = dataset_names2[k],
                  max_dcr = 500,
                  dataset_names = dataset_names,
                  tabsds_dcr_path = tabsds_dcr_path,
                  tabsds_dcr_files = tabsds_dcr_files,
                  tabsds_benchmark_path = tabsds_benchmark_path,
                  dcrs = dcrs,
                  add_test_set_line = TRUE,
                  tabsds_nc = tabsds_nc,
                  add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  TradeoffPlotDBRL(k = k,
                   my_pch = my_pch,
                   methods_names_1 = methods_names_11,
                   methods_names_2 = methods_names_2,
                   main = dataset_names2[k],
                   my_cex = 1.5,
                   tabsds_nc = tabsds_nc,
                   add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  TradeoffPlotSDID(k = k,
                   my_pch = my_pch,
                   methods_names_1 = methods_names_11,
                   methods_names_2 = methods_names_2,
                   main = dataset_names2[k],
                   my_cex = 1.5,
                   tabsds_nc = tabsds_nc,
                   add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
}
dev.off()


jj <- 1
figname <- paste0(fig_path, "tradeoff_plots_auc_vs_dcr_sdbrl_ssdid_part2.pdf")
pdf(figname, width = 8, height = 11.75)
par(mfrow = c(5, 3), mar = c(2.75, 3, 1.1, 2.75), mgp = c(1.5, 0.5, 0))
TradeoffPlotDCR(k = 6,
                my_pch = my_pch,
                methods_names_1 = methods_names_1,
                methods_names_2 = methods_names_2,
                main = dataset_names2[6],
                max_dcr = 500,
                dataset_names = dataset_names,
                tabsds_dcr_path = tabsds_dcr_path,
                tabsds_dcr_files = tabsds_dcr_files,
                tabsds_benchmark_path = tabsds_benchmark_path,
                dcrs = dcrs,
                add_test_set_line = TRUE,
                tabsds_nc = tabsds_nc,
                leg_cex = 0.85)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
TradeoffPlotDBRL(k = 6,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[6],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
TradeoffPlotSDID(k = 6,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[6],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
for (k in seq(7, 10)) {
  TradeoffPlotDCR(k = k,
                  my_pch = my_pch,
                  methods_names_1 = methods_names_1,
                  methods_names_2 = methods_names_2,
                  main = dataset_names2[k],
                  max_dcr = 500,
                  dataset_names = dataset_names,
                  tabsds_dcr_path = tabsds_dcr_path,
                  tabsds_dcr_files = tabsds_dcr_files,
                  tabsds_benchmark_path = tabsds_benchmark_path,
                  dcrs = dcrs,
                  add_test_set_line = TRUE,
                  tabsds_nc = tabsds_nc,
                  add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  TradeoffPlotDBRL(k = k,
                   my_pch = my_pch,
                   methods_names_1 = methods_names_11,
                   methods_names_2 = methods_names_2,
                   main = dataset_names2[k],
                   my_cex = 1.5,
                   tabsds_nc = tabsds_nc,
                   add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  TradeoffPlotSDID(k = k,
                   my_pch = my_pch,
                   methods_names_1 = methods_names_11,
                   methods_names_2 = methods_names_2,
                   main = dataset_names2[k],
                   my_cex = 1.5,
                   tabsds_nc = tabsds_nc,
                   add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
}
dev.off()


######################################################


noise_dcr_files <- c("additive_noise_dcr_AB.csv",
                     "additive_noise_dcr_BM.csv",
                     "additive_noise_dcr_CH.csv",
                     "additive_noise_dcr_CR.csv",
                     "additive_noise_dcr_DI.csv",
                     "additive_noise_dcr_EL.csv",
                     "additive_noise_dcr_EM.csv",
                     "additive_noise_dcr_HO.csv",
                     "additive_noise_dcr_MT.csv",
                     "additive_noise_dcr_PO.csv")

tabsjppds_dcr_files <- c("tab_sjppds_dcr_AB.csv",
                         "tab_sjppds_dcr_BM.csv",
                         "tab_sjppds_dcr_CH.csv",
                         "tab_sjppds_dcr_CR.csv",
                         "tab_sjppds_dcr_DI.csv",
                         "tab_sjppds_dcr_EL.csv",
                         "tab_sjppds_dcr_EM.csv",
                         "tab_sjppds_dcr_HO.csv",
                         "tab_sjppds_dcr_MT.csv",
                         "tab_sjppds_dcr_PO.csv")

tabsds_nc <- c(20,
               100,
               200,
               1000,
               35,
               20,
               20,
               1000,
               25,
               15)


## path for the folder containing the outputs of the
## tabsds_dcr_evaluations_for_icml_2025.ipynb Jupyter notebook
tabsds_dcr_path <- ""

## path for the folder containing the outputs of the
## tabsjppds_dcr_evaluations_for_icml_2025.ipynb Jupyter notebook
tabsjppds_dcr_path <- ""

## path for the folder containing the outputs of the
## additive_noise_dcr.R script
noise_dcr_path <- ""

## path for the folder containing the outputs of the
## benchmark_tabsds_for_icml_2025.ipynb Jupyter notebook
tabsds_benchmark_path <- ""

## path for the folder containing the outputs of the
## benchmark_additive_noise_for_icml_2025.ipynb Jupyter notebook
noise_benchmark_path <- ""


figname <- paste0(fig_path, "tradeoff_plots_tabsds_vs_additive_noise_part1.pdf")
pdf(figname, width = 8, height = 11.75)
par(mfrow = c(5, 3), mar = c(2.75, 3, 1.1, 2.75), mgp = c(1.5, 0.5, 0))
jj <- 1
for (k in seq(1, 5)) {
  TabSDSvsNoiseDCR(k,
                   main = dataset_names2[k],
                   tabsds_dcr_path,
                   tabsds_dcr_files,
                   noise_dcr_path,
                   noise_dcr_files)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  ####
  TabSDSvsNoiseDBRL(k,
                    main = dataset_names2[k],
                    dbrl_sdid_names,
                    dataset_names,
                    tabsds_benchmark_path,
                    noise_benchmark_path)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  ####
  TabSDSvsNoiseSDID(k,
                    main = dataset_names2[k],
                    dbrl_sdid_names,
                    dataset_names,
                    tabsds_benchmark_path,
                    noise_benchmark_path)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
}
dev.off()


figname <- paste0(fig_path, "tradeoff_plots_tabsds_vs_additive_noise_part2.pdf")
pdf(figname, width = 8, height = 11.75)
par(mfrow = c(5, 3), mar = c(2.75, 3, 1.1, 2.75), mgp = c(1.5, 0.5, 0))
jj <- 1
for (k in seq(6, 10)) {
  TabSDSvsNoiseDCR(k,
                   main = dataset_names2[k],
                   tabsds_dcr_path,
                   tabsds_dcr_files,
                   noise_dcr_path,
                   noise_dcr_files)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  ####
  TabSDSvsNoiseDBRL(k,
                    main = dataset_names2[k],
                    dbrl_sdid_names,
                    dataset_names,
                    tabsds_benchmark_path,
                    noise_benchmark_path)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  ####
  TabSDSvsNoiseSDID(k,
                    main = dataset_names2[k],
                    dbrl_sdid_names,
                    dataset_names,
                    tabsds_benchmark_path,
                    noise_benchmark_path)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
}
dev.off()



figname <- paste0(fig_path, "compare_tabsds_vs_tabsjppds_part1.pdf")
pdf(figname, width = 8, height = 11.75)
par(mfrow = c(5, 3), mar = c(2.75, 3, 1.1, 2.75), mgp = c(1.5, 0.5, 0))
jj <- 1
for (k in seq(1, 5)) {
  TabSDSvsTabSJPPDSDCR(k,
                       main = dataset_names2[k],
                       tabsds_dcr_path,
                       tabsds_dcr_files,
                       tabsjppds_dcr_path,
                       tabsjppds_dcr_files)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  ####
  TabSDSvsTabSJPPDSDBRL(k,
                        main = dataset_names2[k],
                        dbrl_sdid_names)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  ####
  TabSDSvsTabSJPPDSSDID(k,
                        main = dataset_names2[k],
                        dbrl_sdid_names)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
}
dev.off()


figname <- paste0(fig_path, "compare_tabsds_vs_tabsjppds_part2.pdf")
pdf(figname, width = 8, height = 11.75)
par(mfrow = c(5, 3), mar = c(2.75, 3, 1.1, 2.75), mgp = c(1.5, 0.5, 0))
jj <- 1
for (k in seq(6, 10)) {
  TabSDSvsTabSJPPDSDCR(k,
                       main = dataset_names2[k],
                       tabsds_dcr_path,
                       tabsds_dcr_files,
                       tabsjppds_dcr_path,
                       tabsjppds_dcr_files)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  ####
  TabSDSvsTabSJPPDSDBRL(k,
                        main = dataset_names2[k],
                        dbrl_sdid_names)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  ####
  TabSDSvsTabSJPPDSSDID(k,
                        main = dataset_names2[k],
                        dbrl_sdid_names)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
}
dev.off()



#########################################
## scaled version
#########################################

tabsds_dcr_path <- ""
## models DCRs (output generated by the "compute_model_dcrs.R" script)
load("dcrs_additional_methods_with_dp_methods_scaled.RData")

tabsds_nc <- c(20, ## AB
               15, ## BM
               45, ## CH
               20, ## CR
               10, ## DI
               40, ## EL
               10, ## EM
               15, ## HO
               15, ## MT
               15) ## PO


jj <- 1
figname <- paste0(fig_path, "tradeoff_plots_auc_vs_dcr_sdbrl_ssdid_scaled_part1.pdf")
pdf(figname, width = 8, height = 11.75)
par(mfrow = c(5, 3), mar = c(2.75, 3, 1.1, 2.75), mgp = c(1.5, 0.5, 0))
TradeoffPlotDCR(k = 1,
                my_pch = my_pch,
                methods_names_1 = methods_names_1,
                methods_names_2 = methods_names_2,
                main = dataset_names2[1],
                max_dcr = 500,
                dataset_names = dataset_names,
                tabsds_dcr_path = tabsds_dcr_path,
                tabsds_dcr_files = tabsds_dcr_files,
                tabsds_benchmark_path = tabsds_benchmark_path,
                dcrs = dcrs,
                add_test_set_line = TRUE,
                tabsds_nc = tabsds_nc,
                leg_cex = 1.0)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
TradeoffPlotDBRL(k = 1,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[1],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
TradeoffPlotSDID(k = 1,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[1],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
for (k in seq(2, 5)) {
  TradeoffPlotDCR(k = k,
                  my_pch = my_pch,
                  methods_names_1 = methods_names_1,
                  methods_names_2 = methods_names_2,
                  main = dataset_names2[k],
                  max_dcr = 500,
                  dataset_names = dataset_names,
                  tabsds_dcr_path = tabsds_dcr_path,
                  tabsds_dcr_files = tabsds_dcr_files,
                  tabsds_benchmark_path = tabsds_benchmark_path,
                  dcrs = dcrs,
                  add_test_set_line = TRUE,
                  tabsds_nc = tabsds_nc,
                  add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  TradeoffPlotDBRL(k = k,
                   my_pch = my_pch,
                   methods_names_1 = methods_names_11,
                   methods_names_2 = methods_names_2,
                   main = dataset_names2[k],
                   my_cex = 1.5,
                   tabsds_nc = tabsds_nc,
                   add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  TradeoffPlotSDID(k = k,
                   my_pch = my_pch,
                   methods_names_1 = methods_names_11,
                   methods_names_2 = methods_names_2,
                   main = dataset_names2[k],
                   my_cex = 1.5,
                   tabsds_nc = tabsds_nc,
                   add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
}
dev.off()


jj <- 1
figname <- paste0(fig_path, "tradeoff_plots_auc_vs_dcr_sdbrl_ssdid_scaled_part2.pdf")
pdf(figname, width = 8, height = 11.75)
par(mfrow = c(5, 3), mar = c(2.75, 3, 1.1, 2.75), mgp = c(1.5, 0.5, 0))
TradeoffPlotDCR(k = 6,
                my_pch = my_pch,
                methods_names_1 = methods_names_1,
                methods_names_2 = methods_names_2,
                main = dataset_names2[6],
                max_dcr = 500,
                dataset_names = dataset_names,
                tabsds_dcr_path = tabsds_dcr_path,
                tabsds_dcr_files = tabsds_dcr_files,
                tabsds_benchmark_path = tabsds_benchmark_path,
                dcrs = dcrs,
                add_test_set_line = TRUE,
                tabsds_nc = tabsds_nc,
                leg_cex = 0.85)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
TradeoffPlotDBRL(k = 6,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[6],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
TradeoffPlotSDID(k = 6,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[6],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
for (k in seq(7, 9)) {
  TradeoffPlotDCR(k = k,
                  my_pch = my_pch,
                  methods_names_1 = methods_names_1,
                  methods_names_2 = methods_names_2,
                  main = dataset_names2[k],
                  max_dcr = 500,
                  dataset_names = dataset_names,
                  tabsds_dcr_path = tabsds_dcr_path,
                  tabsds_dcr_files = tabsds_dcr_files,
                  tabsds_benchmark_path = tabsds_benchmark_path,
                  dcrs = dcrs,
                  add_test_set_line = TRUE,
                  tabsds_nc = tabsds_nc,
                  add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  TradeoffPlotDBRL(k = k,
                   my_pch = my_pch,
                   methods_names_1 = methods_names_11,
                   methods_names_2 = methods_names_2,
                   main = dataset_names2[k],
                   my_cex = 1.5,
                   tabsds_nc = tabsds_nc,
                   add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
  TradeoffPlotSDID(k = k,
                   my_pch = my_pch,
                   methods_names_1 = methods_names_11,
                   methods_names_2 = methods_names_2,
                   main = dataset_names2[k],
                   my_cex = 1.5,
                   tabsds_nc = tabsds_nc,
                   add_legend = FALSE)
  mtext(letters[jj], side = 3, adj = 0)
  jj <- jj + 1
}
## deal with NA values in pategan
TradeoffPlotDCR(k = 10,
                my_pch = my_pch[-7],
                methods_names_1 = methods_names_1[-7],
                methods_names_2 = methods_names_2[-7],
                main = dataset_names2[10],
                max_dcr = 500,
                dataset_names = dataset_names,
                tabsds_dcr_path = tabsds_dcr_path,
                tabsds_dcr_files = tabsds_dcr_files,
                tabsds_benchmark_path = tabsds_benchmark_path,
                dcrs = dcrs,
                add_test_set_line = TRUE,
                tabsds_nc = tabsds_nc,
                add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
####
TradeoffPlotDBRL(k = 10,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[10],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
TradeoffPlotSDID(k = 10,
                 my_pch = my_pch,
                 methods_names_1 = methods_names_11,
                 methods_names_2 = methods_names_2,
                 main = dataset_names2[10],
                 my_cex = 1.5,
                 tabsds_nc = tabsds_nc,
                 add_legend = FALSE)
mtext(letters[jj], side = 3, adj = 0)
jj <- jj + 1
dev.off()








