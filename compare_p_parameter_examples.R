
CompareParameterP <- function(out_path_benchmark_1,
                              file_name_benchmark_1,
                              out_path_dcr_1 = NULL,
                              file_name_dcr_1 = NULL,
                              out_path_benchmark_2,
                              file_name_benchmark_2,
                              out_path_dcr_2 = NULL,
                              file_name_dcr_2 = NULL,
                              my_layout_matrix,
                              my_mar = c(5, 4, 2, 2),
                              my_mgp = c(3, 1, 0),
                              dcr_labels) {
  
  par(mar = my_mar, mgp = my_mgp)
  
  ## benchmark panels
  means_1 <- read.csv(paste0(out_path_benchmark_1, file_name_benchmark_1))
  means_2 <- read.csv(paste0(out_path_benchmark_2, file_name_benchmark_2))
  
  nms <- colnames(means_1)[-1]
  tp_grid <- as.numeric(unlist(lapply(strsplit(nms, "_"), function(x) x[2])))
  
  metric_names <- means_1$X
  
  means_1 <- means_1[, -1]
  rownames(means_1) <- metric_names
  
  means_2 <- means_2[, -1]
  rownames(means_2) <- metric_names
  
  layout(my_layout_matrix)
  
  vals_1 <- as.numeric(means_1["detection.detection_xgb.mean",])
  vals_2 <- as.numeric(means_2["detection.detection_xgb.mean",])
  my_ylim <- c(min(c(vals_1, vals_2)),
               max(c(vals_1, vals_2)))
  plot(tp_grid, vals_1, type = "b", xlab = expression(n[c]), ylab = "metric",
       main = "detection test", col = "red", ylim = my_ylim)
  lines(tp_grid, vals_2, type = "b", col = "blue")
  mtext(side = 3, "(a)", at = 0)
  legend("topright", legend = c("p = 0.5", "p = 0.1"), 
         text.col = c("red", "blue"), bty = "n")
  
  val_gt <- means_1["performance.xgb.gt", 1]
  vals_1 <- as.numeric(means_1["performance.xgb.syn_ood",])
  vals_2 <- as.numeric(means_2["performance.xgb.syn_ood",])
  my_ylim <- c(min(c(val_gt, vals_1, vals_2)),
               max(c(val_gt, vals_1, vals_2)))
  plot(tp_grid, vals_1, type = "b", xlab = expression(n[c]), ylab = "metric",
       main = "ML efficiency", ylim = my_ylim, col = "red")
  lines(tp_grid, vals_2, type = "b", col = "blue")
  abline(h = val_gt, col = "grey")
  mtext(side = 3, "(b)", at = 0)
  legend("bottomright", legend = c("p = 0.5", "p = 0.1"), 
         text.col = c("red", "blue"), bty = "n")
  
  if ("privacy.DomiasMIA_prior.aucroc" %in% metric_names) {
    vals_1 <- as.numeric(means_1["privacy.DomiasMIA_prior.aucroc",])
    vals_2 <- as.numeric(means_2["privacy.DomiasMIA_prior.aucroc",])
    my_ylim <- c(min(c(vals_1, vals_2)),
                 max(c(vals_1, vals_2)))
    plot(tp_grid, vals_1, type = "b", xlab = expression(n[c]), ylab = "metric",
         main = "Domias MIA", col = "red", ylim = my_ylim)
    lines(tp_grid, vals_2, type = "b", col = "blue")
    legend("bottomright", legend = c("p = 0.5", "p = 0.1"), 
           text.col = c("red", "blue"), bty = "n")
    mtext(side = 3, "(c)", at = 0)
  }
  else {
    plot(tp_grid, vals_1, type = "n", xlab = expression(n[c]), ylab = "metric",
         main = "domias MIA", col = "black", xaxt = "n", yaxt = "n")
    mtext(side = 3, "(c)", at = 0)
  }
  
  ## dcr panel
  if (!is.null(file_name_dcr_1)) {
    dcrs_1 <- read.csv(paste0(out_path_dcr_1, file_name_dcr_1))
    dcrs_2 <- read.csv(paste0(out_path_dcr_2, file_name_dcr_2))
    
    names(dcrs_1) <- dcr_labels[-1]
    names(dcrs_2) <- dcr_labels[-1]
    boxplot(dcrs_1[, -1], outline = FALSE, las = 2, ylab = "DCR", 
            main = "DCR distributions", border = rgb(1, 0, 0, 0.5),
            col = "white")
    boxplot(dcrs_2[, -1], outline = FALSE, las = 2, ylab = "DCR", 
            main = "DCR distributions", border = rgb(0, 0, 1, 0.5), 
            at = seq(24)+0.2, add = TRUE, xaxt = "n",
            col = "white")
    mtext(side = 3, "(d)", at = 0)
    legend("topright", legend = c("p = 0.5", "p = 0.1"), 
           text.col = c("red", "blue"), bty = "n")
  }
  
}


my_layout_matrix <- matrix(c(1, 2, 3, 4, 4), nr = 1, nc = 5)

manus_path <- ""

out_path_benchmark_1 <- "~/outputs/tuning_param_selection_SynTabDS/"
out_path_dcr_1 <- "~/outputs/tuning_param_dcr/"

out_path_benchmark_2 <- "~/outputs/tuning_param_selection_SynTabDS_0.1/"
out_path_dcr_2 <- "~/outputs/tuning_param_dcr_0.1/"

par_grid <- c(seq(5, 50, by = 5), seq(60, 100, by = 10), seq(200, 1000, by = 100))
dcr_labels <- c("test", par_grid)

my_mar <- c(2.5, 2.5, 1.1, 0.1)
my_mgp <- c(1.5, 0.5, 0)


#pdf(paste0(manus_path, "compare_p_AB.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_abalone.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_AB.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_abalone_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_AB_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1,
                  file_name_dcr_1,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2,
                  file_name_dcr_2,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_AD.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_adult.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_AD.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_adult_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_AD_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1 = NULL,
                  file_name_dcr_1 = NULL,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2 = NULL,
                  file_name_dcr_2 = NULL,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_BM.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_bank_marketing.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_BM.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_bank_marketing_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_BM_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1,
                  file_name_dcr_1,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2,
                  file_name_dcr_2,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_CH.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_california_orig.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_CH.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_california_orig_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_CH_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1,
                  file_name_dcr_1,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2,
                  file_name_dcr_2,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_CR.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_credit.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_CR.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_credit_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_CR_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1,
                  file_name_dcr_1,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2,
                  file_name_dcr_2,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_DI.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_diabetes130us.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_DI.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_diabetes130us_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_DI_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1,
                  file_name_dcr_1,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2,
                  file_name_dcr_2,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_EL.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_electricity.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_EL.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_electricity_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_EL_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1,
                  file_name_dcr_1,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2,
                  file_name_dcr_2,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_EM.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_eye_movement.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_EM.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_eye_movement_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_EM_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1,
                  file_name_dcr_1,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2,
                  file_name_dcr_2,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_HO.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_house_16h.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_HO.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_house_16h_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_HO_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1,
                  file_name_dcr_1,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2,
                  file_name_dcr_2,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_MT.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_magic_telescope.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_MT.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_magic_telescope_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_MT_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1,
                  file_name_dcr_1,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2,
                  file_name_dcr_2,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_MU.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_mushroom.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_MU.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_mushroom_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_MU_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1 = NULL,
                  file_name_dcr_1 = NULL,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2 = NULL,
                  file_name_dcr_2 = NULL,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "compare_p_PO.pdf"), width = 8, height = 1.8)
file_name_benchmark_1 <- "mean_tuning_param_selection_pol.csv"
file_name_dcr_1 <- "syn_tab_sjppds_dcr_PO.csv"
file_name_benchmark_2 <- "mean_tuning_param_selection_pol_0.1.csv"
file_name_dcr_2 <- "syn_tab_sjppds_dcr_PO_0.1.csv"
CompareParameterP(out_path_benchmark_1,
                  file_name_benchmark_1,
                  out_path_dcr_1,
                  file_name_dcr_1,
                  out_path_benchmark_2,
                  file_name_benchmark_2,
                  out_path_dcr_2,
                  file_name_dcr_2,
                  my_layout_matrix = my_layout_matrix,
                  my_mar = my_mar,
                  my_mgp = my_mgp,
                  dcr_labels = dcr_labels)
#dev.off()

