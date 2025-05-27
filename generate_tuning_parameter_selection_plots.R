
TuningParamSelPlot <- function(out_path_benchmark,
                               file_name_benchmark,
                               out_path_dcr = NULL,
                               file_name_dcr = NULL,
                               my_layout_matrix,
                               my_mar = c(5, 4, 2, 2),
                               my_mgp = c(3, 1, 0),
                               sel_param = NULL,
                               dcr_labels) {
  
  par(mar = my_mar, mgp = my_mgp)
  
  ## benchmark panels
  means <- read.csv(paste0(out_path_benchmark, file_name_benchmark))
  nms <- colnames(means)[-1]
  tp_grid <- as.numeric(unlist(lapply(strsplit(nms, "_"), function(x) x[2])))
  
  metric_names <- means$X
  
  means <- means[, -1]
  rownames(means) <- metric_names
  
  if (!is.null(sel_param)) {
    idx_sel <- which(tp_grid == sel_param)
  }
  
  layout(my_layout_matrix)
  
  vals <- as.numeric(means["detection.detection_xgb.mean",])
  plot(tp_grid, vals, type = "b", xlab = expression(n[c]), ylab = "metric",
       main = "detection test")
  if (!is.null(sel_param)) {
    points(tp_grid[idx_sel], vals[idx_sel], col = "red", pch = 19)
  }
  mtext(side = 3, "(a)", at = 0)
  
  val_gt <- means["performance.xgb.gt", 1]
  vals_ood <- as.numeric(means["performance.xgb.syn_ood",])
  my_ylim <- c(min(c(val_gt, vals_ood)),
               max(c(val_gt, vals_ood)))
  plot(tp_grid, vals_ood, type = "b", xlab = expression(n[c]), ylab = "metric",
       main = "ML efficiency", ylim = my_ylim, col = "black")
  abline(h = val_gt, col = "grey")
  if (!is.null(sel_param)) {
    points(tp_grid[idx_sel], vals_ood[idx_sel], col = "red", pch = 19)
  }
  mtext(side = 3, "(b)", at = 0)
  
  if ("privacy.DomiasMIA_prior.aucroc" %in% metric_names) {
    vals <- as.numeric(means["privacy.DomiasMIA_prior.aucroc",])
    plot(tp_grid, vals, type = "b", xlab = expression(n[c]), ylab = "metric",
         main = "Domias MIA", col = "black")
    if (!is.null(sel_param)) {
      points(tp_grid[idx_sel], vals[idx_sel], col = "red", pch = 19)
    }
  }
  else {
    plot(tp_grid, vals, type = "n", xlab = expression(n[c]), ylab = "metric",
         main = "domias MIA", col = "black", xaxt = "n", yaxt = "n")
    mtext(side = 3, "(c)", at = 0)
  }
  
  ## dcr panel
  if (!is.null(file_name_dcr)) {
    dcrs <- read.csv(paste0(out_path_dcr, file_name_dcr))
    names(dcrs) <- dcr_labels
    boxplot(dcrs, outline = FALSE, las = 2, ylab = "DCR", 
            main = "DCR distributions", col = c("grey", rep("white", 24)))
    abline(h = median(dcrs[, 1]), col = "red")
    mtext(side = 3, "(d)", at = 0)
  }
  
}





manus_path <- ""

my_layout_matrix <- matrix(c(1, 2, 3, 4, 4), nr = 1, nc = 5)

out_path_benchmark <- "~/outputs/tuning_param_selection_SynTabDS/"
out_path_dcr <- "~/outputs/tuning_param_dcr/"

par_grid <- c(seq(5, 50, by = 5), seq(60, 100, by = 10), seq(200, 1000, by = 100))
dcr_labels <- c("test", par_grid)

my_mar <- c(2.5, 2.5, 1.1, 0.1)
my_mgp <- c(1.5, 0.5, 0)



#pdf(paste0(manus_path, "sel_plot_AB.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_abalone.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_AB.csv"
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 20,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_AD.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_adult.csv"
file_name_dcr <- NULL
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 20,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_BM.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_bank_marketing.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_BM.csv"
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 100,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_CH.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_california_orig.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_CH.csv"
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 200,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_CR.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_credit.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_CR.csv"
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 1000,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_DI.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_diabetes130us.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_DI.csv"
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 35,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_EL.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_electricity.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_EL.csv"
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 25,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_EM.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_eye_movement.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_EM.csv"
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 20,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_HO.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_house_16h.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_HO.csv"
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 1000,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_MT.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_magic_telescope.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_MT.csv"
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 25,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_MU.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_mushroom.csv"
file_name_dcr <- NULL
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 40,
                   dcr_labels = dcr_labels)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_PO.pdf"), width = 8, height = 1.8)
file_name_benchmark <- "mean_tuning_param_selection_pol.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_PO.csv"
TuningParamSelPlot(out_path_benchmark,
                   file_name_benchmark,
                   out_path_dcr,
                   file_name_dcr,
                   my_layout_matrix = my_layout_matrix,
                   my_mar = my_mar,
                   my_mgp = my_mgp,
                   sel_param = 15,
                   dcr_labels = dcr_labels)
#dev.off()




#####################################
#####################################
#####################################

## main text

BenchmarkPlot <- function(out_path_benchmark,
                          file_name_benchmark,
                          my_mfrow,
                          my_mar = c(5, 4, 2, 2),
                          my_mgp = c(3, 1, 0)) {
  
  par(mfrow = my_mfrow, mar = my_mar, mgp = my_mgp)
  
  ## benchmark panels
  means <- read.csv(paste0(out_path_benchmark, file_name_benchmark))
  nms <- colnames(means)[-1]
  tp_grid <- as.numeric(unlist(lapply(strsplit(nms, "_"), function(x) x[2])))
  
  metric_names <- means$X
  
  means <- means[, -1]
  rownames(means) <- metric_names
  
  vals <- as.numeric(means["detection.detection_xgb.mean",])
  plot(tp_grid, vals, type = "b", xlab = expression(n[c]), ylab = "metric",
       main = "Detection test")
  legend("topright", legend = "(a)", bty = "n", cex = 1.2)
  
  val_gt <- means["performance.xgb.gt", 1]
  vals_ood <- as.numeric(means["performance.xgb.syn_ood",])
  my_ylim <- c(min(c(val_gt, vals_ood)),
               max(c(val_gt, vals_ood)))
  plot(tp_grid, vals_ood, type = "b", xlab = expression(n[c]), ylab = "metric",
       main = "ML efficiency", ylim = my_ylim, col = "black")
  abline(h = val_gt, col = "grey")
  legend("bottomright", legend = "(b)", bty = "n", cex = 1.2)
  
  vals <- as.numeric(means["privacy.DomiasMIA_prior.aucroc",])
  plot(tp_grid, vals, type = "b", xlab = expression(n[c]), ylab = "metric",
       main = "Domias MIA", col = "black")
  legend("bottomright", legend = "(c)", bty = "n", cex = 1.2)
  
}

#pdf(paste0(manus_path, "sel_plot_CH_main.pdf"), width = 4, height = 1.4)
file_name_benchmark <- "mean_tuning_param_selection_california_orig.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_CH.csv"
BenchmarkPlot(out_path_benchmark,
              file_name_benchmark,
              my_mfrow = c(1, 3),
              my_mar = my_mar,
              my_mgp = my_mgp)
#dev.off()


#pdf(paste0(manus_path, "sel_plot_EL_main.pdf"), width = 4, height = 1.4)
file_name_benchmark <- "mean_tuning_param_selection_electricity.csv"
file_name_dcr <- "syn_tab_sjppds_dcr_EL.csv"
BenchmarkPlot(out_path_benchmark,
              file_name_benchmark,
              my_mfrow = c(1, 3),
              my_mar = my_mar,
              my_mgp = my_mgp)
#dev.off()

out_path_dcr_1 <- out_path_dcr
out_path_dcr_2 <- out_path_dcr
file_name_dcr_1 <- "syn_tab_sjppds_dcr_CH.csv" 
file_name_dcr_2 <- "syn_tab_sjppds_dcr_EL.csv"

par_grid <- c(seq(5, 50, by = 5), seq(60, 100, by = 10), seq(200, 1000, by = 100))

dcrs1 <- read.csv(paste0(out_path_dcr_1, file_name_dcr_1))
names(dcrs1) <- c("test", par_grid)
dcrs2 <- read.csv(paste0(out_path_dcr_2, file_name_dcr_2))
names(dcrs2) <- c("test", par_grid)

#pdf(paste0(manus_path, "dcr_distr_main.pdf"), width = 8, height = 2.5)
par(mfrow = c(1, 2), mar = c(2.5, 2.5, 1, 0.1), mgp = c(1.5, 0.5, 0))
boxplot(dcrs1, outline = FALSE, las = 2, ylab = "DCR", 
        main = "DCR distributions (CH)", cex.main = 1.5, 
        col = c("grey", rep("white", 24)))
abline(h = median(dcrs1[, 1]), col = "red")
mtext(side = 3, "(a)", at = 0, cex = 1.5)
boxplot(dcrs2, outline = FALSE, las = 2, ylab = "", 
        main = "DCR distributions (EL)", cex.main = 1.5, 
        col = c("grey", rep("white", 24)))
abline(h = median(dcrs2[, 1]), col = "red")
mtext(side = 3, "(b)", at = 0, cex = 1.5)
#dev.off()


