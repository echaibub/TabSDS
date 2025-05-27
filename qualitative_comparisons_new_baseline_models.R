
code_path <- "" ## set path for folder storing the code
source(paste0(code_path, "utility_functions_for_SynTabSJPPDS.R"))


library(corrplot)

L2DistAssociationMatrix <- function(am1, am2) {
  return(mean((am1 - am2)^2))
}


DeltaAssociationMatrixPlot2 <- function(data_set_names,
                                        folder_path_part1,
                                        folder_path_part2,
                                        n_c_values,
                                        num_variables_list,
                                        cat_variables_list,
                                        my_mar,
                                        data_labels,
                                        label_cex = 1) {
  
  num_datasets <- length(data_set_names)
  
  par(mfrow = c(num_datasets, 5), mar = my_mar)
  
  for (i in seq(num_datasets)) {
    data_set_name <- data_set_names[i]
    n_c <- n_c_values[i]
    num_variables <- num_variables_list[[i]]
    cat_variables <- cat_variables_list[[i]]
    data_label <- data_labels[i]
    
    ## load the data
    data_path <- paste0(folder_path_part1, data_set_name, folder_path_part2)
    dat_train <- read.csv(paste0(data_path, "train_set.csv"))
    sel_tabsds <- paste0("syn_tab_sjppds_", n_c, "_0.5.csv")
    dat_tabsds <- read.csv(paste0(data_path, sel_tabsds))
    
    dat_smote5 <- read.csv(paste0(data_path, "syn_smote_k5.csv"))
    dat_smote20 <- read.csv(paste0(data_path, "syn_smote_k20.csv"))
    dat_adsgan <- read.csv(paste0(data_path, "syn_adsgan.csv"))
    dat_pategan <- read.csv(paste0(data_path, "syn_pategan.csv"))
    
    ## compute association matrices
    am_train <- ComputeAssociationMatrix(dat = dat_train,
                                         num_variables,
                                         cat_variables)
    am_tabsds <- ComputeAssociationMatrix(dat = dat_tabsds,
                                          num_variables,
                                          cat_variables)
    am_smote5 <- ComputeAssociationMatrix(dat = dat_smote5,
                                          num_variables,
                                          cat_variables)
    am_smote20 <- ComputeAssociationMatrix(dat = dat_smote20,
                                           num_variables,
                                           cat_variables)
    am_adsgan <- ComputeAssociationMatrix(dat = dat_adsgan,
                                          num_variables,
                                          cat_variables)
    am_pategan <- ComputeAssociationMatrix(dat = dat_pategan,
                                           num_variables,
                                           cat_variables)
    
    ## compute deltas between synthetic and training 
    ## association matrices
    delta_tabsds <- abs(am_train - am_tabsds)
    delta_smote5 <- abs(am_train - am_smote5)
    delta_smote20 <- abs(am_train - am_smote20)
    delta_adsgan <- abs(am_train - am_adsgan)
    delta_pategan <- abs(am_train - am_pategan)
    
    dimnames(delta_tabsds) <- NULL
    dimnames(delta_smote5) <- NULL
    dimnames(delta_smote20) <- NULL
    dimnames(delta_adsgan) <- NULL
    dimnames(delta_pategan) <- NULL
    
    ## compute L2 distances between training and synthetic data association
    ## matrices
    l2_tabsds <- L2DistAssociationMatrix(am_train, am_tabsds)
    l2_smote5 <- L2DistAssociationMatrix(am_train, am_smote5)
    l2_smote20 <- L2DistAssociationMatrix(am_train, am_smote20)
    l2_adsgan <- L2DistAssociationMatrix(am_train, am_adsgan)
    l2_pategan <- L2DistAssociationMatrix(am_train, am_pategan)
    
    ## plot the delta association matrices
    p <- ncol(dat_train)
    
    image(t(delta_tabsds)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("TabSDS, l2d = ", sprintf("%.3f", l2_tabsds)))
    mtext(side = 2, data_label, cex = label_cex)
    
    image(t(delta_smote5)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("SMOTE_5, l2d = ", sprintf("%.3f", l2_smote5)))
    mtext(side = 2, data_label, cex = label_cex)
    
    image(t(delta_smote20)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("SMOTE_20, l2d = ", sprintf("%.3f", l2_smote20)))
    mtext(side = 2, data_label, cex = label_cex)
    
    image(t(delta_adsgan)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("ADSGAN, l2d = ", sprintf("%.3f", l2_adsgan)))
    mtext(side = 2, data_label, cex = label_cex)
    
    image(t(delta_pategan)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("PATEGAN, l2d = ", sprintf("%.3f", l2_pategan)))
    mtext(side = 2, data_label, cex = label_cex)
  }
  
}


MarginalDensityPlotsQC2 <- function(var_name,
                                    dat_train,
                                    dat_tabsds,
                                    dat_smote5,
                                    dat_smote20,
                                    dat_adsgan,
                                    dat_pategan,
                                    leg_pos = "topright") {
  
  densi_train <- density(dat_train[, var_name])
  densi_syn <- vector(mode = "list", length = 5)
  densi_syn[[1]] <- density(dat_tabsds[, var_name])
  densi_syn[[2]] <- density(dat_smote5[, var_name])
  densi_syn[[3]] <- density(dat_smote20[, var_name])
  densi_syn[[4]] <- density(dat_adsgan[, var_name])
  densi_syn[[5]] <- density(dat_pategan[, var_name])
  
  methods_names <- c("TabSDS", 
                     "SMOTE_5", 
                     "SMOTE_20", 
                     "ADSGAN", 
                     "PATEGAN")
  
  for (i in seq(5)) {
    my_ylim <- c(0, max(densi_train$y, densi_syn[[i]]$y))
    lab_at <- min(densi_train$x)
    
    plot(densi_train$x, densi_train$y, type = "l", 
         xlab = var_name, ylab = "density", 
         main = methods_names[i], lwd = 2,
         col = "blue", ylim = my_ylim)
    lines(densi_syn[[i]]$x, densi_syn[[i]]$y, type = "l", col = "red")
    legend(leg_pos, legend = c("synth", "real"), 
           text.col = c("red", "blue"), bty = "n")
  }
}


MergeMarginalTables <- function(tb_train, tb_syn) {
  cat_levels <- names(tb_train)
  num_levels <- length(cat_levels)
  out <- matrix(NA, 2, num_levels)
  colnames(out) <- cat_levels
  out[1,] <- as.numeric(tb_train)
  for (i in seq(num_levels)) {
    out[2, i] <- tb_syn[cat_levels[i]]
  }
  return(out)
}


MarginalBarPlotsQC2 <- function(var_name,
                                dat_train,
                                dat_tabsds,
                                dat_smote5,
                                dat_smote20,
                                dat_adsgan,
                                dat_pategan,
                                leg_pos = "topright") {
  MergeMarginalTables <- function(tb_train, tb_syn) {
    cat_levels <- names(tb_train)
    num_levels <- length(cat_levels)
    out <- matrix(NA, 2, num_levels)
    colnames(out) <- cat_levels
    out[1,] <- as.numeric(tb_train)
    for (i in seq(num_levels)) {
      out[2, i] <- tb_syn[cat_levels[i]]
    }
    return(out)
  }
  
  tb_train <- table(dat_train[, var_name])
  tb_syn <- vector(mode = "list", length = 6)
  tb_syn[[1]] <- table(dat_tabsds[, var_name])
  tb_syn[[2]] <- table(dat_smote5[, var_name])
  tb_syn[[3]] <- table(dat_smote20[, var_name])
  tb_syn[[4]] <- table(dat_adsgan[, var_name])
  tb_syn[[5]] <- table(dat_pategan[, var_name])
  
  methods_names <- c("TabSDS", 
                     "SMOTE_5", 
                     "SMOTE_20", 
                     "ADSGAN", 
                     "PATEGAN")
  
  for (i in seq(6)) {
    aux <- MergeMarginalTables(tb_train, tb_syn[[i]])
    
    barplot(aux, beside = TRUE, col = c("blue", "red"), 
            xlab = var_name, ylab = "frequency",
            main = methods_names[i])
    
    legend(leg_pos, legend = c("synth", "real"), 
           text.col = c("red", "blue"), bty = "n")
  }
}


QualComparMargDistr2 <- function(data_set_name,
                                 folder_path_part1,
                                 folder_path_part2,
                                 sel_num_variables,
                                 sel_cat_variables,
                                 n_c,
                                 my_mar,
                                 my_mgp) {
  
  ## load the data
  data_path <- paste0(folder_path_part1, 
                      data_set_name, folder_path_part2)
  dat_train <- read.csv(paste0(data_path, "train_set.csv"))
  sel_tabsds <- paste0("syn_tab_sjppds_", n_c, "_0.5.csv")
  dat_tabsds <- read.csv(paste0(data_path, sel_tabsds))
  dat_smote5 <- read.csv(paste0(data_path, "syn_smote_k5.csv"))
  dat_smote20 <- read.csv(paste0(data_path, "syn_smote_k20.csv"))
  dat_adsgan <- read.csv(paste0(data_path, "syn_adsgan.csv"))
  dat_pategan <- read.csv(paste0(data_path, "syn_pategan.csv"))
  
  len_num_vars <- length(sel_num_variables)
  len_cat_vars <- length(sel_cat_variables)
  
  len_vars <- len_num_vars + len_cat_vars
  
  par(mfrow = c(len_vars, 5), mar = my_mar, mgp = my_mgp)
  
  nms <- colnames(dat_train)
  
  if (len_num_vars > 0) {
    for (i in seq(len_num_vars)) {
      MarginalDensityPlotsQC2(var_name = nms[sel_num_variables[i]],
                              dat_train,
                              dat_tabsds,
                              dat_smote5,
                              dat_smote20,
                              dat_adsgan,
                              dat_pategan)
    }
  }
  
  if (len_cat_vars > 0) {
    for (j in seq(len_cat_vars)) {
      MarginalBarPlotsQC2(var_name = nms[sel_cat_variables[j]],
                          dat_train,
                          dat_tabsds,
                          dat_smote5,
                          dat_smote20,
                          dat_adsgan,
                          dat_pategan)
    }
  }
}


############################################################
############################################################
############################################################

## This script assumes that the data has been stored in a folder 
## structure: "~/outputs/<data set name>/simulated_datasets/"

folder_path_part1 <- "~/outputs/"
folder_path_part2 <- "/simulated_datasets/"

data_set_names <- c("abalone",
                    "bank_marketing",
                    "california_housing_original",
                    "credit",
                    "diabetes_130US",
                    "electricity",
                    "eye_movement",
                    "house_16h",
                    "magic_telescope",
                    "pol")

data_labels <- c("Abalone (AB)",
                 "Bank mark. (BM)",
                 "Cal. hous. (CH)",
                 "Credit (CR)",
                 "Diabetes (DI)",
                 "Electric. (EL)",
                 "Eye mov. (EM)",
                 "House 16H (HO)",
                 "Magic tel. (MT)",
                 "Pol (PO)")

n_c_values <- c(20,
                100,
                200,
                1000,
                35, 
                20, 
                20,
                1000,
                25,
                15)


num_variables_list <- vector(mode = "list", length = 10)
cat_variables_list <- vector(mode = "list", length = 10)

## abalone
num_variables_list[[1]] <- seq(2, 9)
cat_variables_list[[1]] <- 1

## bank marketing
num_variables_list[[2]] <- seq(7)
cat_variables_list[[2]] <- 8

## calif. hous.
num_variables_list[[3]] <- seq(9)
cat_variables_list[[3]] <- NULL

## credit
num_variables_list[[4]] <- seq(10)
cat_variables_list[[4]] <- 11

## diabetes
num_variables_list[[5]] <- seq(7)
cat_variables_list[[5]] <- 8

## electricity
num_variables_list[[6]] <- seq(7)
cat_variables_list[[6]] <- 8

## eye movements
num_variables_list[[7]] <- seq(20)
cat_variables_list[[7]] <- 21

## house 16h
num_variables_list[[8]] <- seq(16)
cat_variables_list[[8]] <- 17

## magic telescope
num_variables_list[[9]] <- seq(10)
cat_variables_list[[9]] <- 11

## pol
num_variables_list[[10]] <- seq(26)
cat_variables_list[[10]] <- 27


my_mar <- c(1.2, 1.2, 1, 0.2)

manus_path <- ""

pdf(paste0(manus_path, "delta_assoc_new_models.pdf"), width = 10, height = 13)
DeltaAssociationMatrixPlot2(data_set_names,
                            folder_path_part1,
                            folder_path_part2,
                            n_c_values,
                            num_variables_list,
                            cat_variables_list,
                            my_mar,
                            data_labels,
                            label_cex = 0.75)
dev.off()


my_mar <- c(3, 3, 1, 0.5)
my_mgp <- c(1.75, 0.75, 0)

n_var <- 7


for (k in seq(1, 10)) {
  fname <- paste0("marg_distr_", data_set_names[k], "_new_baseline_models.pdf")
  pdf(paste0(manus_path, fname), width = 8, height = 10)
  QualComparMargDistr2(data_set_name = data_set_names[k],
                       folder_path_part1,
                       folder_path_part2,
                       sel_num_variables = num_variables_list[[k]][1:n_var],
                       sel_cat_variables = NULL,
                       n_c = n_c_values[k],
                       my_mar = my_mar,
                       my_mgp = my_mgp)
  dev.off()
}

