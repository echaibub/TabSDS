
code_path <- "" ## set path for folder storing the code
source(paste0(code_path, "utility_functions_for_SynTabSJPPDS.R"))

library(corrplot)

L2DistAssociationMatrix <- function(am1, am2) {
  return(mean((am1 - am2)^2))
}


DeltaAssociationMatrixPlot <- function(data_set_names,
                                       folder_path_part1,
                                       folder_path_part2,
                                       n_c_values,
                                       num_variables_list,
                                       cat_variables_list,
                                       my_mar,
                                       data_labels,
                                       label_cex = 1) {
  
  num_datasets <- length(data_set_names)
  
  par(mfrow = c(num_datasets, 6), mar = my_mar)
  
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
    dat_ddpm <- read.csv(paste0(data_path, "syn_ddpm.csv"))
    dat_arf <- read.csv(paste0(data_path, "syn_arf.csv"))
    dat_tvae <- read.csv(paste0(data_path, "syn_tvae.csv"))
    dat_ctgan <- read.csv(paste0(data_path, "syn_ctgan.csv"))
    dat_bayesnet <- read.csv(paste0(data_path, "syn_bayesnet.csv"))
    
    ## compute association matrices
    am_train <- ComputeAssociationMatrix(dat = dat_train,
                                         num_variables,
                                         cat_variables)
    am_tabsds <- ComputeAssociationMatrix(dat = dat_tabsds,
                                          num_variables,
                                          cat_variables)
    am_ddpm <- ComputeAssociationMatrix(dat = dat_ddpm,
                                        num_variables,
                                        cat_variables)
    am_arf <- ComputeAssociationMatrix(dat = dat_arf,
                                       num_variables,
                                       cat_variables)
    am_tvae <- ComputeAssociationMatrix(dat = dat_tvae,
                                        num_variables,
                                        cat_variables)
    am_ctgan <- ComputeAssociationMatrix(dat = dat_ctgan,
                                         num_variables,
                                         cat_variables)
    am_bayesnet <- ComputeAssociationMatrix(dat = dat_bayesnet,
                                            num_variables,
                                            cat_variables)
    
    ## compute deltas between synthetic and training 
    ## association matrices
    delta_tabsds <- abs(am_train - am_tabsds)
    delta_ddpm <- abs(am_train - am_ddpm)
    delta_arf <- abs(am_train - am_arf)
    delta_tvae <- abs(am_train - am_tvae)
    delta_ctgan <- abs(am_train - am_ctgan)
    delta_bayesnet <- abs(am_train - am_bayesnet)
    
    dimnames(delta_tabsds) <- NULL
    dimnames(delta_ddpm) <- NULL
    dimnames(delta_arf) <- NULL
    dimnames(delta_tvae) <- NULL
    dimnames(delta_ctgan) <- NULL
    dimnames(delta_bayesnet) <- NULL
    
    ## compute L2 distances between training and synthetic data association
    ## matrices
    l2_tabsds <- L2DistAssociationMatrix(am_train, am_tabsds)
    l2_ddpm <- L2DistAssociationMatrix(am_train, am_ddpm)
    l2_arf <- L2DistAssociationMatrix(am_train, am_arf)
    l2_tvae <- L2DistAssociationMatrix(am_train, am_tvae)
    l2_ctgan <- L2DistAssociationMatrix(am_train, am_ctgan)
    l2_bayesnet <- L2DistAssociationMatrix(am_train, am_bayesnet)
    
    ## plot the delta association matrices
    p <- ncol(dat_train)
    
    image(t(delta_tabsds)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("TabSDS, l2d = ", sprintf("%.3f", l2_tabsds)))
    mtext(side = 2, data_label, cex = label_cex)
    
    image(t(delta_ddpm)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("ddpm, l2d = ", sprintf("%.3f", l2_ddpm)))
    mtext(side = 2, data_label, cex = label_cex)
    
    image(t(delta_arf)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("arf, l2d = ", sprintf("%.3f", l2_arf)))
    mtext(side = 2, data_label, cex = label_cex)
    
    image(t(delta_tvae)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("tvae, l2d = ", sprintf("%.3f", l2_tvae)))
    mtext(side = 2, data_label, cex = label_cex)
    
    image(t(delta_ctgan)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("ctgan, l2d = ", sprintf("%.3f", l2_ctgan)))
    mtext(side = 2, data_label, cex = label_cex)
    
    image(t(delta_bayesnet)[, p:1], zlim = c(0, 1), axes = FALSE,
          col = hcl.colors(56, "YlOrRd", rev = TRUE),
          main = paste("bayesnet, l2d = ", sprintf("%.3f", l2_bayesnet)))
    mtext(side = 2, data_label, cex = label_cex)
    
  }
  
}

## This script assumes that the data has been stored in a folder 
## structure: "~/outputs/<data set name>/simulated_datasets/"

folder_path_part1 <- "~/outputs/"
folder_path_part2 <- "/simulated_datasets/"

data_set_names <- c("abalone",
                    "adult",
                    "bank_marketing",
                    "california_housing_original",
                    "credit",
                    "diabetes_130US",
                    "electricity",
                    "eye_movement",
                    "house_16h",
                    "magic_telescope",
                    "mushroom",
                    "pol")

data_labels <- c("Abalone (AB)",
                 "Adult (AD)",
                 "Bank mark. (BM)",
                 "Cal. hous. (CH)",
                 "Credit (CR)",
                 "Diabetes (DI)",
                 "Electric. (EL)",
                 "Eye mov. (EM)",
                 "House 16H (HO)",
                 "Magic tel. (MT)",
                 "Mushroom (MU)",
                 "Pol (PO)")

n_c_values <- c(20,
                20,
                100,
                200,
                1000,
                35, 
                20, 
                20,
                1000,
                25,
                40, 
                15)

num_variables_list <- vector(mode = "list", length = 12)
cat_variables_list <- vector(mode = "list", length = 12)

## abalone
num_variables_list[[1]] <- seq(2, 9)
cat_variables_list[[1]] <- 1

## adult
num_variables_list[[2]] <- c(3, 5)
cat_variables_list[[2]] <- seq(15)[-c(3, 5)]

## bank marketing
num_variables_list[[3]] <- seq(7)
cat_variables_list[[3]] <- 8

## calif. hous.
num_variables_list[[4]] <- seq(9)
cat_variables_list[[4]] <- NULL

## credit
num_variables_list[[5]] <- seq(10)
cat_variables_list[[5]] <- 11

## diabetes
num_variables_list[[6]] <- seq(7)
cat_variables_list[[6]] <- 8

## electricity
num_variables_list[[7]] <- seq(7)
cat_variables_list[[7]] <- 8

## eye movements
num_variables_list[[8]] <- seq(20)
cat_variables_list[[8]] <- 21

## house 16h
num_variables_list[[9]] <- seq(16)
cat_variables_list[[9]] <- 17

## magic telescope
num_variables_list[[10]] <- seq(10)
cat_variables_list[[10]] <- 11

## mushroom
num_variables_list[[11]] <- NULL
cat_variables_list[[11]] <- seq(21)

## pol
num_variables_list[[12]] <- seq(26)
cat_variables_list[[12]] <- 27


my_mar <- c(1.2, 1.2, 1, 0.2)

manus_path <- ""

#pdf(paste0(manus_path, "delta_assoc_all.pdf"), width = 10, height = 13)
DeltaAssociationMatrixPlot(data_set_names,
                           folder_path_part1,
                           folder_path_part2,
                           n_c_values,
                           num_variables_list,
                           cat_variables_list,
                           my_mar,
                           data_labels,
                           label_cex = 0.75)
#dev.off()

