
MarginalDensityPlotsQC <- function(var_name,
                                   dat_train,
                                   dat_tabsds,
                                   dat_ddpm,
                                   dat_arf,
                                   dat_tvae,
                                   dat_ctgan,
                                   dat_bayesnet,
                                   leg_pos = "topright") {
  
  densi_train <- density(dat_train[, var_name])
  densi_syn <- vector(mode = "list", length = 6)
  densi_syn[[1]] <- density(dat_tabsds[, var_name])
  densi_syn[[2]] <- density(dat_ddpm[, var_name])
  densi_syn[[3]] <- density(dat_arf[, var_name])
  densi_syn[[4]] <- density(dat_tvae[, var_name])
  densi_syn[[5]] <- density(dat_ctgan[, var_name])
  densi_syn[[6]] <- density(dat_bayesnet[, var_name])
  
  methods_names <- c("TabSDS", 
                     "ddpm", 
                     "arf", 
                     "tvae", 
                     "ctgan", 
                     "bayesnet")
  
  for (i in seq(6)) {
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


MarginalBarPlotsQC <- function(var_name,
                               dat_train,
                               dat_tabsds,
                               dat_ddpm,
                               dat_arf,
                               dat_tvae,
                               dat_ctgan,
                               dat_bayesnet,
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
  tb_syn[[2]] <- table(dat_ddpm[, var_name])
  tb_syn[[3]] <- table(dat_arf[, var_name])
  tb_syn[[4]] <- table(dat_tvae[, var_name])
  tb_syn[[5]] <- table(dat_ctgan[, var_name])
  tb_syn[[6]] <- table(dat_bayesnet[, var_name])
  
  methods_names <- c("TabSDS", 
                     "ddpm", 
                     "arf", 
                     "tvae", 
                     "ctgan", 
                     "bayesnet")
  
  for (i in seq(6)) {
    aux <- MergeMarginalTables(tb_train, tb_syn[[i]])
    
    barplot(aux, beside = TRUE, col = c("blue", "red"), 
            xlab = var_name, ylab = "frequency",
            main = methods_names[i])
    
    legend(leg_pos, legend = c("synth", "real"), 
           text.col = c("red", "blue"), bty = "n")
  }
}


QualComparMargDistr <- function(data_set_name,
                                folder_path_part1,
                                folder_path_part2,
                                sel_num_variables,
                                sel_cat_variables,
                                n_c,
                                my_mar,
                                my_mgp) {
  
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
  
  len_num_vars <- length(sel_num_variables)
  len_cat_vars <- length(sel_cat_variables)
  
  len_vars <- len_num_vars + len_cat_vars
  
  par(mfrow = c(len_vars, 6), mar = my_mar, mgp = my_mgp)
  
  nms <- colnames(dat_train)
  
  if (len_num_vars > 0) {
    for (i in seq(len_num_vars)) {
      MarginalDensityPlotsQC(var_name = nms[sel_num_variables[i]],
                             dat_train,
                             dat_tabsds,
                             dat_ddpm,
                             dat_arf,
                             dat_tvae,
                             dat_ctgan,
                             dat_bayesnet)
    }
  }
  
  if (len_cat_vars > 0) {
    for (j in seq(len_cat_vars)) {
      MarginalBarPlotsQC(var_name = nms[sel_cat_variables[j]],
                         dat_train,
                         dat_tabsds,
                         dat_ddpm,
                         dat_arf,
                         dat_tvae,
                         dat_ctgan,
                         dat_bayesnet)
    }
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

num_variables_list <- vector(mode = "list", length = 11)
cat_variables_list <- vector(mode = "list", length = 11)

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


manus_path <- ""

my_mar <- c(3, 3, 1, 0.5)
my_mgp <- c(1.75, 0.75, 0)

n_var <- 7

k <- 1
#pdf(paste0(manus_path, "marg_distr_abalone.pdf"), width = 8, height = 10)
QualComparMargDistr(data_set_name = data_set_names[k],
                    folder_path_part1,
                    folder_path_part2,
                    sel_num_variables = num_variables_list[[k]][1:n_var],
                    sel_cat_variables = NULL,
                    n_c = n_c_values[k],
                    my_mar = my_mar,
                    my_mgp = my_mgp)
#dev.off()

k <- 2
#pdf(paste0(manus_path, "marg_distr_adult.pdf"), width = 8, height = 10)
QualComparMargDistr(data_set_name = data_set_names[k],
                    folder_path_part1,
                    folder_path_part2,
                    sel_num_variables = num_variables_list[[k]][1:2],
                    sel_cat_variables = cat_variables_list[[k]][1:5],
                    n_c = n_c_values[k],
                    my_mar = my_mar,
                    my_mgp = my_mgp)
#dev.off()


for (k in seq(3, 10)) {
  fname <- paste0("marg_distr_", data_set_names[k], ".pdf")
  #pdf(paste0(manus_path, fname), width = 8, height = 10)
  QualComparMargDistr(data_set_name = data_set_names[k],
                      folder_path_part1,
                      folder_path_part2,
                      sel_num_variables = num_variables_list[[k]][1:n_var],
                      sel_cat_variables = NULL,
                      n_c = n_c_values[k],
                      my_mar = my_mar,
                      my_mgp = my_mgp)
  #dev.off()
}


k <- 11
#pdf(paste0(manus_path, "marg_distr_mushroom.pdf"), width = 8, height = 10)
QualComparMargDistr(data_set_name = data_set_names[k],
                    folder_path_part1,
                    folder_path_part2,
                    sel_num_variables = NULL,
                    sel_cat_variables = cat_variables_list[[k]][1:n_var],
                    n_c = n_c_values[k],
                    my_mar = c(3, 3, 1, 0.5),
                    my_mgp = c(1.75, 0.75, 0))
#dev.off()

k <- 12
#pdf(paste0(manus_path, "marg_distr_pol.pdf"), width = 8, height = 10)
QualComparMargDistr(data_set_name = data_set_names[k],
                    folder_path_part1,
                    folder_path_part2,
                    sel_num_variables = num_variables_list[[k]][1:n_var],
                    sel_cat_variables = NULL,
                    n_c = n_c_values[k],
                    my_mar = my_mar,
                    my_mgp = my_mgp)
#dev.off()









