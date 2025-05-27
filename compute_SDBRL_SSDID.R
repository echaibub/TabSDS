
code_path <- ""
source(paste0(code_path, "utility_functions_for_SynTabSJPPDS.R"))

library(Rfast)
library(caret)

DBRL <- function(dat_o, dat_m) {
  IndexesOfMinimum <- function(x) {
    x <- as.numeric(x)
    min_x <- min(x)
    return(which(x == min_x))
  }
  n <- nrow(dat_m)
  dm <- dista(xnew = as.matrix(dat_m), 
              x = as.matrix(dat_o), 
              type = "euclidean")
  idx_min <- apply(dm, 1, IndexesOfMinimum, simplify = FALSE)
  idx_seq <- seq(n)
  flag <- sapply(idx_seq, function(i) ifelse(i %in% idx_min[[i]], 1, 0))
  disclosure_risk <- sum(flag)/n
  
  return(disclosure_risk)
}


DistanceBasedRecordLinkage <- function(dat_o, 
                                       dat_m, 
                                       num_variables,
                                       sort_data = FALSE) {
  dat_o <- dat_o[, num_variables]
  dat_m <- dat_m[, num_variables]
  dat_o <- scale(dat_o)
  dat_m <- scale(dat_m)
  if (sort_data) {
    ## select most variable numerical column to pivot the sorting
    ulen <- apply(dat_o, 2, function(x) length(unique(x)))
    j <- which.max(ulen)
    
    ## sort the datasets
    idx_o <- order(dat_o[, j])
    idx_m <- order(dat_m[, j])
    dat_o_sorted <- dat_o[idx_o,]
    dat_m_sorted <- dat_m[idx_m,]
    ## compute DBRL on sorted data
    disclosure_risk <- DBRL(dat_o_sorted, dat_m_sorted)
  }
  else {
    disclosure_risk <- DBRL(dat_o, dat_m)
  }
  
  return(disclosure_risk)
}


DatasetsOneHotEncoding <- function(dat1_n, dat2_n) {
  OneHotEncoding <- function(dat_n) {
    dummy <- dummyVars(" ~ .", data = dat_n)
    dat_oh <- data.frame(predict(dummy, newdata = dat_n))
    return(dat_oh)
  }
  n1 <- nrow(dat1_n)
  n2 <- nrow(dat2_n)
  dat_n <- rbind(dat1_n, dat2_n)
  dat_oh <- OneHotEncoding(dat_n)
  dat1_oh <- dat_oh[seq(1, n1),]
  dat2_oh <- dat_oh[seq(n1 + 1, n1 + n2),]
  
  return(list(dat1_oh = dat1_oh,
              dat2_oh = dat2_oh))
}


TabSJPPDS <- function(dat, 
                      num_variables,
                      cat_variables,
                      n_levels,
                      shuffle_type = "simple",
                      verbose = TRUE) {
  
  ## numerical data case
  if (!is.null(num_variables) & is.null(cat_variables)) {
    sdat <- SJPPDS(dat, n_levels, shuffle_type, verbose)
  }
  
  ## categorical data case
  if (is.null(num_variables) & !is.null(cat_variables)) {
    sdat <- CatSJPPDS(dat, n_levels, shuffle_type, verbose)
  }
  
  ## mixed data case
  if (!is.null(num_variables) & !is.null(cat_variables)) {
    sdat <- MixedSJPPDS(dat, 
                        num_variables,
                        cat_variables,
                        n_levels, 
                        shuffle_type, 
                        verbose)
  }
  
  return(sdat)
}


MixedSJPPDS <- function(dat, 
                        num_variables,
                        cat_variables,
                        n_levels,
                        shuffle_type = "simple",
                        verbose = TRUE) {
  
  ## get data matrix of numeric and categorical variables
  X_n <- dat[, num_variables, drop = FALSE]
  X_c <- dat[, cat_variables, drop = FALSE]
  
  ## get numeric and categorical variable names
  nms <- colnames(dat)
  num_variables <- nms[num_variables]
  cat_variables <- nms[cat_variables]
  
  ## convert categorical variables to numerical encoding
  aux <- CategoricalToNumeric(X_c)
  R_c <- aux$dat_N
  lnr <- aux$level_numeric_ranges
  
  ## concatenate the original numeric data with the 
  ## transformed categorical data
  W <- cbind(X_n, R_c)
  
  ## apply SJPPDS to the concatenated data
  W_star <- SJPPDS(W, n_levels, shuffle_type, verbose)
  
  ## transform back the shuffled numeric rank encondings to categorical variables 
  Y_c <- NumericToCategorical(dat_N = W_star[, cat_variables, drop = FALSE],
                              level_numeric_ranges = lnr)
  
  ## concatenate numeric and categorical shuffled data
  X_s <- cbind(W_star[, num_variables], Y_c)
  
  ## reorder the columns of the suffled data to match the order in 
  ## the original data 
  X_s <- X_s[, colnames(dat)]
  
  return(X_s)
}



## computes the SSDID metric
StandardDeviationIntervalDistance <- function(dat_o, 
                                              dat_m, 
                                              num_variables,
                                              k,
                                              record_mapping = TRUE) {
  dat_o <- dat_o[, num_variables]
  dat_m <- dat_m[, num_variables]
  SDID <- function(dat_o, dat_m, k) {
    n <- nrow(dat_o)
    p <- ncol(dat_o)
    ind <- matrix(0, n, p)
    for (j in seq(p)) {
      interval_length <- sd(dat_m[, j]) * k
      for (i in seq(n)) {
        lower_bound <- dat_m[i, j] - interval_length
        upper_bound <- dat_m[i, j] + interval_length
        if (dat_o[i, j] >= lower_bound & dat_o[i, j] <= upper_bound) {
          ind[i, j] <- 1
        }
      }
    }
    record_ind <- apply(ind, 1, sum)
    disclosure_risk <- sum(record_ind == p)/n
    return(disclosure_risk)
  }
  if (record_mapping) {
    disclosure_risk <- SDID(dat_o, dat_m, k)
  }
  else {
    ulen <- apply(dat_o, 2, function(x) length(unique(x)))
    j <- which.max(ulen)
    idx_o <- order(dat_o[, j])
    idx_m <- order(dat_m[, j])
    dat_o_sorted <- dat_o[idx_o,]
    dat_m_sorted <- dat_m[idx_m,]
    disclosure_risk <- SDID(dat_o_sorted, dat_m_sorted, k)
  }
  
  return(list(disclosure_risk = disclosure_risk))
}


## computes and averages the SDID over the k parameter grid
AverageSDID <- function(dat_o, 
                        dat_m, 
                        num_variables,
                        k_grid = seq(0.01, 0.10, by = 0.01),
                        record_mapping = TRUE) {
  n_k <- length(k_grid)
  aux <- rep(NA, n_k)
  for (i in seq(n_k)) {
    aux[i] <- StandardDeviationIntervalDistance(dat_o, 
                                                dat_m,
                                                num_variables,
                                                k_grid[i],
                                                record_mapping)$disclosure_risk
  }
  disclosure_risk <- mean(aux)
  
  return(disclosure_risk)
}


NoiseExperiments <- function(dat_o,
                             num_variables,
                             tuning_par_grid,
                             num_repli = 3,
                             noise_type = "additive") {
  ## number of tuning parameters
  n_pars <- length(tuning_par_grid)
  
  ## objects for storing the metrics
  output_dbrl <- data.frame(matrix(NA, n_pars, num_repli))
  colnames(output_dbrl) <- paste0("repli", seq(num_repli))
  rownames(output_dbrl) <- tuning_par_grid
  output_sdid <- output_dbrl
  output_rid <- output_dbrl
  
  for (i in seq(n_pars)) {
    cat("####################### run ", i, "\n")
    cat("tuning parameter ", tuning_par_grid[i], "\n")
    
    for (j in seq(num_repli)) {
      cat("replication ", j, "\n")
      
      #cat("masking the data", "\n")
      aux_m <- sdcMicro::addNoise(dat_o[, num_variables], 
                                  noise = tuning_par_grid[i], 
                                  method = noise_type)$xm
      ## replace the original numeric variables by the masked ones
      dat_m <- dat_o
      dat_m[, num_variables] <- aux_m
      
      #cat("computing DBRL", "\n")
      output_dbrl[i, j] <- DistanceBasedRecordLinkage(dat_o, 
                                                      dat_m,
                                                      num_variables,
                                                      sort_data = FALSE)
      
      output_sdid[i, j] <- AverageSDID(dat_o, 
                                       dat_m, 
                                       num_variables,
                                       k_grid = seq(0.01, 0.10, by = 0.01),
                                       record_mapping = TRUE)
    }
  }
  
  output <- data.frame(matrix(NA, n_pars, 3))
  colnames(output) <- c("tuning_par", "DBRL", "SDID")
  output[, 1] <- tuning_par_grid
  output[, 2] <- apply(output_dbrl, 1, mean)
  output[, 3] <- apply(output_sdid, 1, mean)
  
  return(list(output = output,
              output_dbrl = output_dbrl,
              output_sdid = output_sdid))
}



TabSDSExperiments <- function(dat_o,
                              num_variables,
                              cat_variables,
                              tuning_par_grid,
                              num_repli = 3) {
  ## number of tuning parameters
  n_pars <- length(tuning_par_grid)
  
  ## objects for storing the metrics
  output_dbrl <- data.frame(matrix(NA, n_pars, num_repli))
  colnames(output_dbrl) <- paste0("repli", seq(num_repli))
  rownames(output_dbrl) <- tuning_par_grid
  output_sdid <- output_dbrl
  output_rid <- output_dbrl
  
  for (i in seq(n_pars)) {
    cat("####################### run ", i, "\n")
    cat("tuning parameter ", tuning_par_grid[i], "\n")
    
    for (j in seq(num_repli)) {
      cat("replication ", j, "\n")
      
      #cat("masking the data", "\n")
      dat_m <- SynTabSJPPDS(dat = dat_o, 
                            num_variables,
                            cat_variables,
                            n_levels = tuning_par_grid[i],
                            shuffle_type = "simple",
                            n_prop = 0.5,
                            verbose = FALSE)
      
      #cat("computing DBRL", "\n")
      output_dbrl[i, j] <- DistanceBasedRecordLinkage(dat_o, 
                                                      dat_m,
                                                      num_variables,
                                                      sort_data = TRUE)
      
      output_sdid[i, j] <- AverageSDID(dat_o, 
                                       dat_m, 
                                       num_variables,
                                       k_grid = seq(0.01, 0.10, by = 0.01),
                                       record_mapping = FALSE)
    }
  }
  
  output <- data.frame(matrix(NA, n_pars, 3))
  colnames(output) <- c("tuning_par", "DBRL", "SDID")
  output[, 1] <- tuning_par_grid
  output[, 2] <- apply(output_dbrl, 1, mean)
  output[, 3] <- apply(output_sdid, 1, mean)
  
  return(list(output = output,
              output_dbrl = output_dbrl,
              output_sdid = output_sdid))
}


TabSJPPDSExperiments <- function(dat_o,
                                 num_variables,
                                 cat_variables,
                                 tuning_par_grid,
                                 num_repli = 3) {
  ## number of tuning parameters
  n_pars <- length(tuning_par_grid)
  
  ## objects for storing the metrics
  output_dbrl <- data.frame(matrix(NA, n_pars, num_repli))
  colnames(output_dbrl) <- paste0("repli", seq(num_repli))
  rownames(output_dbrl) <- tuning_par_grid
  output_sdid <- output_dbrl
  output_rid <- output_dbrl
  
  for (i in seq(n_pars)) {
    cat("####################### run ", i, "\n")
    cat("tuning parameter ", tuning_par_grid[i], "\n")
    
    for (j in seq(num_repli)) {
      cat("replication ", j, "\n")
      
      #cat("masking the data", "\n")
      dat_m <- TabSJPPDS(dat = dat_o, 
                         num_variables,
                         cat_variables,
                         n_levels = tuning_par_grid[i],
                         shuffle_type = "simple",
                         verbose = FALSE)
      
      #cat("computing DBRL", "\n")
      output_dbrl[i, j] <- DistanceBasedRecordLinkage(dat_o, 
                                                      dat_m,
                                                      num_variables,
                                                      sort_data = TRUE)
      
      output_sdid[i, j] <- AverageSDID(dat_o, 
                                       dat_m, 
                                       num_variables,
                                       k_grid = seq(0.01, 0.10, by = 0.01),
                                       record_mapping = FALSE)
    }
  }
  
  output <- data.frame(matrix(NA, n_pars, 3))
  colnames(output) <- c("tuning_par", "DBRL", "SDID")
  output[, 1] <- tuning_par_grid
  output[, 2] <- apply(output_dbrl, 1, mean)
  output[, 3] <- apply(output_sdid, 1, mean)
  
  return(list(output = output,
              output_dbrl = output_dbrl,
              output_sdid = output_sdid))
}


AdditionalExperiments <- function(ori_data_path,
                                  syn_data_path,
                                  num_variables,
                                  file_names,
                                  methods_names) {
  ## number of generators
  n_methods <- length(methods_names)
  
  ## object for storing the metrics
  output <- data.frame(matrix(NA, n_methods, 3))
  colnames(output) <- c("generator", "DBRL", "SDID")
  output[, 1] <- methods_names
  
  for (i in seq(n_methods)) {
    cat("generator: ", methods_names[i], "\n")
    
    dat_o <- read.csv(paste0(ori_data_path, "train_set.csv"))
    dat_m <- read.csv(paste0(syn_data_path, file_names[i]))
    
    output[i, 2] <- DistanceBasedRecordLinkage(dat_o, 
                                               dat_m, 
                                               num_variables,
                                               sort_data = TRUE)
    output[i, 3] <- AverageSDID(dat_o, 
                                dat_m, 
                                num_variables,
                                k_grid = seq(0.01, 0.10, by = 0.01),
                                record_mapping = FALSE)
  }
  
  return(list(output = output))
}


#####################################################
#####################################################
#####################################################

tabsds_grid <- c(seq(5, 50, by = 5), seq(60, 100, by = 10), seq(200, 1000, by = 100))
tabsds_grid
noise_grid <- c(1, seq(5, 115, by = 5))
noise_grid


num_repli <- 1

file_names <- c("syn_ddpm.csv", 
                "syn_arf.csv", 
                "syn_tvae.csv", 
                "syn_ctgan.csv",
                "syn_bayesnet.csv",
                "test_set.csv",
                "syn_adsgan.csv",
                "syn_pategan.csv",
                "syn_smote_5.csv",
                "syn_smote_20.csv")

methods_names <- c("TabSDS", "DDPM", "ARF", "TVAE", "CTGAN", "BayesNet", 
                   "test_set", "ADSGAN", "PATEGAN", "SMOTE_5", "SMOTE_20")

######################
## abalone
######################

data_path <- "~/outputs/abalone/simulated_datasets/"
dat <- read.csv(paste0(data_path, "train_set.csv"))

set.seed(12345)
abalone_tabsds <- TabSDSExperiments(dat_o = dat,
                                    num_variables = seq(2, 9),
                                    cat_variables = 1,
                                    tuning_par_grid = tabsds_grid,
                                    num_repli = num_repli)

set.seed(12345)
abalone_tabsjppds <- TabSJPPDSExperiments(dat_o = dat,
                                          num_variables = seq(2, 9),
                                          cat_variables = 1,
                                          tuning_par_grid = tabsds_grid,
                                          num_repli = num_repli)

set.seed(12345)
abalone_noise <- NoiseExperiments(dat_o = dat,
                                  num_variables = seq(2, 9),
                                  tuning_par_grid = noise_grid,
                                  num_repli = num_repli,
                                  noise_type = "additive")

tabsds_name <- "syn_tab_sjppds_20_0.5.csv"
set.seed(12345)
abalone_additional <- AdditionalExperiments(ori_data_path = data_path,
                                            syn_data_path = data_path,
                                            num_variables = seq(2, 9),
                                            file_names = c(tabsds_name, file_names),
                                            methods_names = methods_names)


######################
## bank marketing
######################

data_path <- "~/outputs/bank_marketing/simulated_datasets/"
dat <- read.csv(paste0(data_path, "train_set.csv"))

set.seed(12345)
bank_marketing_tabsds <- TabSDSExperiments(dat_o = dat,
                                           num_variables = seq(7),
                                           cat_variables = 8,
                                           tuning_par_grid = tabsds_grid,
                                           num_repli = num_repli)

set.seed(12345)
bank_marketing_tabsjppds <- TabSJPPDSExperiments(dat_o = dat,
                                                 num_variables = seq(7),
                                                 cat_variables = 8,
                                                 tuning_par_grid = tabsds_grid,
                                                 num_repli = num_repli)

set.seed(12345)
bank_marketing_noise <- NoiseExperiments(dat_o = dat,
                                         num_variables = seq(7),
                                         tuning_par_grid = noise_grid,
                                         num_repli = num_repli,
                                         noise_type = "additive")

tabsds_name <- "syn_tab_sjppds_100_0.5.csv"
set.seed(12345)
bank_marketing_additional <- AdditionalExperiments(ori_data_path = data_path,
                                                   syn_data_path = data_path,
                                                   num_variables = seq(7),
                                                   file_names = c(tabsds_name, file_names),
                                                   methods_names = methods_names)



#######################
## california housing
#######################

data_path <- "~/outputs/california_housing_original/simulated_datasets/"
dat <- read.csv(paste0(data_path, "train_set.csv"))

set.seed(12345)
california_tabsds <- TabSDSExperiments(dat_o = dat,
                                       num_variables = seq(9),
                                       cat_variables = NULL,
                                       tuning_par_grid = tabsds_grid,
                                       num_repli = num_repli)

set.seed(12345)
california_tabsjppds <- TabSJPPDSExperiments(dat_o = dat,
                                             num_variables = seq(9),
                                             cat_variables = NULL,
                                             tuning_par_grid = tabsds_grid,
                                             num_repli = num_repli)

set.seed(12345)
california_noise <- NoiseExperiments(dat_o = dat,
                                     num_variables = seq(9),
                                     tuning_par_grid = noise_grid,
                                     num_repli = num_repli,
                                     noise_type = "additive")

tabsds_name <- "syn_tab_sjppds_200_0.5.csv"
set.seed(12345)
california_additional <- AdditionalExperiments(ori_data_path = data_path,
                                               syn_data_path = data_path,
                                               num_variables = seq(9),
                                               file_names = c(tabsds_name, file_names),
                                               methods_names = methods_names)



#######################
## credit
#######################

data_path <- "~/outputs/credit/simulated_datasets/"
dat <- read.csv(paste0(data_path, "train_set.csv"))

set.seed(12345)
credit_tabsds <- TabSDSExperiments(dat_o = dat,
                                   num_variables = seq(10),
                                   cat_variables = 11,
                                   tuning_par_grid = tabsds_grid,
                                   num_repli = num_repli)

set.seed(12345)
credit_tabsjppds <- TabSJPPDSExperiments(dat_o = dat,
                                         num_variables = seq(10),
                                         cat_variables = 11,
                                         tuning_par_grid = tabsds_grid,
                                         num_repli = num_repli)

set.seed(12345)
credit_noise <- NoiseExperiments(dat_o = dat,
                                 num_variables = seq(10),
                                 tuning_par_grid = noise_grid,
                                 num_repli = num_repli,
                                 noise_type = "additive")

tabsds_name <- "syn_tab_sjppds_1000_0.5.csv"
set.seed(12345)
credit_additional <- AdditionalExperiments(ori_data_path = data_path,
                                           syn_data_path = data_path,
                                           num_variables = seq(10),
                                           file_names = c(tabsds_name, file_names),
                                           methods_names = methods_names)


#######################
## diabetes
#######################

data_path <- "~/outputs/diabetes_130US/simulated_datasets/"
dat <- read.csv(paste0(data_path, "train_set.csv"))

set.seed(12345)
diabetes_tabsds <- TabSDSExperiments(dat_o = dat,
                                     num_variables = seq(7),
                                     cat_variables = 8,
                                     tuning_par_grid = tabsds_grid,
                                     num_repli = num_repli)

set.seed(12345)
diabetes_tabsjppds <- TabSJPPDSExperiments(dat_o = dat,
                                           num_variables = seq(7),
                                           cat_variables = 8,
                                           tuning_par_grid = tabsds_grid,
                                           num_repli = num_repli)


set.seed(12345)
diabetes_noise <- NoiseExperiments(dat_o = dat,
                                   num_variables = seq(7),
                                   tuning_par_grid = noise_grid,
                                   num_repli = num_repli,
                                   noise_type = "additive")

tabsds_name <- "syn_tab_sjppds_35_0.5.csv"
set.seed(12345)
diabetes_additional <- AdditionalExperiments(ori_data_path = data_path,
                                             syn_data_path = data_path,
                                             num_variables = seq(7),
                                             file_names = c(tabsds_name, file_names),
                                             methods_names = methods_names)


#######################
## electricity
#######################

data_path <- "~/outputs/electricity/simulated_datasets/"
dat <- read.csv(paste0(data_path, "train_set.csv"))

set.seed(12345)
electricity_tabsds <- TabSDSExperiments(dat_o = dat,
                                        num_variables = seq(7),
                                        cat_variables = 8,
                                        tuning_par_grid = tabsds_grid,
                                        num_repli = num_repli)

set.seed(12345)
electricity_tabsjppds <- TabSJPPDSExperiments(dat_o = dat,
                                              num_variables = seq(7),
                                              cat_variables = 8,
                                              tuning_par_grid = tabsds_grid,
                                              num_repli = num_repli)

set.seed(12345)
electricity_noise <- NoiseExperiments(dat_o = dat,
                                      num_variables = seq(7),
                                      tuning_par_grid = noise_grid,
                                      num_repli = num_repli,
                                      noise_type = "additive")

tabsds_name <- "syn_tab_sjppds_20_0.5.csv"
set.seed(12345)
electricity_additional <- AdditionalExperiments(ori_data_path = data_path,
                                                syn_data_path = data_path,
                                                num_variables = seq(7),
                                                file_names = c(tabsds_name, file_names),
                                                methods_names = methods_names)


#######################
## eye_movement
#######################

data_path <- "~/outputs/eye_movement/simulated_datasets/"
dat <- read.csv(paste0(data_path, "train_set.csv"))

set.seed(12345)
eye_movement_tabsds <- TabSDSExperiments(dat_o = dat,
                                         num_variables = seq(20),
                                         cat_variables = 21,
                                         tuning_par_grid = tabsds_grid,
                                         num_repli = num_repli)

set.seed(12345)
eye_movement_tabsjppds <- TabSJPPDSExperiments(dat_o = dat,
                                               num_variables = seq(20),
                                               cat_variables = 21,
                                               tuning_par_grid = tabsds_grid,
                                               num_repli = num_repli)

set.seed(12345)
eye_movement_noise <- NoiseExperiments(dat_o = dat,
                                       num_variables = seq(20),
                                       tuning_par_grid = noise_grid,
                                       num_repli = num_repli,
                                       noise_type = "additive")

tabsds_name <- "syn_tab_sjppds_20_0.5.csv"
set.seed(12345)
eye_movement_additional <- AdditionalExperiments(ori_data_path = data_path,
                                                 syn_data_path = data_path,
                                                 num_variables = seq(20),
                                                 file_names = c(tabsds_name, file_names),
                                                 methods_names = methods_names)



#######################
## house_16h
#######################

data_path <- "~/outputs/house_16h/simulated_datasets/"
dat <- read.csv(paste0(data_path, "train_set.csv"))

set.seed(12345)
house_16h_tabsds <- TabSDSExperiments(dat_o = dat,
                                      num_variables = seq(16),
                                      cat_variables = 17,
                                      tuning_par_grid = tabsds_grid,
                                      num_repli = num_repli)

set.seed(12345)
house_16h_tabsjppds <- TabSJPPDSExperiments(dat_o = dat,
                                            num_variables = seq(16),
                                            cat_variables = 17,
                                            tuning_par_grid = tabsds_grid,
                                            num_repli = num_repli)

set.seed(12345)
house_16h_noise <- NoiseExperiments(dat_o = dat,
                                    num_variables = seq(16),
                                    tuning_par_grid = noise_grid,
                                    num_repli = num_repli,
                                    noise_type = "additive")


tabsds_name <- "syn_tab_sjppds_1000_0.5.csv"
set.seed(12345)
house_16h_additional <- AdditionalExperiments(ori_data_path = data_path,
                                              syn_data_path = data_path,
                                              num_variables = seq(16),
                                              file_names = c(tabsds_name, file_names),
                                              methods_names = methods_names)



#######################
## magic_telescope
#######################

data_path <- "~/outputs/magic_telescope/simulated_datasets/"
dat <- read.csv(paste0(data_path, "train_set.csv"))

set.seed(12345)
magic_telescope_tabsds <- TabSDSExperiments(dat_o = dat,
                                            num_variables = seq(10),
                                            cat_variables = 11,
                                            tuning_par_grid = tabsds_grid,
                                            num_repli = num_repli)

set.seed(12345)
magic_telescope_tabsjppds <- TabSJPPDSExperiments(dat_o = dat,
                                                  num_variables = seq(10),
                                                  cat_variables = 11,
                                                  tuning_par_grid = tabsds_grid,
                                                  num_repli = num_repli)

set.seed(12345)
magic_telescope_noise <- NoiseExperiments(dat_o = dat,
                                          num_variables = seq(10),
                                          tuning_par_grid = noise_grid,
                                          num_repli = num_repli,
                                          noise_type = "additive")


tabsds_name <- "syn_tab_sjppds_25_0.5.csv"
set.seed(12345)
magic_telescope_additional <- AdditionalExperiments(ori_data_path = data_path,
                                                    syn_data_path = data_path,
                                                    num_variables = seq(10),
                                                    file_names = c(tabsds_name, file_names),
                                                    methods_names = methods_names)


#######################
## pol
#######################

data_path <- "~/outputs/pol/simulated_datasets/"
dat <- read.csv(paste0(data_path, "train_set.csv"))

set.seed(12345)
pol_tabsds <- TabSDSExperiments(dat_o = dat,
                                num_variables = seq(26),
                                cat_variables = 27,
                                tuning_par_grid = tabsds_grid,
                                num_repli = num_repli)

set.seed(12345)
pol_tabsjppds <- TabSJPPDSExperiments(dat_o = dat,
                                      num_variables = seq(26),
                                      cat_variables = 27,
                                      tuning_par_grid = tabsds_grid,
                                      num_repli = num_repli)

set.seed(12345)
pol_noise <- NoiseExperiments(dat_o = dat,
                              num_variables = seq(26),
                              tuning_par_grid = noise_grid,
                              num_repli = num_repli,
                              noise_type = "additive")

tabsds_name <- "syn_tab_sjppds_15_0.5.csv"
set.seed(12345)
pol_additional <- AdditionalExperiments(ori_data_path = data_path,
                                        syn_data_path = data_path,
                                        num_variables = seq(26),
                                        file_names = c(tabsds_name, file_names),
                                        methods_names = methods_names)


## save all outputs
save(abalone_tabsds, abalone_tabsjppds, abalone_noise, abalone_additional,
     bank_marketing_tabsds, bank_marketing_tabsjppds, bank_marketing_noise, bank_marketing_additional,
     california_tabsds, california_tabsjppds, california_noise, california_additional,
     credit_tabsds, credit_tabsjppds, credit_noise, credit_additional,
     diabetes_tabsds, diabetes_tabsjppds, diabetes_noise, diabetes_additional,
     electricity_tabsds, electricity_tabsjppds, electricity_noise, electricity_additional,
     eye_movement_tabsds, eye_movement_tabsjppds, eye_movement_noise, eye_movement_additional,
     house_16h_tabsds, house_16h_tabsjppds, house_16h_noise, house_16h_additional,
     magic_telescope_tabsds, magic_telescope_tabsjppds, magic_telescope_noise, magic_telescope_additional,
     pol_tabsds, pol_tabsjppds, pol_noise, pol_additional,
     file = "dbrl_sdid_outputs.RData", compress = TRUE)






