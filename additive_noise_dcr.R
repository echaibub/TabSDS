
code_path <- ""
source(paste0(code_path, "utility_functions_for_SynTabSJPPDS.R"))

library(Rfast)
library(caret)

AdditiveNoisePerturbation <- function(dat_o,
                                      noise_percent,
                                      num_variables) {
  ## Adds independent gaussian noise to each numeric variable of a dataset
  ##
  ## Parameters:
  ## dat_o: data.frame containing numerical variables (and potentially categorical ones)
  ## noise_percent: proportion of noise (positive float)
  ## num_variables: indexes of the columns containing numerical variables
  ##
  ## Returns:
  ## dat_m: data.frame containing the perturbed data
  n <- nrow(dat_o)
  dat_m <- dat_o
  for (j in num_variables) {
    var_sd <- sd(dat_o[, j])
    dat_m[, j] <- dat_o[, j] + rnorm(n, 0, noise_percent * var_sd)
  }
  
  return(dat_m)
}


AdditiveNoiseDCR <- function(dat_train,
                             dat_test,
                             num_variables,
                             cat_variables,
                             tuning_par_grid) {
  
  ## number of tuning parameters
  num_param <- length(tuning_par_grid)
  
  ## create matrix to store results
  DCR <- matrix(NA, nrow(dat_train), num_param + 1)
  colnames(DCR) <- c("test", tuning_par_grid)
  
  ## compute DCR between train and test sets
  cat("test set", "\n")
  ## perform one-hot-encoding of categorical data if necessary
  if (!is.null(cat_variables)) {
    dat_train_oh <- OneHotEncoding(dat_train)
    dat_test_oh <- OneHotEncoding(dat_test)
    DCR[, 1] <- ComputeDCR(dat_train_oh, 
                           dat_test_oh, 
                           distance_type = "euclidean")
  }
  else {
    DCR[, 1] <- ComputeDCR(dat_train, 
                           dat_test, 
                           distance_type = "euclidean")
  }
  
  for (i in seq(num_param)) {
    cat(tuning_par_grid[i], "\n")
    
    ## generate synthetic data for tuning parameter at position i
    dat_synth <- AdditiveNoisePerturbation(dat_o = dat_train,
                                           noise_percent = tuning_par_grid[i],
                                           num_variables = num_variables)
    
    ## compute DCR between training and synthetic data
    if (!is.null(cat_variables)) {
      dat_synth_oh <- OneHotEncoding(dat_synth)
      DCR[, i + 1] <- ComputeDCR(dat_train_oh, 
                                 dat_synth_oh, 
                                 distance_type = "euclidean")
    }
    else {
      DCR[, i + 1] <- ComputeDCR(dat_train, 
                                 dat_synth, 
                                 distance_type = "euclidean")
    }
  }
  
  return(DCR)
}


###########################################################
###########################################################
###########################################################

noise_grid <- c(1, seq(5, 115, by = 5))
noise_grid

out_path <- "" ## folder path for saving the output file

#####################################
## abalone
#####################################

data_path <- "~/outputs/abalone/simulated_datasets/"
dat_train <- read.csv(paste0(data_path, "train_set.csv"))
dat_test <- read.csv(paste0(data_path, "test_set.csv"))

dcrs_AB <- AdditiveNoiseDCR(dat_train = dat_train,
                            dat_test = dat_test,
                            num_variables = seq(2, 9),
                            cat_variables = 1,
                            tuning_par_grid = noise_grid)

fname <- paste0(out_path, "additive_noise_dcr_AB.csv")
write.csv(dcrs_AB, file = fname, row.names = FALSE)


#####################################
## bank marketing
#####################################

data_path <- "~/outputs/bank_marketing/simulated_datasets/"
dat_train <- read.csv(paste0(data_path, "train_set.csv"))
dat_test <- read.csv(paste0(data_path, "test_set.csv"))

dcrs_BM <- AdditiveNoiseDCR(dat_train = dat_train,
                            dat_test = dat_test,
                            num_variables = seq(7),
                            cat_variables = 8,
                            tuning_par_grid = noise_grid)

fname <- paste0(out_path, "additive_noise_dcr_BM.csv")
write.csv(dcrs_BM, file = fname, row.names = FALSE)


#####################################
## california housing
#####################################

data_path <- "~/outputs/california_housing_original/simulated_datasets/"
dat_train <- read.csv(paste0(data_path, "train_set.csv"))
dat_test <- read.csv(paste0(data_path, "test_set.csv"))

dcrs_CH <- AdditiveNoiseDCR(dat_train = dat_train,
                            dat_test = dat_test,
                            num_variables = seq(9),
                            cat_variables = NULL,
                            tuning_par_grid = noise_grid)

fname <- paste0(out_path, "additive_noise_dcr_CH.csv")
write.csv(dcrs_CH, file = fname, row.names = FALSE)


#####################################
## credit
#####################################

data_path <- "~/outputs/credit/simulated_datasets/"
dat_train <- read.csv(paste0(data_path, "train_set.csv"))
dat_test <- read.csv(paste0(data_path, "test_set.csv"))

dcrs_CR <- AdditiveNoiseDCR(dat_train = dat_train,
                            dat_test = dat_test,
                            num_variables = seq(10),
                            cat_variables = 11,
                            tuning_par_grid = noise_grid)

fname <- paste0(out_path, "additive_noise_dcr_CR.csv")
write.csv(dcrs_CR, file = fname, row.names = FALSE)


#####################################
## diabetes 130US
#####################################

data_path <- "~/outputs/diabetes_130US/simulated_datasets/"
dat_train <- read.csv(paste0(data_path, "train_set.csv"))
dat_test <- read.csv(paste0(data_path, "test_set.csv"))

dcrs_DI <- AdditiveNoiseDCR(dat_train = dat_train,
                            dat_test = dat_test,
                            num_variables = seq(7),
                            cat_variables = 8,
                            tuning_par_grid = noise_grid)

fname <- paste0(out_path, "additive_noise_dcr_DI.csv")
write.csv(dcrs_DI, file = fname, row.names = FALSE)


#####################################
## electricity
#####################################

data_path <- "~/outputs/electricity/simulated_datasets/"
dat_train <- read.csv(paste0(data_path, "train_set.csv"))
dat_test <- read.csv(paste0(data_path, "test_set.csv"))

dcrs_EL <- AdditiveNoiseDCR(dat_train = dat_train,
                            dat_test = dat_test,
                            num_variables = seq(7),
                            cat_variables = 8,
                            tuning_par_grid = noise_grid)

fname <- paste0(out_path, "additive_noise_dcr_EL.csv")
write.csv(dcrs_EL, file = fname, row.names = FALSE)


#####################################
## eye movement
#####################################

data_path <- "~/outputs/eye_movement/simulated_datasets/"
dat_train <- read.csv(paste0(data_path, "train_set.csv"))
dat_test <- read.csv(paste0(data_path, "test_set.csv"))

dcrs_EM <- AdditiveNoiseDCR(dat_train = dat_train,
                            dat_test = dat_test,
                            num_variables = seq(20),
                            cat_variables = 21,
                            tuning_par_grid = noise_grid)

fname <- paste0(out_path, "additive_noise_dcr_EM.csv")
write.csv(dcrs_EM, file = fname, row.names = FALSE)


#####################################
## house 16h
#####################################

data_path <- "~/outputs/house_16h/simulated_datasets/"
dat_train <- read.csv(paste0(data_path, "train_set.csv"))
dat_test <- read.csv(paste0(data_path, "test_set.csv"))

dcrs_HO <- AdditiveNoiseDCR(dat_train = dat_train,
                            dat_test = dat_test,
                            num_variables = seq(16),
                            cat_variables = 17,
                            tuning_par_grid = noise_grid)

fname <- paste0(out_path, "additive_noise_dcr_HO.csv")
write.csv(dcrs_HO, file = fname, row.names = FALSE)


#####################################
## magic telescope
#####################################

data_path <- "~/outputs/magic_telescope/simulated_datasets/"
dat_train <- read.csv(paste0(data_path, "train_set.csv"))
dat_test <- read.csv(paste0(data_path, "test_set.csv"))

dcrs_MT <- AdditiveNoiseDCR(dat_train = dat_train,
                            dat_test = dat_test,
                            num_variables = seq(10),
                            cat_variables = 11,
                            tuning_par_grid = noise_grid)

fname <- paste0(out_path, "additive_noise_dcr_MT.csv")
write.csv(dcrs_MT, file = fname, row.names = FALSE)


#####################################
## pol
#####################################

data_path <- "~/outputs/pol/simulated_datasets/"
dat_train <- read.csv(paste0(data_path, "train_set.csv"))
dat_test <- read.csv(paste0(data_path, "test_set.csv"))

dcrs_PO <- AdditiveNoiseDCR(dat_train = dat_train,
                            dat_test = dat_test,
                            num_variables = seq(26),
                            cat_variables = 27,
                            tuning_par_grid = noise_grid)

fname <- paste0(out_path, "additive_noise_dcr_PO.csv")
write.csv(dcrs_PO, file = fname, row.names = FALSE)





