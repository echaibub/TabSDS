
#############################################################################
## utility functions implementing the SJPPDS approaches
#############################################################################

## discretize a single numerical variable
CategorizeVariable <- function(x, n_levels) {
  ## Inputs:
  ## x: vector of numeric values
  ## n_levels: number of categories/levels (i.e., n_c in the paper's notation)
  ##
  ## Output:
  ## out: vector of categorical variables
  
  var_levels <- seq(n_levels)
  out <- cut(x, breaks = n_levels, labels = var_levels)
  out <- as.character(as.numeric(out))
  
  return(out)
}


## discretize the entire numeric dataset
CategorizeData <- function(dat, n_levels) {
  ## Inputs:
  ## dat: numerical dataset
  ## n_levels: number of categories/levels (i.e., n_c in the paper's notation)
  ##
  ## Output:
  ## out: dataset of categorical variables
  
  n_vars <- ncol(dat)
  dat_C <- dat
  for (i in seq(n_vars)) {
    dat_C[, i] <- CategorizeVariable(dat[, i], n_levels)
  } 
  
  return(dat_C)
}


## implement the full JPPDS approach
JointProbabilityPreservingDataShufflingF <- function(dat, dat_C) {
  ## Inputs:
  ## dat: numerical dataset
  ## dat_C: categorical dataset
  ##
  ## Output:
  ## dat_S: masked dataset
  
  p <- ncol(dat)
  dat_S <- dat
  for (i in seq(p - 1)) {
    col_idx <- seq(i + 1, p)
    lcombs <- apply(dat_C[, col_idx, drop = FALSE], 1, function (x) paste(x, collapse = "_"))
    ucombs <- unique(lcombs)
    for (j in seq(length(ucombs))) {
      idx <- which(lcombs == ucombs[j])
      shuffled_idx <- idx[sample(length(idx))]
      dat_S[idx, seq(1, i)] <- dat[shuffled_idx, seq(1, i)]
    }
  }
  ## because the last columns of dat and dat_S are
  ## identical, we perform a final random shuffling 
  ## of the entire data set
  dat_S <- dat_S[sample(nrow(dat_S)),]
  
  return(dat_S)
}


## implement the simplified JPPDS approach 
JointProbabilityPreservingDataShufflingS <- function(dat, dat_C) {
  ## Inputs:
  ## dat: numerical dataset
  ## dat_C: categorical dataset
  ##
  ## Output:
  ## dat_S: masked dataset
  
  p <- ncol(dat)
  dat_S <- dat
  last_col_classes <- unique(dat_C[, p])
  col_idx <- seq(1, p - 1)
  for (j in seq(length(last_col_classes))) {
    idx <- which(dat_C[, p] == last_col_classes[j])
    shuffled_idx <- idx[sample(length(idx))]
    dat_S[idx, col_idx] <- dat[shuffled_idx, col_idx]
  }
  
  ## because the last columns of dat and dat_S are
  ## identical, we perform a final random suffling 
  ## of the entire data set
  dat_S <- dat_S[sample(nrow(dat_S)),]
  
  return(dat_S)
}


## implement the SJPPDS approach
SJPPDS <- function(dat, 
                   n_levels,
                   shuffle_type = "simple",
                   verbose = FALSE) {
  ## Inputs:
  ## dat: numerical dataset
  ## n_levels: number of categories/levels (i.e., n_c in the paper's notation)
  ## shuffle_type: "simple" implements the simplified SJPPDS, while "full"
  ##               implements the full SJPPDS approach
  ## verbose: whether to print the computation progress
  ##
  ## Output:
  ## dat_S: masked dataset
  
  ## switch between the simplified and full approaches
  GenerateShuffledDataset <- function(dat, n_levels, shuffle_type = "simple") {
    dat <- data.frame(dat)
    dat_C <- CategorizeData(dat, n_levels)
    if (shuffle_type == "simple") {
      dat_S <- JointProbabilityPreservingDataShufflingS(dat, dat_C)
    }
    else if (shuffle_type == "full") {
      dat_S <- JointProbabilityPreservingDataShufflingF(dat, dat_C)
    }
    
    return(dat_S)
  }
  
  p <- ncol(dat) 
  if (verbose) {
    cat("shuffle", 1, "\n") 
  }
  dat_S <- GenerateShuffledDataset(dat, n_levels, shuffle_type)
  for (i in seq(1, p - 1)) {
    if (verbose) {
      cat("shuffle", i + 1, "\n") 
    }
    dat_S <- dat_S[, c(2:p, 1)]
    dat_S <- GenerateShuffledDataset(dat_S, n_levels, shuffle_type)
  }
  dat_S <- dat_S[, c(2:p, 1)] ## revert the data to original variable order
  
  return(dat_S)
}


#################################################
## categorical variable functions
#################################################

CategoricalToNumeric <- function(dat_C) {
  n <- nrow(dat_C)
  n_var <- ncol(dat_C)
  dat_N <- data.frame(matrix(NA, n, n_var))
  names(dat_N) <- names(dat_C)
  level_numeric_ranges <- vector(mode = "list", length = n_var)
  names(level_numeric_ranges) <- colnames(dat_C)
  for (i in seq(n_var)) {
    tb <- table(dat_C[, i])
    variable_levels <- names(tb)
    cumulative_counts <- cumsum(c(0, as.numeric(tb)))
    n_levels <- length(variable_levels)
    tmp <- data.frame(matrix(NA, n_levels, 3))
    names(tmp) <- c("level", "lower_bound", "upper_bound")
    for (j in seq(n_levels)) {
      idx <- which(dat_C[, i] == variable_levels[j])
      lower_bound <- cumulative_counts[j] + 1
      upper_bound <- cumulative_counts[j + 1]
      ## shuffle the values ("random tie breaking of ranks")
      dat_N[idx, i] <- seq(lower_bound, upper_bound)[sample(length(idx))]
      tmp[j, 1] <- variable_levels[j]
      tmp[j, 2] <- lower_bound
      tmp[j, 3] <- upper_bound
    }
    level_numeric_ranges[[i]] <- tmp
  }
  
  list(dat_N = dat_N,
       level_numeric_ranges = level_numeric_ranges)
}


NumericToCategorical <- function(dat_N,
                                 level_numeric_ranges) {
  n <- nrow(dat_N)
  n_var <- ncol(dat_N)
  dat_C <- data.frame(matrix(NA, n, n_var))
  names(dat_C) <- names(dat_N)
  for (i in seq(n_var)) {
    su <- level_numeric_ranges[[i]]
    variable_levels <- as.character(su$level)
    n_levels <- length(variable_levels)
    for (j in seq(n_levels)) {
      idx <- which(dat_N[, i] >= su[j, "lower_bound"] & dat_N[, i] <= su[j, "upper_bound"])
      dat_C[idx, i] <- variable_levels[j]
    }
    dat_C[, i] <- as.factor(dat_C[, i])
  }
  
  dat_C
}


CatSJPPDS <- function(dat, 
                      n_levels,
                      shuffle_type = "simple",
                      verbose = TRUE) {
  
  ## convert categorical variables to numerical encoding
  aux <- CategoricalToNumeric(dat)
  dat_C2N <- aux$dat_N
  lnr <- aux$level_numeric_ranges
  
  ## apply SJPPDS to the concatenated data
  sdat_C2N <- SJPPDS(dat_C2N, n_levels, shuffle_type, verbose)
  
  ## transform back the shuffled numeric encondings to categorical variables 
  sdat <- NumericToCategorical(dat_N = sdat_C2N,
                               level_numeric_ranges = lnr)
  
  return(sdat)
}


#####################################################
## interpolated order statistics functions
#####################################################


IOSSampling <- function(x, n_prop = 0.5) {
  IOSSampler <- function(x_1, x_2) {
    x_1_s <- sort(x_1)
    x_2_s <- sort(x_2)
    n <- length(x_1)
    y <- rep(NA, n)
    lower_bound <- pmin(x_1_s, x_2_s)
    upper_bound <- pmax(x_1_s, x_2_s)
    y <- runif(n, lower_bound, upper_bound)
    return(y)
  }
  n <- length(x)
  seq_n <- seq(n)
  nn <- floor(n * n_prop)
  n_draws <- ceiling(n/nn)
  
  y <- matrix(NA, n_draws, nn)
  for (i in seq(n_draws)) {
    idx_1 <- sample(n, nn, replace = FALSE)
    idx_remain <- setdiff(seq_n, idx_1)
    idx_2 <- sample(idx_remain, nn, replace = FALSE)
    y[i,] <- IOSSampler(x_1 = x[idx_1], x_2 = x[idx_2])
  }
  y <- as.vector(y)
  y <- sample(y, n, replace = FALSE)
  
  return(y)
}


GenerateSyntheticMarginalsIOS <- function(dat_o, n_prop = 0.5) {
  n <- nrow(dat_o)
  p <- ncol(dat_o)
  dat_s <- matrix(NA, n, p)
  colnames(dat_s) <- colnames(dat_o)
  for (i in seq(p)) {
    dat_s[, i] <- IOSSampling(x = dat_o[, i], n_prop)
  }
  
  return(dat_s)
}


#####################################################
## additional rank related functions
#####################################################

GetDataRankMatrix <- function(X) {
  p <- ncol(X)
  R <- X
  for (j in seq(p)) {
    R[, j] <- rank(X[, j], ties.method = "random")
  }
  
  return(R)
} 


MatchRanksOld <- function(synthetic_marginals_matrix, rank_matrix) {
  dat_s <- synthetic_marginals_matrix
  p <- ncol(rank_matrix)
  n <- nrow(rank_matrix)
  for (j in seq(p)) {
    r_j <- rank(synthetic_marginals_matrix[, j], ties.method = "random")
    for (i in seq(n)) {
      ii <- which(r_j == rank_matrix[i, j])
      dat_s[i, j] <- synthetic_marginals_matrix[ii, j]
    }
  }
  
  return(dat_s)
}


MatchRanks <- function(synthetic_marginals_matrix, rank_matrix) {
  dat_s <- synthetic_marginals_matrix
  p <- ncol(rank_matrix)
  n <- nrow(rank_matrix)
  for (j in seq(p)) {
    sorted_syn_dat <- sort(synthetic_marginals_matrix[, j])
    dat_s[, j] <- sorted_syn_dat[rank_matrix[, j]]
  }
  
  return(dat_s)
}


SyntheticSJPPDS <- function(dat, 
                            n_levels,
                            shuffle_type = "simple",
                            n_prop = 0.5,
                            verbose = FALSE) {
  ## generate independent synthetic marginals
  ## via interpolated order statistics
  M_s <- GenerateSyntheticMarginalsIOS(dat, n_prop)
  
  ## perform SJPPDS on the data matrix
  X_star <- SJPPDS(dat = dat, 
                      n_levels,
                      shuffle_type,
                      verbose)
  
  ## compute ranks of shuffled data
  R_star <- GetDataRankMatrix(X_star)
  
  ## match ranks
  Y <- MatchRanks(M_s, R_star)
  
  return(Y)
}


MixedSyntheticSJPPDS <- function(dat,
                                 num_variables,
                                 cat_variables,
                                 n_levels,
                                 shuffle_type = "simple",
                                 n_prop = 0.5,
                                 verbose = FALSE) {
  
  ## grab the numeric and categorical data
  X_n <- dat[, num_variables, drop = FALSE]
  X_c <- dat[, cat_variables, drop = FALSE]
  
  ## get numeric and categorical variable names
  nms <- colnames(dat)
  num_variables <- nms[num_variables]
  cat_variables <- nms[cat_variables]
  
  ## convert categorical variables to numerical rank encoding
  aux <- CategoricalToNumeric(X_c)
  R_c <- aux$dat_N
  lnr <- aux$level_numeric_ranges
  
  ## concatenate the original numeric data with the 
  ## transformed categorical data
  W <- cbind(X_n, R_c)
  
  ## perform SJPPDS on the rank matrix
  W_star <- SJPPDS(dat = W, 
                   n_levels,
                   shuffle_type,
                   verbose)
  
  ## generate independent synthetic marginals
  ## via interpolated order statistics
  M_s <- GenerateSyntheticMarginalsIOS(X_n, n_prop)
  
  ## match ranks for numeric data
  R_n_star <- GetDataRankMatrix(W_star[, num_variables, drop = FALSE])
  W_n <- MatchRanks(M_s, R_n_star)
  
  ## transform back the shuffled numerical rank encodings to shuffled 
  ## categorical data
  W_c <- NumericToCategorical(dat_N = W_star[, cat_variables, drop = FALSE],
                              level_numeric_ranges = lnr)
  
  ## concatenate numeric and categorical data
  X_s <- cbind(W_n, W_c)
  
  ## reorder the columns of the synthetic data to match the order in 
  ## the original data 
  X_s <- X_s[, colnames(dat)]
  
  return(X_s)
}




SynTabSJPPDS <- function(dat, 
                         num_variables,
                         cat_variables,
                         n_levels,
                         shuffle_type = "simple",
                         n_prop = 0.5,
                         verbose = TRUE) {
  
  ## numerical data case
  if (!is.null(num_variables) & is.null(cat_variables)) {
    sdat <- SyntheticSJPPDS(dat = dat, 
                            n_levels = n_levels, 
                            shuffle_type = shuffle_type,
                            n_prop = n_prop,
                            verbose = verbose)
  }
  
  ## categorical data case
  if (is.null(num_variables) & !is.null(cat_variables)) {
    sdat <- CatSJPPDS(dat = dat, 
                      n_levels = n_levels, 
                      shuffle_type = shuffle_type, 
                      verbose = verbose)
  }
  
  ## mixed data case
  if (!is.null(num_variables) & !is.null(cat_variables)) {
    sdat <- MixedSyntheticSJPPDS(dat = dat, 
                                 num_variables = num_variables,
                                 cat_variables = cat_variables,
                                 n_levels = n_levels, 
                                 shuffle_type = shuffle_type, 
                                 n_prop = n_prop,
                                 verbose = verbose)
  }
  
  return(sdat)
}


##########################################
## optimization functions
##########################################

library(Rfast)
library(caret)

OneHotEncoding <- function(dat) {
  dummy <- dummyVars(" ~ .", data = dat)
  dat_oh <- data.frame(predict(dummy, newdata = dat))
  
  return(dat_oh)
}


ComputeDCR <- function(dat_o, dat_s, distance_type = "euclidean") {
  ## compute the distance matrix
  ##
  ## we set: xnew = dat_s
  ##         x = dat_o
  ##
  ## first row of dm:
  ## dm[1, 1] = distance between dat_s[1,] and dat_o[1,]
  ## dm[1, 2] = distance between dat_s[1,] and dat_o[2,]
  ## dm[1, 3] = distance between dat_s[1,] and dat_o[3,]
  ## ...
  ##
  ## second row of dm:
  ## dm[2, 1] = distance between dat_s[2,] and dat_o[1,]
  ## dm[2, 2] = distance between dat_s[2,] and dat_o[2,]
  ## dm[2, 3] = distance between dat_s[2,] and dat_o[3,]
  ## ...
  dm <- dista(xnew = as.matrix(dat_s), 
              x = as.matrix(dat_o), 
              type = distance_type)
  
  ## compute the minimal distance vector
  ## dm[1] = minimal distance between the first row of 
  ##         dat_o and all rows of dat_s
  ## dm[2] = minimal distance between the second row of 
  ##         dat_o and all rows of dat_s
  ## ...
  dm <- apply(dm, 2, min)
  dm <- as.vector(dm)
  
  return(dm)
}


OptSynTabDsDCR <- function(dat_train,
                           dat_test,
                           num_variables,
                           cat_variables,
                           n_prop = 0.5,
                           tuning_par_grid,
                           use_early_stop = TRUE) {
  ## Inputs:
  ## dat_train: training data (R dataframe).
  ## dat_test: test data (R dataframe).
  ## num_variables: Indices of numeric variables.
  ## cat_variables: Indices of categorical variables.
  ## n_prop: Proportion for synthetic marginal generation.
  ## tuning_par_grid: grid of number of categories/levels
  ## use_early_stop: whether to use early stop
  ##
  ## Output:
  ## DRC: data matrix containing the DCR values between 
  ##      training and test sets and between training and 
  ##      synthetic data sets
  
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
  
  ## get median of DCR between training and test sets 
  test_median <- median(DCR[, 1])
  
  stop_flag <- FALSE
  i <- 1
  while (i <= num_param & stop_flag == FALSE) {
    cat(tuning_par_grid[i], "\n")
    
    ## generate synthetic data for tuning parameter at position i
    dat_synth <- SynTabSJPPDS(dat = dat_train, 
                              num_variables = num_variables,
                              cat_variables = cat_variables,
                              n_levels = tuning_par_grid[i],
                              n_prop = n_prop,
                              shuffle_type = "simple",
                              verbose = FALSE)
    
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
    
    ## test early stop condition
    if (use_early_stop) {
      syn_median <- median(DCR[, i + 1])
      if (syn_median <= test_median) {
        stop_flag <- TRUE
      }
    }
    
    i <- i + 1
  }
  DCR <- DCR[, 1:i]
  
  return(DCR)
}


GetModelsDCRs <- function(ori_data_file,
                          syn_data_files,
                          ori_data_path,
                          syn_data_path,
                          num_variables,
                          cat_variables,
                          methods_names,
                          scale_data = FALSE) {
  n_methods <- length(syn_data_files)
  odat <- read.csv(paste0(ori_data_path, ori_data_file))
  n <- nrow(odat)
  out <- matrix(NA, n, n_methods)
  colnames(out) <- methods_names
  
  ## perform one-hot-encoding of categorical data if necessary
  if (!is.null(cat_variables)) {
    odat <- OneHotEncoding(odat)
  }
  if (scale_data) {
    odat <- scale(odat)
  }
  
  
  for (i in seq(n_methods)) {
    cat("computing DCR for model: ", methods_names[i], "\n")
    sdat <- read.csv(paste0(syn_data_path, syn_data_files[i]))
    
    ## perform one-hot-encoding of categorical data if necessary
    if (!is.null(cat_variables)) {
      sdat <- OneHotEncoding(sdat)
    }
    
    if (scale_data) {
      sdat <- scale(sdat)
    }
    
    aux <- try(ComputeDCR(dat_o = odat, dat_s = sdat), silent = TRUE)
    if (!inherits(aux, "try-error")) {
      out[, i] <- aux
    }
    
  }
  
  return(out)
}


TrainTestDataSplit <- function(X, my_seed) {
  ## set random seed
  set.seed(my_seed)
  
  ## randomly sample indexes for training and test sets
  n <- nrow(X)
  n_sub <- floor(n/2)
  idx_train <- sample(seq(n), n_sub, replace = FALSE)
  idx_test <- setdiff(seq(n), idx_train)
  
  ## if neccesary, remove one sample to makes training and 
  ## test set sizes equal
  if (idx_train < idx_test) {
    idx_test <- idx_test[-1]
  }
  
  ## split the data
  X_train <- X[idx_train,]
  X_test <- X[idx_test,]
  
  return(list(X_train = X_train,
              X_test = X_test))
}


CramerV <- function(v1, v2) {
  n <- length(v1)
  n1 <- length(unique(v1))
  n2 <- length(unique(v2))
  chisq.stat <- as.numeric(chisq.test(v1, v2, correct = TRUE)$statistic)
  sqrt(chisq.stat/(n * min(c(n1-1, n2-1))))
}


## Computes the square root of the R2 of a linear model where
## the response is a numeric variable and the covariate is a 
## categorical variable. (This reduces to the absolute value 
## of the correlation when the covariate is numeric.)
NumCatCor <- function(dat,
                      num_var,
                      cat_var) {
  aux <- summary(lm(dat[, num_var] ~ dat[, cat_var]))
  
  return(sqrt(aux$r.squared))
}


ComputeAssociationMatrix <- function(dat,
                                     num_variables,
                                     cat_variables) {
  ## get variable names
  nms <- colnames(dat)
  
  n_num <- length(num_variables)
  n_cat <- length(cat_variables)
  
  AM <- matrix(1, n_num + n_cat, n_num + n_cat)
  rownames(AM) <- nms
  colnames(AM) <- nms
  
  ## compute pearson correlations between numeric variables
  if (n_num > 1) {
    for (i in num_variables) {
      for (j in num_variables) {
        AM[i, j] <- cor(dat[, i], dat[, j])
        AM[j, i] <- AM[i, j]
      }
    }
  }
  
  ## compute Cramer V statistics between categorical variables
  if (n_cat > 1) {
    for (i in cat_variables) {
      for (j in cat_variables) {
        AM[i, j] <- CramerV(dat[, i], dat[, j])
        AM[j, i] <- AM[i, j]
      }
    }
  }  
  
  ## compute sqrt(R2) between numeric and categorical variables
  if (n_num > 0 & n_cat > 0) {
    for (i in num_variables) {
      for (j in cat_variables) {
        AM[i, j] <- NumCatCor(dat = dat, num_var = i, cat_var = j)
        AM[j, i] <- AM[i, j]
      }
    }
  }
  
  return(AM)
} 

