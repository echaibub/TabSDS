
code_path <- ""
source(paste0(code_path, "utility_functions_for_SynTabSJPPDS.R"))


CategoricalToNumericShuffled <- function(dat_C) {
  n <- nrow(dat_C)
  n_var <- ncol(dat_C)
  dat_N <- data.frame(matrix(NA, n, n_var))
  names(dat_N) <- names(dat_C)
  level_numeric_ranges <- vector(mode = "list", length = n_var)
  names(level_numeric_ranges) <- colnames(dat_C)
  for (i in seq(n_var)) {
    tb <- table(dat_C[, i])
    variable_levels <- names(tb)
    
    ## randomly change the order of the table columns
    ## in order to change the mapping between the
    ## variable levels and numerical encondings
    sidx <- sample(length(variable_levels))
    tb <- tb[sidx]
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

library(corrplot)

#########################################
## mushroom
#########################################

dat_path <- "~/synthetic_privacy/tabular/SynTabSJPPDS/outputs/mushroom/simulated_datasets/"

datC <- read.csv(paste0(dat_path, "train_set.csv"))

set.seed(123456)
datC <- datC[, sample(seq(21), 7, replace = FALSE)]

cv0 <- ComputeAssociationMatrix(datC, num_variables = NULL, cat_variables = seq(ncol(datC)))

#pdf(file = paste0(manus_path, "arbitrariness_example_orig_data.pdf"), width = 6, height = 6)
corrplot(cv0)
#dev.off()

colnames(datC) <- NULL

head(datC)
var_names <- colnames(datC)

set.seed(1234)
aux1 <- CategoricalToNumericShuffled(datC)
datN1 <- aux1$dat_N
su1 <- aux1$level_numeric_ranges

set.seed(4321)
aux2 <- CategoricalToNumericShuffled(datC)
datN2 <- aux2$dat_N
su2 <- aux2$level_numeric_ranges

corrplot(cor(datN1))
corrplot(cor(datN2))

n_c <- 200

set.seed(123456789)
sdatN1 <- SJPPDS(dat = datN1, n_levels = n_c, shuffle_type = "simple")
sdatN2 <- SJPPDS(dat = datN2, n_levels = n_c, shuffle_type = "simple")
colnames(sdatN1) <- NULL
colnames(sdatN2) <- NULL

sdatC1 <- NumericToCategorical(dat_N = sdatN1,
                               level_numeric_ranges = su1)
sdatC2 <- NumericToCategorical(dat_N = sdatN2,
                               level_numeric_ranges = su2)


cv1 <- ComputeAssociationMatrix(sdatC1, num_variables = NULL, cat_variables = seq(ncol(datC)))
cv2 <- ComputeAssociationMatrix(sdatC2, num_variables = NULL, cat_variables = seq(ncol(datC)))


corrplot(cv1)
corrplot(cv2)

manus_path <- ""

my_mar <- c(0.5, 0.5, 1, 0.5)
label_cex <- 1.5
my_line <- 2
my_at <- 0.25

#pdf(file = paste0(manus_path, "arbitrariness_example.pdf"), width = 6, height = 4)
par(mfrow = c(2, 3))
corrplot(cor(datN1), mar = my_mar, main = "cor. encoding a")
mtext(side = 3, "(a)", at = my_at, cex = label_cex, line = my_line)
corrplot(cor(sdatN1), mar = my_mar, main = "cor. SJPPDS encode a")
mtext(side = 3, "(b)", at = my_at, cex = label_cex, line = my_line)
corrplot(cv1, mar = my_mar, main = "Cramer V encoding a")
mtext(side = 3, "(c)", at = my_at, cex = label_cex, line = my_line)
corrplot(cor(datN2), mar = my_mar, main = "cor. encoding b")
mtext(side = 3, "(d)", at = my_at, cex = label_cex, line = my_line)
corrplot(cor(sdatN2), mar = my_mar, main = "cor. SJPPDS encode b")
mtext(side = 3, "(e)", at = my_at, cex = label_cex, line = my_line)
corrplot(cv2, mar = my_mar, main = "Cramer V encoding b")
mtext(side = 3, "(f)", at = my_at, cex = label_cex, line = my_line)
#dev.off()
par(mfrow = c(1, 1))



