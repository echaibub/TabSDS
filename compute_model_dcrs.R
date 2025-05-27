
code_path <- "" ## path for the folder containing the code
source(paste0(code_path, "utility_functions_for_SynTabSJPPDS.R"))


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

n_datasets <- length(dataset_names)

dcrs <- vector(mode = "list", length = n_datasets)

num_variables_list <- vector(mode = "list", length = n_datasets)
cat_variables_list <- vector(mode = "list", length = n_datasets)
type_list <- vector(mode = "list", length = n_datasets)

## abalone
num_variables_list[[1]] <- c(2:9)
cat_variables_list[[1]] <- c(1)
type_list[[1]] <- "regr"

## bank marketing
num_variables_list[[2]] <- c(1:7)
cat_variables_list[[2]] <- c(8)
type_list[[2]] <- "class"

## california housing
num_variables_list[[3]] <- c(1:9)
cat_variables_list[[3]] <- NULL
type_list[[3]] <- "regr"

## credit
num_variables_list[[4]] <- c(1:10)
cat_variables_list[[4]] <- c(11)
type_list[[4]] <- "class"

## diabetes 130US
num_variables_list[[5]] <- c(1:7)
cat_variables_list[[5]] <- c(8)
type_list[[5]] <- "class"

## electricity
num_variables_list[[6]] <- c(1:7)
cat_variables_list[[6]] <- c(8)
type_list[[6]] <- "class"

## eye movement
num_variables_list[[7]] <- c(1:20)
cat_variables_list[[7]] <- c(21)
type_list[[7]] <- "class"

## house 16h
num_variables_list[[8]] <- c(1:16)
cat_variables_list[[8]] <- c(17)
type_list[[8]] <- "class"

## magic telescope
num_variables_list[[9]] <- c(1:10)
cat_variables_list[[9]] <- c(11)
type_list[[9]] <- "class"

## pol
num_variables_list[[10]] <- c(1:26)
cat_variables_list[[10]] <- c(27)
type_list[[10]] <- "class"


ori_data_file <- "train_set.csv"

tabsds_files <- c("syn_tab_sjppds_20_0.5.csv", ## abalone
                  "syn_tab_sjppds_100_0.5.csv", ## bank marketing
                  "syn_tab_sjppds_200_0.5.csv", ## california
                  "syn_tab_sjppds_1000_0.5.csv", ## credit
                  "syn_tab_sjppds_35_0.5.csv", ## diabetes
                  "syn_tab_sjppds_20_0.5.csv", ## electricity
                  "syn_tab_sjppds_20_0.5.csv", ## eye movement
                  "syn_tab_sjppds_1000_0.5.csv", ## house 16h
                  "syn_tab_sjppds_25_0.5.csv", ## magic telescope
                  "syn_tab_sjppds_15_0.5.csv") ## pol

additional_data_files <- c("syn_ddpm.csv",
                           "syn_arf.csv",
                           "syn_tvae.csv",
                           "syn_ctgan.csv",
                           "syn_bayesnet.csv",
                           "syn_adsgan.csv",
                           "syn_pategan.csv",
                           "syn_smote_k20.csv",
                           "syn_smote_k5.csv")

methods_names <- c("test_set",
                   "TabSDS",
                   "DDPM",
                   "ARF",  
                   "TVAE",
                   "CTGAN",
                   "BayesNet",
                   "ADSGAN",
                   "PATEGAN",
                   "SMOTE_k20",
                   "SMOTE_k5")

path_part_1 <- "~/outputs/"
path_part_2 <- "/simulated_datasets/"

for (i in seq(n_datasets)) {
  cat(dataset_names[i], "\n")
  
  syn_data_files <- c("test_set.csv",
                      tabsds_files[i],
                      additional_data_files)
  
  data_path <- paste0(path_part_1, dataset_names[i], path_part_2)
  
  dcrs[[i]] <- GetModelsDCRs(ori_data_file = "train_set.csv",
                             syn_data_files = syn_data_files,
                             ori_data_path = data_path,
                             syn_data_path = data_path,
                             num_variables = num_variables_list[[i]],
                             cat_variables = cat_variables_list[[i]],
                             methods_names = methods_names)
}


#save(dcrs, 
#     file = "dcrs_additional_methods_with_dp_methods.RData",
#     compress = TRUE)


###########################
## scaling the data
###########################

dcrs <- vector(mode = "list", length = n_datasets)

for (i in seq(n_datasets)) {
  cat(dataset_names[i], "\n")
  
  syn_data_files <- c("test_set.csv",
                      tabsds_files[i],
                      additional_data_files)
  
  data_path <- paste0(path_part_1, dataset_names[i], path_part_2)
  
  dcrs[[i]] <- GetModelsDCRs(ori_data_file = "train_set.csv",
                             syn_data_files = syn_data_files,
                             ori_data_path = data_path,
                             syn_data_path = data_path,
                             num_variables = num_variables_list[[i]],
                             cat_variables = cat_variables_list[[i]],
                             methods_names = methods_names,
                             scale_data = TRUE)
  
}


#save(dcrs, 
#     file = "dcrs_additional_methods_with_dp_methods_scaled.RData",
#     compress = TRUE)

