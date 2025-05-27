# utility functions implementing SynTabSJPPDS (TabSDS for short)

import numpy as np
import pandas as pd
from scipy.stats import rankdata


def categorize_variable(x, n_levels):
    """
    Categorizes a numeric array into a specified number of levels.

    Parameters:
    x : array-like
        A numeric array or list of values to be categorized.
    n_levels : int
        The number of categories/levels to divide the data into.

    Returns:
    out : list of str
        A list of categorical variables represented as strings.
    """
    # Generate the categorical bins and labels
    var_levels = list(range(1, n_levels + 1))
    categories = pd.cut(x, bins=n_levels, labels=var_levels)

    # Convert the output to a list of strings
    out = categories.astype(str).tolist()

    return out


def categorize_data(dat, n_levels):
    """
    Categorizes all columns in a numerical dataset into specified levels.

    Parameters:
    dat : pandas.DataFrame
        A DataFrame with numeric data to be categorized.
    n_levels : int
        The number of categories/levels to divide each column into.

    Returns:
    dat_C : pandas.DataFrame
        A DataFrame of categorical variables.
    """
    dat_C = dat.copy()
    for col in dat_C.columns:
        dat_C[col] = categorize_variable(dat_C[col], n_levels)
    return dat_C


def joint_probability_preserving_data_shuffling_simple(dat, dat_C):
    """
    Performs joint probability-preserving data shuffling on a dataset.
    (Implements Algorithm 7 in the paper.)

    Parameters:
    dat : pandas.DataFrame
        A numerical dataset.
    dat_C : pandas.DataFrame
        A categorical dataset with the same structure as `dat`.

    Returns:
    dat_S : pandas.DataFrame
        A masked dataset with shuffled data preserving joint probabilities.
    """
    dat_S = dat.copy()
    p = dat.shape[1]  # Number of columns
    last_col_classes = dat_C.iloc[:, -1].unique()  # Unique classes in the last column of dat_C
    col_idx = list(range(p - 1))  # Indices for columns except the last one

    # Shuffle rows within each class in the last column of dat_C
    for cls in last_col_classes:
        idx = dat_C.index[dat_C.iloc[:, -1] == cls].tolist()  # Rows corresponding to the current class
        shuffled_idx = np.random.permutation(idx)  # Shuffle indices
        dat_S.iloc[idx, col_idx] = dat.iloc[shuffled_idx, col_idx].values

    # Shuffle the entire dataset
    dat_S = dat_S.sample(frac=1).reset_index(drop=True)

    return dat_S



def sequential_jppds(dat, n_levels, shuffle_type="simple", verbose=False):
    """
    Sequential Joint Probability Preserving Data Shuffling (SJPPDS) using pandas DataFrames.
    (Implements Algorithm 6 in the paper.)

    Parameters:
    dat : pd.DataFrame
        A numerical dataset (pandas DataFrame).
    n_levels : int
        Number of categories/levels.
    shuffle_type : str, optional
        "simple" for simplified SJPPDS, "full" for full SJPPDS approach (only "simple" is implemented).
    verbose : bool, optional
        Whether to print the computation progress (default is False).

    Returns:
    dat_S : pd.DataFrame
        A masked dataset (pandas DataFrame).
    """
    def generate_shuffled_dataset(dat, n_levels, shuffle_type):
        # Convert the dataset to categorical levels
        dat_C = categorize_data(dat, n_levels)
        if shuffle_type == "simple":
            return joint_probability_preserving_data_shuffling_simple(dat, dat_C)
        elif shuffle_type == "full":
            return joint_probability_preserving_data_shuffling_full(dat, dat_C)
        else:
            raise ValueError("Invalid shuffle_type. Choose 'simple' or 'full'.")

    # Initialize the number of columns in the dataset
    p = dat.shape[1]

    if verbose:
        print("shuffle", 1)

    # Shuffle the dataset
    dat_S = generate_shuffled_dataset(dat, n_levels, shuffle_type)

    # Sequential shuffling
    for i in range(1, p):
        if verbose:
            print("shuffle", i + 1)
        
        # Move the first column to the end, then shuffle the rest
        dat_S = pd.concat([dat_S.iloc[:, 1:], dat_S.iloc[:, [0]]], axis=1)
        
        # Shuffle the updated dataset
        dat_S = generate_shuffled_dataset(dat_S, n_levels, shuffle_type)

    # Revert to the original column order
    dat_S = pd.concat([dat_S.iloc[:, 1:], dat_S.iloc[:, [0]]], axis=1)

    return dat_S



def ios_sampling(x, n_prop=0.5):
    """
    Implements the InterpolatedOrderStatsSampling algorithm. (Algorithm 9 in the paper.)
    Args:
        x (array-like): Input data array.
        n_prop (float): Proportion of the input data to sample.
    Returns:
        y (np.ndarray): Sampled values.
    """
    def ios_sampler(x_1, x_2):
        """
        Generates random values uniformly between the lower and upper bounds of sorted arrays.
        Args:
            x_1 (np.ndarray): First subset of data.
            x_2 (np.ndarray): Second subset of data.
        Returns:
            y (np.ndarray): Random values generated between lower and upper bounds.
        """
        x_1_s = np.sort(x_1)
        x_2_s = np.sort(x_2)
        n = len(x_1)
        lower_bound = np.minimum(x_1_s, x_2_s)
        upper_bound = np.maximum(x_1_s, x_2_s)
        y = np.random.uniform(lower_bound, upper_bound, size=n)
        return y

    # Main function logic
    n = len(x)
    seq_n = np.arange(n)
    nn = int(np.floor(n * n_prop))
    n_draws = int(np.ceil(n / nn))

    y = np.empty((n_draws, nn))

    for i in range(n_draws):
        idx_1 = np.random.choice(n, nn, replace=False)
        idx_remain = np.setdiff1d(seq_n, idx_1)
        idx_2 = np.random.choice(idx_remain, nn, replace=False)
        y[i, :] = ios_sampler(x[idx_1], x[idx_2])

    y = y.flatten()
    y = np.random.choice(y, n, replace=False)

    return y


def generate_synthetic_marginals_ios(dat_o, n_prop=0.5):
    """
    Generates synthetic marginal distributions using IOS sampling.
    (Implements Algorithm 8 in the paper.)

    Parameters:
        dat_o (pd.DataFrame): Original data as a Pandas DataFrame.
        n_prop (float): Proportion for IOSSampling.

    Returns:
        pd.DataFrame: Synthetic marginal distributions as a DataFrame.
    """
    n, p = dat_o.shape
    dat_s = pd.DataFrame(index=range(n), columns=dat_o.columns)
    
    for i in range(p):
        dat_s.iloc[:, i] = ios_sampling(dat_o.iloc[:, i], n_prop)
    
    return dat_s


def ranking_with_random_tie_breaking(x):
    """
    Rank data with random tie-breaking by shuffling indices.
    
    Args:
        x (array-like): Input data to rank.
        
    Returns:
        np.ndarray: Ranks with random tie-breaking.
    """
    x = np.array(x)
    # Shuffle the indices to break ties randomly
    shuffled_indices = np.random.permutation(len(x))
    shuffled_x = x[shuffled_indices]
    # Rank the shuffled data
    ranks = rankdata(shuffled_x, method='ordinal')
    # Reorder ranks back to original order
    return ranks[np.argsort(shuffled_indices)]


def get_data_rank_matrix(X):
    """
    Computes the rank matrix of a DataFrame, with ties broken randomly.

    Parameters:
        X (pd.DataFrame): Input DataFrame where each column is ranked 
        individually.

    Returns:
        pd.DataFrame: Rank matrix with the same shape as the input.
    """
    R = X.copy()
    p = X.shape[1]
    
    for j in range(p):
        R.iloc[:, j] = ranking_with_random_tie_breaking(X.iloc[:, j])
    
    return R


def match_ranks_old(synthetic_marginals_matrix, rank_matrix):
    """
    Matches the ranks between synthetic marginals and a rank matrix.
    (Implements Algorithm 10 in the paper.)

    Parameters:
        synthetic_marginals_matrix (pd.DataFrame): Synthetic marginals matrix.
        rank_matrix (pd.DataFrame): Rank matrix.

    Returns:
        pd.DataFrame: Matrix with matched ranks.
    """
    dat_s = synthetic_marginals_matrix.copy()
    p = rank_matrix.shape[1]
    n = rank_matrix.shape[0]
    
    for j in range(p):
        # Compute ranks for the j-th column of synthetic_marginals_matrix with ties broken randomly
        r_j = ranking_with_random_tie_breaking(synthetic_marginals_matrix.iloc[:, j])
        
        for i in range(n):
            # Find the index where rank matches
            ii = np.where(r_j == rank_matrix.iloc[i, j])[0][0]
            dat_s.iloc[i, j] = synthetic_marginals_matrix.iloc[ii, j]
    
    return dat_s


def match_ranks(synthetic_marginals_matrix: pd.DataFrame, rank_matrix: pd.DataFrame):
    """
    Matches the ranks between synthetic marginals and a rank matrix.

    Parameters:
        synthetic_marginals_matrix (pd.DataFrame): Synthetic marginals matrix.
        rank_matrix (pd.DataFrame): Rank matrix.

    Returns:
        pd.DataFrame: Matrix with matched ranks.
    """
    dat_s = synthetic_marginals_matrix.copy()
    p = rank_matrix.shape[1]
    n = rank_matrix.shape[0]
    
    for j in range(p):
        # Sort the synthetic data column
        sorted_syn_dat = np.sort(synthetic_marginals_matrix.iloc[:, j].values)
        
        # Match sorted values to the provided ranks
        # Subtract 1 from the ranks because ranks are 1-based (like in R) 
        dat_s.iloc[:, j] = sorted_syn_dat[rank_matrix.iloc[:, j].values.astype(int) - 1]
    
    return dat_s


def synthetic_sjppds(dat, 
                    n_levels, 
                    shuffle_type="simple", 
                    verbose=False, 
                    n_prop=0.5):
    """
    Performs Synthetic Sequential Joint Probability Preserving Data Shuffling.
    (Implements Algorithm 2 in the paper.)

    Parameters:
        dat (pd.DataFrame): Input data as a Pandas DataFrame.
        n_levels (int): Number of levels for SequentialJPPDS.
        shuffle_type (str): Type of shuffle ("simple" or others).
        verbose (bool): Whether to print intermediate steps.
        n_prop (float): Proportion for synthetic marginal generation.

    Returns:
        pd.DataFrame: Resulting synthetic data.
    """
    # Generate independent synthetic marginals via interpolated order statistics
    M_s = generate_synthetic_marginals_ios(dat, n_prop)

    # Perform SJPPDS on the data matrix
    X_star = sequential_jppds(dat=dat, 
                                n_levels=n_levels, 
                                shuffle_type=shuffle_type, 
                                verbose=verbose)

    # Compute ranks of the shuffled data
    R_star = get_data_rank_matrix(X_star)
    
    # Match ranks
    X_s = match_ranks(M_s, R_star)
    
    return X_s



def categorical_to_numeric(dat_C):
    """
    Converts categorical data in a NumPy array to numeric values while generating numeric ranges for each level.
    (Implements Algorithm 11 in the paper)

    Parameters:
        dat_C (pd.DataFrame): DataFrame with categorical variables

    Returns:
        dict: A dictionary with:
              - 'dat_N': DataFrame with numeric values for each categorical level.
              - 'level_numeric_ranges': List of dictionaries containing the numeric ranges for each variable's levels.
    """
    # get column names
    cnms = dat_C.columns.tolist()
    
    # convert to numpy array
    dat_C = dat_C.to_numpy()
    
    n, n_var = dat_C.shape
    dat_N = np.empty_like(dat_C, dtype=np.float64)  # Initialize with float for NaN support
    dat_N[:] = np.nan  # Fill with NaN
    
    level_numeric_ranges = []

    for i in range(n_var):
        column = dat_C[:, i]
        unique_levels, counts = np.unique(column, return_counts=True)
        cumulative_counts = np.cumsum(np.concatenate(([0], counts)))
        n_levels = len(unique_levels)
        
        level_ranges = []
    
        for j in range(n_levels):
            level = unique_levels[j]
            idx = np.where(column == level)[0]  # Get indices of this level
            lower_bound = cumulative_counts[j] + 1
            upper_bound = cumulative_counts[j + 1]
            dat_N[idx, i] = np.random.permutation(np.arange(lower_bound, upper_bound + 1))
            level_ranges.append({
                "level": level,
                "lower_bound": lower_bound,
                "upper_bound": upper_bound
            })
        
        level_numeric_ranges.append(level_ranges)

    # convert dat_N to pandas DataFrame
    dat_N = pd.DataFrame(dat_N, columns = cnms)

    return {"dat_N": dat_N, "level_numeric_ranges": level_numeric_ranges}


def numeric_to_categorical(dat_N, level_numeric_ranges):
    """
    Converts numeric rank data back into categorical data based on level numeric ranges.
    (Implements Algorithm 12 in the paper.)

    Parameters:
        dat_N (np.ndarray): A 2D NumPy array of numeric data.
        level_numeric_ranges (list): A list of dictionaries where each dictionary contains
                                     'level', 'lower_bound', and 'upper_bound' for each variable.

    Returns:
        np.ndarray: A 2D NumPy array of categorical data.
    """
    # get column names
    cnms = dat_N.columns.tolist()
    
    # convert to numpy array
    dat_N = dat_N.to_numpy()
    
    n, n_var = dat_N.shape
    dat_C = np.empty_like(dat_N, dtype=object)  # Initialize with object type for categorical data

    for i in range(n_var):
        ranges = level_numeric_ranges[i]  # Get ranges for the i-th variable
        for level_info in ranges:
            level = level_info["level"]
            lower_bound = level_info["lower_bound"]
            upper_bound = level_info["upper_bound"]
            
            # Assign the categorical level where numeric values match the range
            idx = (dat_N[:, i] >= lower_bound) & (dat_N[:, i] <= upper_bound)
            dat_C[idx, i] = level

    # convert dat_C to pandas DataFrame
    dat_C = pd.DataFrame(dat_C, columns = cnms)
    
    return dat_C


def cat_sjppds(dat, n_levels, shuffle_type="simple", verbose=True):
    """
    Implements the CategoricalSJPPDS algorithm. (Algorithm 4 in the paper.)

    Parameters:
        dat (pd.DataFrame): Input categorical data.
        n_levels (int): Number of levels for the SJPPDS.
        shuffle_type (str): Type of shuffle ("simple" or others).
        verbose (bool): Whether to print intermediate steps.

    Returns:
        pd.DataFrame: Shuffled categorical data.
    """
    # Convert categorical variables to numerical encoding
    aux = categorical_to_numeric(dat)
    dat_C2N = aux["dat_N"]
    level_numeric_ranges = aux["level_numeric_ranges"]
    
    # Apply SJPPDS to the numerical data
    sdat_C2N = sequential_jppds(dat_C2N, n_levels=n_levels, shuffle_type=shuffle_type, verbose=verbose)
    
    # Transform shuffled numeric encodings back to categorical variables
    sdat = numeric_to_categorical(sdat_C2N, level_numeric_ranges)
    
    return sdat


def mixed_synthetic_sjppds(dat, 
                         num_variables, 
                         cat_variables, 
                         n_levels, 
                         shuffle_type="simple", 
                         verbose=False, 
                         n_prop=0.5):
    """
    Implements the MixedSyntheticSJPPDS algorithm. (Algorithm 5 in the paper.)

    Parameters:
        dat (pd.DataFrame): Input data matrix (numeric, categorical, or mixed).
        num_variables (list): Indices of numeric variables.
        cat_variables (list): Indices of categorical variables.
        n_levels (int): Number of levels for SJPPDS.
        shuffle_type (str): Type of shuffle ("simple" or others).
        verbose (bool): Whether to print intermediate steps.
        n_prop (float): Proportion for synthetic marginal generation.

    Returns:
        pd.DataFrame: Synthetic shuffled data matrix with the same structure as the input.
    """    
    # Replace variable positions with variable names
    num_variable_names = dat.columns[num_variables]
    cat_variable_names = dat.columns[cat_variables]
    
    # Split numeric and categorical data
    X_n = dat[num_variable_names]
    X_c = dat[cat_variable_names]

    # Convert categorical variables to numerical rank encoding
    aux = categorical_to_numeric(X_c)
    R_c = aux["dat_N"]
    lnr = aux["level_numeric_ranges"]

    # Concatenate the original numeric data and the transformed categorical data
    W = pd.concat([X_n, R_c], axis=1)

    # Perform SJPPDS on the rank matrix
    W_star = sequential_jppds(dat=W, 
                                 n_levels=n_levels, 
                                 shuffle_type=shuffle_type, 
                                 verbose=verbose)

    # Generate independent synthetic marginals via interpolated order statistics
    M_s = generate_synthetic_marginals_ios(X_n, n_prop)

    # Match ranks for numeric data
    R_n_star = get_data_rank_matrix(W_star[num_variable_names])
    W_n = match_ranks(M_s, R_n_star)

    # Transform back the shuffled numerical rank encodings to shuffle categorical data
    W_c = numeric_to_categorical(
        dat_N=W_star[cat_variable_names], 
        level_numeric_ranges=lnr
    )
    
    X_s = pd.concat([W_n, W_c], axis=1)

    # Reorder columns to match the order in the original data
    X_s = X_s[dat.columns]
    
    return X_s


def synthetic_tab_sjppds(dat, 
                         num_variables, 
                         cat_variables, 
                         n_levels, 
                         shuffle_type="simple", 
                         verbose=False, 
                         n_prop=0.5):
    """
    Implements TabSDS algorithm. (Algorithm 1 in the paper.) 

    Parameters:
        dat (pd.DataFrame): Input data matrix (numeric, categorical, or mixed).
        num_variables (list): Indices of numeric variables.
        cat_variables (list): Indices of categorical variables.
        n_levels (int): Number of levels for SJPPDS.
        shuffle_type (str): Type of shuffle (only "simple" is implemented at this point).
        verbose (bool): Whether to print intermediate steps.
        n_prop (float): Proportion for synthetic marginal generation.

    Returns:
        pd.DataFrame: Synthetic shuffled data matrix with the same structure as the input.
    """
    dat = dat.reset_index(drop=True)

    # Numeric data case 
    if num_variables is not None and cat_variables is None:
        sdat = synthetic_sjppds(dat, 
                                n_levels=n_levels, 
                                shuffle_type=shuffle_type, 
                                verbose=verbose,
                                n_prop=n_prop)

    # Categorical data case
    elif num_variables is None and cat_variables is not None:
        sdat = cat_sjppds(dat, 
                          n_levels=n_levels, 
                          shuffle_type=shuffle_type, 
                          verbose=verbose)
    
    # Mixed data case
    elif num_variables is not None and cat_variables is not None:
        sdat = mixed_synthetic_sjppds(dat, 
                                      num_variables=num_variables,
                                      cat_variables=cat_variables,
                                      n_levels=n_levels, 
                                      shuffle_type=shuffle_type, 
                                      verbose=verbose,
                                      n_prop=n_prop)
    else:
        raise ValueError("At least one of num_variables or cat_variables must be specified.")
    
    return sdat


