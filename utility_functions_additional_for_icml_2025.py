
from tqdm import tqdm
from scipy.spatial.distance import cdist
from sklearn.preprocessing import OneHotEncoder


def enforce_dtypes(dat, 
                   num_variables, 
                   cat_variables):
    """
    Enforce "float64" type for numeric variables and "object" type for the
    categorical variables
    Parameters:
        dat (pd.DataFrame): Input data matrix (numeric, categorical, or mixed).
        num_variables (list): Indices of numeric variables.
        cat_variables (list): Indices of categorical variables.

    Returns:
    pd.DataFrame: with transformed data types
    """
    if num_variables is not None and cat_variables is None:
        dat_N = pd.DataFrame(dat.iloc[:, num_variables], dtype = "float64")
        dat = dat_N

    elif num_variables is None and cat_variables is not None:
        dat_C = pd.DataFrame(dat.iloc[:, cat_variables], dtype = "str")
        dat = dat_C

    elif num_variables is not None and cat_variables is not None:
        dat_N = pd.DataFrame(dat.iloc[:, num_variables], dtype = "float64")
        dat_C = pd.DataFrame(dat.iloc[:, cat_variables], dtype = "str")
        dat = pd.concat([dat_N, dat_C], axis=1)
        # Reorder columns to match the order in the original data
        reordered_indices = num_variables + cat_variables
        dat = dat.iloc[:, np.argsort(reordered_indices)]

    else:
        raise ValueError("At least one of num_variables or cat_variables must be specified.")
    
    return dat 



def train_test_data_split(X, my_seed):
    """
    Splits the data X into training and testing sets, using a random seed.
    
    Parameters:
    X (pd.DataFrame): The input data DataFrame.
    my_seed (int): The random seed for reproducibility.
    
    Returns:
    dict: A dictionary containing the training and testing DataFrames.
          {'X_train': X_train, 'X_test': X_test}
    """
    # Set random seed
    np.random.seed(my_seed)
    
    # Get the total number of rows
    n = X.shape[0]
    n_sub = n // 2  # Floor division to get half the rows
    
    # Randomly sample indexes for the training set
    idx_train = np.random.choice(X.index, size=n_sub, replace=False)
    
    # Compute the test indexes as the set difference
    idx_test = X.index.difference(idx_train)

    # Adjust sizes to make them equal if necessary
    if len(idx_train) < len(idx_test):
        idx_test = idx_test[:-1]  # Remove the last test index
    
    # Split the data
    X_train = X.loc[idx_train]
    X_test = X.loc[idx_test]
    
    return {"X_train": X_train, "X_test": X_test}



def one_hot_encoding(dat, cat_variables):
    """
    Perform one-hot-encoding of the categorical variables in a pandas DataFrame

    Parameters:
        dat (pd.DataFrame): Input data matrix (numeric, categorical, or mixed).
        cat_variables (list): Indices of categorical variables.

    Returns:
        pd.DataFrame: one-hot-encoded data (first columns correspond to the 
                      untransformed numerical data).
    """
    # Extract the categorical and numerical columns using indexes
    categorical_data = dat.iloc[:, cat_variables]
    numerical_data = dat.drop(columns=dat.columns[cat_variables])
    
    # Initialize the OneHotEncoder
    encoder = OneHotEncoder(sparse_output=False, drop=None)
    
    # Fit and transform the categorical data
    encoded_array = encoder.fit_transform(categorical_data)
    
    # Create a DataFrame with the encoded categorical data
    encoded_df = pd.DataFrame(encoded_array, columns=encoder.get_feature_names_out(categorical_data.columns))
    
    # Combine numerical and encoded categorical data
    final_df = pd.concat([numerical_data.reset_index(drop=True), encoded_df.reset_index(drop=True)], axis=1)
    
    return final_df



def compute_dcr(dat_o, dat_s, distance_type="euclidean"):
    """
    Compute the minimum distance vector between two matrices using a specified metric.
    
    Parameters:
        dat_o (Pandas DataFrame): real data.
        dat_s (Pandas DataFrame): synthetic data.
        distance_type (str): Type of distance metric (e.g., "euclidean").
        
    Returns:
        minimal_distances: Vector of minimal distances.
    """
    # Compute the distance matrix
    dm = cdist(dat_s, dat_o, metric=distance_type)
    
    # Compute the minimal distance for each row of dat_o
    minimal_distances = dm.min(axis=0)  # Find minimum distance for each column in dm
    
    return minimal_distances



def syn_tab_sjppds_dcr(dat_train, 
                       dat_test, 
                       num_variables, 
                       cat_variables, 
                       n_prop=0.5, 
                       tuning_par_grid=None):
    """
    Computes DCR distributions for the TabSDS method over a grid of n_levels tuning 
    parameter.
    
    Parameters:
    - dat_train: Pandas DataFrame. Training data.
    - dat_test: Pandas DataFrame. Test data.
    - num_variables: List of indices for numeric variables.
    - cat_variables: List of indices for categorical variables.
    - n_prop: float. Proportion for synthetic marginal generation.
    - tuning_par_grid: List of tuning parameters for the number of categories/levels.
    
    Returns:
    - DCR: Pandas DataFrame. Data matrix containing the DCR values.
    """
    # Number of tuning parameters
    num_param = len(tuning_par_grid)

    # Initialize the DataFrame with columns for test and tuning parameters
    column_names = ["test"] + tuning_par_grid
    DCR = pd.DataFrame(np.nan, index=range(len(dat_train)), columns=column_names)
    
    # Compute DCR between training and test sets
    print("Computing DCR between training and test sets")
    if cat_variables:
        dat_train_oh = one_hot_encoding(dat_train, cat_variables)
        dat_test_oh = one_hot_encoding(dat_test, cat_variables)
        DCR["test"] = compute_dcr(dat_train_oh, dat_test_oh, distance_type="euclidean")
    else:
        DCR["test"] = compute_dcr(dat_train, dat_test, distance_type="euclidean")

    print("Computing DCR between training and synthetic datasets")
    for i in tqdm(range(num_param), desc="Tuning parameters", ncols=100):
        tuning_param = tuning_par_grid[i]    
        # Generate synthetic data for current tuning parameter
        dat_synth = synthetic_tab_sjppds(
            dat=dat_train,
            num_variables=num_variables,
            cat_variables=cat_variables,
            n_levels=tuning_param,
            n_prop=n_prop,
            shuffle_type="simple",
            verbose=False
        )

        dat_synth = enforce_dtypes(dat = dat_synth, 
                   num_variables = num_idx, 
                   cat_variables = cat_idx)
        
        # Compute DCR between training and synthetic data
        if cat_variables:
            dat_synth_oh = one_hot_encoding(dat_synth, cat_variables)
            DCR[tuning_param] = compute_dcr(dat_train_oh, dat_synth_oh, distance_type="euclidean")
        else:
            DCR[tuning_param] = compute_dcr(dat_train, dat_synth, distance_type="euclidean")
            
    return DCR



def process_adult_data(df):
    """
    Function to process the adult data. 
    (Without this processing both tvae and ctgan could not be benchmarked.)
    Parameters:
        df: original adult data
    Returns:
        df: the processed data
    """
    df = df.copy()
    df["native-country"] = df["native-country"].astype(str)
    df.loc[df["native-country"] != "United-States", "native-country"] = "Non-US"
    df.loc[df["native-country"] == "United-States", "native-country"] = "US"

    df["marital-status"] = df["marital-status"].replace(
        ["Divorced", "Married-spouse-absent", "Never-married", "Separated", "Widowed"],
        "Single",
    )
    df["marital-status"] = df["marital-status"].replace(
        ["Married-AF-spouse", "Married-civ-spouse"],
        "Married",
    )

    df["capitalgain"] = df["capitalgain"].astype(str)
    df["capitalgain"] = df["capitalgain"].replace(
        ["1", "2", "3", "4"],
        "1"
    )

    df["capitalloss"] = df["capitalloss"].astype(str)
    df["capitalloss"] = df["capitalloss"].replace(
        ["1", "2", "3", "4"],
        "1"
    )
    
    return df



def extract_summary(score_output,
                   synthesizer_names,
                   summary_name):
    """
    Function to extract the summary statistics from Synthcity's 
    Benchmarks function 

    Parameters:
        - score_output: output from Synthcity's Benchmarks function
        - synthesizer_names: names of the generators (need to match the 
                             names in score_output)
        - summary_name: the summary statistic to be extracted (e.g., mean, stddev)

    Returns:
        - pd.DataFrame: with the summary values for all metrics across all selected
                        synthesizers
    """
    aux = score_output.get(synthesizer_names[0])[summary_name]
    n_rows = aux.shape[0]
    n_cols = len(synthesizer_names)
    out = pd.DataFrame(index=aux.index, columns=synthesizer_names)
    for cls in synthesizer_names:
        out[cls] = score_output.get(cls)[summary_name]
        
    return out



def additive_noise_perturbation(dat: pd.DataFrame, noise_percent: float, num_variables: list) -> pd.DataFrame:
    """
    Adds independent Gaussian noise to each numeric variable of a dataset.
    
    Parameters:
    dat_o (pd.DataFrame): DataFrame containing numerical and potentially categorical variables.
    noise_percent (float): Proportion of noise (positive float).
    num_variables (list): Indexes of the columns containing numerical variables.
    
    Returns:
    pd.DataFrame: DataFrame containing the perturbed data.
    """
    dat_m = dat.copy()
    
    for j in num_variables:
        var_sd = dat.iloc[:, j].std()
        dat_m.iloc[:, j] += np.random.normal(0, 0.01 * noise_percent * var_sd, size=len(dat))
    
    return dat_m



def synth_smote(df, k, num_variables, target_variable, task_type="class"):
    """
    Implements a SMOTE-like algorithm to generate synthetic samples for oversampling.
    
    Parameters:
    df: Pandas DataFrame containing the data
    k: Number of nearest neighbors to use
    num_variables: List of column indices containing numerical variables
    target_variable: Column index for the target variable
    smote_type: "class" for classification, otherwise uses median split for regression
    
    Returns:
    synthetic_df: Pandas DataFrame containing the synthetic data
    """
    
    def generate_synthetic_data(data, knn_indices, num_variables):
        """
        Generates synthetic samples using interpolation between a sample and one of its k-nearest neighbors.
        
        Parameters:
        data: Subset of the original DataFrame (either positive or negative class)
        knn_indices: Indices of k-nearest neighbors for each sample
        num_variables: List of column indices for numerical variables
        
        Returns:
        synthetic_data: DataFrame with synthetic samples
        """
        synthetic_data = data.copy()
        
        for i in range(len(data)):
            x = data.iloc[i, num_variables].values.astype(float)
            nn_idx = np.random.choice(knn_indices[i])  # Randomly select one nearest neighbor
            x_nn = data.iloc[nn_idx, num_variables].values.astype(float)
            
            lambda_ = np.random.uniform(0, 1)  # Random interpolation factor
            synthetic_data.iloc[i, num_variables] = x + lambda_ * (x_nn - x)
        
        return synthetic_data
    
    # Split the dataset based on target variable (classification or regression)
    if task_type != "class":
        median_val = df.iloc[:, target_variable].median()
        idx_neg = df[df.iloc[:, target_variable] <= median_val].index
        idx_pos = df[df.iloc[:, target_variable] > median_val].index
    else:
        target_classes = df.iloc[:, target_variable].squeeze().unique()
        idx_neg = df[df.iloc[:, target_variable] == target_classes[0]].index
        idx_pos = df[df.iloc[:, target_variable] == target_classes[1]].index
    
    # Separate the data into two groups
    df_neg = df.loc[idx_neg]
    df_pos = df.loc[idx_pos]
    
    # Find k-nearest neighbors for each group
    knn_neg = NearestNeighbors(n_neighbors=k).fit(df_neg.iloc[:, num_variables])
    knn_pos = NearestNeighbors(n_neighbors=k).fit(df_pos.iloc[:, num_variables])
    
    # Get nearest neighbors indices
    knn_neg_indices = knn_neg.kneighbors(df_neg.iloc[:, num_variables], return_distance=False)
    knn_pos_indices = knn_pos.kneighbors(df_pos.iloc[:, num_variables], return_distance=False)
    
    # Generate synthetic samples for each class
    synthetic_neg = generate_synthetic_data(df_neg, knn_neg_indices, num_variables)
    synthetic_pos = generate_synthetic_data(df_pos, knn_pos_indices, num_variables)
    
    # Replace the original rows with the synthetic ones
    synthetic_df = df.copy()
    synthetic_df.loc[idx_neg] = synthetic_neg.values
    synthetic_df.loc[idx_pos] = synthetic_pos.values
    
    return synthetic_df



def tab_sjppds(dat, 
               num_variables, 
               cat_variables, 
               n_levels, 
               shuffle_type="simple", 
               verbose=True):
    """
    Performs Sequential Joint Probability Preserving Data Shuffling (SJPPDS) on tabular data.
    
    Handles three cases: numeric data, categorical data, or mixed numeric and categorical data.

    Parameters:
        dat (pd.DataFame): Input data matrix (numeric, categorical, or mixed).
        num_variables (list or None): Indices of numeric variables.
        cat_variables (list or None): Indices of categorical variables.
        n_levels (int): Number of levels for SJPPDS.
        shuffle_type (str): Type of shuffle ("simple" or others).
        verbose (bool): Whether to print intermediate steps.

    Returns:
        pd.DataFrame: Shuffled data matrix with the same structure as the input.
    """ 

    dat = dat.reset_index(drop=True)   
    # Numeric data case
    if num_variables is not None and cat_variables is None:
        sdat = sequential_jppds(dat=dat, 
				n_levels=n_levels, 
				shuffle_type=shuffle_type, 
				verbose=verbose)
    
    # Categorical data case
    elif num_variables is None and cat_variables is not None:
        sdat = cat_sjppds(dat=dat, 
			n_levels=n_levels, 
			shuffle_type=shuffle_type, 
			verbose=verbose)
    
    # Mixed data case
    elif num_variables is not None and cat_variables is not None:
        sdat = mixed_sjppds(dat=dat, 
                            num_variables=num_variables, 
                            cat_variables=cat_variables, 
                            n_levels=n_levels, 
                            shuffle_type=shuffle_type, 
                            verbose=verbose)
    else:
        raise ValueError("At least one of num_variables or cat_variables must be specified.")
    
    return sdat


def mixed_sjppds(dat,
                 num_variables,
                 cat_variables,
                 n_levels,
                 shuffle_type="simple",
                 verbose=True):
    """
    Performs Sequential Joint Probability Preserving Data Shuffling (SJPPDS) on mixed-type data.
    
    Parameters:
        dat (pd.DataFrame): The input dataset.
        num_variables (list): Indexes of numeric variables.
        cat_variables (list): Indexes of categorical variables.
        n_levels (int): Levels of shuffling to apply.
        shuffle_type (str): Shuffling method (e.g., 'simple').
        verbose (bool): Whether to print debug information.
    
    Returns:
        pd.DataFrame: Shuffled dataset with columns in the original order.
    """
    # Replace variable positions with variable names
    num_variable_names = dat.columns[num_variables]
    cat_variable_names = dat.columns[cat_variables]

    # Get data matrix of numeric and categorical variables
    X_n = dat[num_variable_names]
    X_c = dat[cat_variable_names]

    # Convert categorical variables to numerical encoding
    aux = categorical_to_numeric(X_c)
    R_c = aux['dat_N']
    level_numeric_ranges = aux['level_numeric_ranges']

    # Concatenate the original numeric data and the transformed categorical data
    W = pd.concat([X_n, R_c], axis=1)

    # Apply SJPPDS to the concatenated data
    W_star = sequential_jppds(W, n_levels, shuffle_type, verbose)

    # Transform shuffled numeric encodings back to categorical variables
    Y_c = numeric_to_categorical(W_star[cat_variable_names], level_numeric_ranges)

    # Concatenate numeric and categorical shuffled data
    X_s = pd.concat([W_star[num_variable_names], Y_c], axis=1)

    # Reorder columns to match the original dataset
    X_s = X_s[dat.columns]

    return X_s


def tab_sjppds_dcr(dat_train, 
                   dat_test, 
                   num_variables, 
                   cat_variables, 
                   tuning_par_grid=None,
                   scale_data = False):
    """
    Computes DCR distributions for the TabSJPPDS method over a grid of 
    n_levels tuning parameter.
    
    Parameters:
    - dat_train: Pandas DataFrame. Training data.
    - dat_test: Pandas DataFrame. Test data.
    - num_variables: List of indices for numeric variables.
    - cat_variables: List of indices for categorical variables.
    - tuning_par_grid: List of tuning parameters for the number of categories/levels.
    
    Returns:
    - DCR: Pandas DataFrame. Data matrix containing the DCR values.
    """
    # Number of tuning parameters
    num_param = len(tuning_par_grid)

    # Initialize the DataFrame with columns for test and tuning parameters
    column_names = ["test"] + tuning_par_grid
    DCR = pd.DataFrame(np.nan, index=range(len(dat_train)), columns=column_names)
    
    # Compute DCR between training and test sets
    print("Computing DCR between training and test sets")
    if cat_variables:
        dat_train_oh = one_hot_encoding(dat_train, cat_variables)
        dat_test_oh = one_hot_encoding(dat_test, cat_variables)
        if scale_data:
            scaler = StandardScaler()
            dat_train_oh = pd.DataFrame(scaler.fit_transform(dat_train_oh), columns=dat_train_oh.columns, index=dat_train_oh.index)
            dat_test_oh = pd.DataFrame(scaler.fit_transform(dat_test_oh), columns=dat_test_oh.columns, index=dat_test_oh.index)
        DCR["test"] = compute_dcr(dat_train_oh, dat_test_oh, distance_type="euclidean")
    else:
        if scale_data:
            scaler = StandardScaler()
            dat_train = pd.DataFrame(scaler.fit_transform(dat_train), columns=dat_train.columns, index=dat_train.index)
            dat_test = pd.DataFrame(scaler.fit_transform(dat_test), columns=dat_test.columns, index=dat_test.index)
        DCR["test"] = compute_dcr(dat_train, dat_test, distance_type="euclidean")

    print("Computing DCR between training and synthetic datasets")
    for i in tqdm(range(num_param), desc="Tuning parameters", ncols=100):
        tuning_param = tuning_par_grid[i]    
        # Generate synthetic data for current tuning parameter
        dat_synth = tab_sjppds(
            dat=dat_train,
            num_variables=num_variables,
            cat_variables=cat_variables,
            n_levels=tuning_param,
            shuffle_type="simple",
            verbose=False
        )

        dat_synth = enforce_dtypes(dat = dat_synth, 
                   num_variables = num_idx, 
                   cat_variables = cat_idx)
        
        # Compute DCR between training and synthetic data
        if cat_variables:
            dat_synth_oh = one_hot_encoding(dat_synth, cat_variables)
            if scale_data:
                scaler = StandardScaler()
                dat_synth_oh = pd.DataFrame(scaler.fit_transform(dat_synth_oh), columns=dat_synth_oh.columns, index=dat_synth_oh.index)
            DCR[tuning_param] = compute_dcr(dat_train_oh, dat_synth_oh, distance_type="euclidean")
        else:
            if scale_data:
                scaler = StandardScaler()
                dat_synth = pd.DataFrame(scaler.fit_transform(dat_synth), columns=dat_synth.columns, index=dat_synth.index)
            DCR[tuning_param] = compute_dcr(dat_train, dat_synth, distance_type="euclidean")
            
    return DCR

