# stdlib
from typing import Any, List

# third party
import numpy as np
import pandas as pd
from sklearn.neighbors import NearestNeighbors

# synthcity absolute
from synthcity.plugins.core.dataloader import DataLoader, GenericDataLoader
from synthcity.plugins.core.distribution import Distribution
from synthcity.plugins.core.plugin import Plugin
from synthcity.plugins.core.schema import Schema


class SmotePlugin(Plugin):
    """
    Implements the SMOTE algorithm to generate synthetic samples for oversampling.
    (Only numerical variables at this point.)
    
    Args:
        k: Number of nearest neighbors to use
        num_variables: List of column indices containing numerical variables
        target_variable: Column index for the target variable
        task_type: "class" for classification, otherwise uses median split for regression
    """
    
    def __init__(
        self,
        k: int = 5,
        num_variables: Any = None,
        target_variable: Any = None,
        task_type: str = "class",
        **kwargs: Any
    ) -> None:
        super().__init__(**kwargs)
        self.k = k
        self.num_variables=num_variables
        self.target_variable=target_variable
        self.task_type=task_type
    
    @staticmethod
    def name() -> str:
        return "smote"

    @staticmethod
    def type() -> str:
        return "generic"

    @staticmethod
    def hyperparameter_space(*args: Any, **kwargs: Any) -> List[Distribution]:
        return []

    def _fit(self, X: DataLoader, *args: Any, **kwargs: Any) -> "SmotePlugin":
        self.synthX = synth_smote(
            df=X.dataframe(), 
            k=self.k, 
            num_variables=self.num_variables, 
            target_variable=self.target_variable, 
            task_type=self.task_type
        )
        return self

    def _generate(self, count: int, syn_schema: Schema, **kwargs: Any) -> pd.DataFrame:
        def _sample(count: int) -> pd.DataFrame:
            masked_data = self.synthX
            return masked_data

        return self._safe_generate(_sample, count, syn_schema)