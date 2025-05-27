# stdlib
from typing import Any, List

# third party
import numpy as np
import pandas as pd

# synthcity absolute
from synthcity.plugins.core.dataloader import DataLoader, GenericDataLoader
from synthcity.plugins.core.distribution import Distribution
from synthcity.plugins.core.plugin import Plugin
from synthcity.plugins.core.schema import Schema


class SynTabSjppdsPlugin(Plugin):
    """
    Synthetic Sequential Joint Probability Preserving Data Shuffling
    Args:
        n_levels: int
            Number of levels/categories
        shuffle_type: str
            "simple" for simplified SJPPDS, "full" for full SJPPDS approach (default is "simple").
        verbose: bool
            Whether to print the computation progress (default is False).
        n_prop: float
            Proportion for IOSSampling.
    """
    
    def __init__(
        self,
        n_levels: int = 30,
        num_variables: Any = None,
        cat_variables: Any = None,
        shuffle_type: str = "simple",
        verbose: bool = False,
        n_prop: float = 0.5,
        **kwargs: Any
    ) -> None:
        super().__init__(**kwargs)
        self.n_levels = n_levels
        self.num_variables=num_variables
        self.cat_variables=cat_variables
        self.shuffle_type = shuffle_type
        self.verbose = verbose
        self.n_prop = n_prop
    
    @staticmethod
    def name() -> str:
        return "syn_tab_sjppds"

    @staticmethod
    def type() -> str:
        return "generic"

    @staticmethod
    def hyperparameter_space(*args: Any, **kwargs: Any) -> List[Distribution]:
        return []

    def _fit(self, X: DataLoader, *args: Any, **kwargs: Any) -> "SynTabSjppdsPlugin":
        self.shuffledX = synthetic_tab_sjppds(
            dat=X.dataframe(), 
            num_variables=self.num_variables, 
            cat_variables=self.cat_variables,
            n_levels=self.n_levels, 
            shuffle_type=self.shuffle_type,
            verbose=self.verbose,
            n_prop=self.n_prop
        )
        return self

    def _generate(self, count: int, syn_schema: Schema, **kwargs: Any) -> pd.DataFrame:
        def _sample(count: int) -> pd.DataFrame:
            masked_data = self.shuffledX
            return masked_data

        return self._safe_generate(_sample, count, syn_schema)