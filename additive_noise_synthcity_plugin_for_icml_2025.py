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


class AdditiveNoisePlugin(Plugin):
    """
    Adds independent Gaussian noise to each numeric variable of a dataset. 
    Args:
        noise_percent (float): Proportion of noise (positive float).
        num_variables (list): Indexes of the columns containing numerical variables.
    """
    
    def __init__(
        self,
        noise_percent: float = 0,
        num_variables: Any = None,
        **kwargs: Any
    ) -> None:
        super().__init__(**kwargs)
        self.noise_percent = noise_percent
        self.num_variables=num_variables
    
    @staticmethod
    def name() -> str:
        return "additive_noise"

    @staticmethod
    def type() -> str:
        return "generic"

    @staticmethod
    def hyperparameter_space(*args: Any, **kwargs: Any) -> List[Distribution]:
        return []

    def _fit(self, X: DataLoader, *args: Any, **kwargs: Any) -> "AdditiveNoisePlugin":
        self.perturbedX = additive_noise_perturbation(
            dat=X.dataframe(), 
            noise_percent=self.noise_percent,
            num_variables=self.num_variables
        )
        return self

    def _generate(self, count: int, syn_schema: Schema, **kwargs: Any) -> pd.DataFrame:
        def _sample(count: int) -> pd.DataFrame:
            masked_data = self.perturbedX
            return masked_data

        return self._safe_generate(_sample, count, syn_schema)