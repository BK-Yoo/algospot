import pandas as pd
import numpy as np
import scipy.stats as st

class DistributionFitting:

    # param investment_size list. The sizes of each investment received.
    # returns string. The name('norm', 'cauchy' or 'expon') of the distribution that best fits given data according to Akaike information criterion (AIC).
    @staticmethod
    def best_fit(investment_size):
        return None

#For example, with the parameters below the function should return 'norm'.
investment_size = [
    11624, 9388, 9471, 8927,
    10865, 7698, 11744, 9238,
    10319, 9750, 11462, 7939
]

print(DistributionFitting.best_fit(investment_size))