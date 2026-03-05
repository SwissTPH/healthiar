import os

# Point Python/rpy2 to the Folder wher your R is located  --> healthiar has to be already installed there
os.environ['R_HOME'] = r"C:\Program Files\R\R-4.4.1"

import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.robjects import FloatVector, IntVector,globalenv
from rpy2.robjects import r
from rpy2.robjects import pandas2ri
import pandas as pd


healhipy = importr("healthiar")



# Python-style inputs
exp_central = [20, 20]
prop_pop_exp = [0.5, 0.5]
bhd_central = [10]

# Convert to R vectors
r_exp_central = FloatVector(exp_central)
r_prop_pop_exp = FloatVector(prop_pop_exp)
r_bhd_central = FloatVector(bhd_central)

# Call healthiar function
result = healhipy.attribute_health(
    exp_central=r_exp_central,
    prop_pop_exp=r_prop_pop_exp,
    cutoff_central=5,
    rr_central=1.08,
    rr_increment=10,
    erf_shape="linear_log",
    bhd_central=r_bhd_central
)
print(result.rx2('health_main').rx2('impact'))


result.rx2('health_main')


# print(r('args(healthiar::attribute_health)'))
# print(r('help("attribute_health", package="healthiar")'))
help(healhipy.attribute_health)