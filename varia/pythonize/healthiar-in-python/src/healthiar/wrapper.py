## Import modules
from rpy2.robjects.packages import importr
import rpy2.rinterface as ri

## Initialise R session in background
ri.initr()

## Import healthiar
healthiar = importr("healthiar")

