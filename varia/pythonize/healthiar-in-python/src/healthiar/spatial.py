## Import modules
from rpy2.robjects.packages import importr

## Import R packages
sf = importr("sf")
terra = importr("terra")

## Define functions
def read_vector(path, **kwargs): ## Function to read spatial vector data for prepare_exposure()
    return sf.st_read(dsn = path, **kwargs)

def read_raster(path, **kwargs): ## Function to read spatial raster data for prepare_exposure()
    return terra.rast(x = path, **kwargs)