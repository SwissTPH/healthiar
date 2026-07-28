## Import modules
from rpy2.robjects.packages import importr

## Import R packages
sf = importr("sf")
terra = importr("terra")

## Define functions
def read_vector(path, **kwargs):
    """Read spatial vector data for prepare_exposure().
    
    Based on the st_read() function from the sf R package.

    Parameters
    ----------
    path : str
        Path to vector file.
    **kwargs
        Any additional arguments to be passed to st_read().

    Returns
    -------
    sf
        rpy2 object representing sf object.
    
    Examples
    >>> read_vector("exdat_pwm_2_municipalities_brussels.gpkg")
    Reading layer `municipalities_brussels' from data source
    `municipalities_brussels.gpkg'
    using driver `GPKG'
    Simple feature collection with 19 features and 4 fields
    ...
    """
    return sf.st_read(dsn = path, **kwargs)

def read_raster(path, **kwargs): ## Function to read spatial raster data for prepare_exposure()
    """Read spatial raster data for prepare_exposure(). 
    
    Based on the rast() function from the terra R package.

    Parameters
    ----------
    path : str
        Path to raster file.
    **kwargs
        Any additional arguments to be passed to rast().

    Returns
    -------
    SpatRaster
        rpy2 object representing SpatRaster object.
    
    Examples
    >>> read_raster("exdata_pwm_1_pm25.tif")
    <rpy2.robjects.methods.RS4 object at 0x00000169D97716D0> [25]
    R classes: ('SpatRaster',)
    """
    return terra.rast(x = path, **kwargs)