## Import modules
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
from rpy2.robjects import numpy2ri
from rpy2.robjects.packages import importr
import os
import pathlib
import tempfile
import pandas as pd
import numpy as np
import geopandas as gpd
import xarray as xr
import rioxarray

## Import R packages
sf = importr("sf")
terra = importr("terra")

## Define functions
def py_to_r(obj):
    """Convert Python object to R (rpy2) object.

    Parameters
    ----------
    obj : tuple, list, pandas.DataFrame, pandas.Series, numpy.ndarray
        Python object to convert.
    
    Returns
    -------
    IntVector, FloatVector, StrVector, DataFrame
        rpy2 object resulting from conversion.

    Examples
    --------
    >>> py_to_r([1, 2, 3])
    <rpy2.robjects.vectors.IntVector object at 0x00000169ED71AA50> [13]
    R classes: ('integer',)
    [1, 2, 3]
    """
    if isinstance(obj, (int, float, str)):
        return obj
    elif isinstance(obj, (tuple, list, pd.Series)):
        if all(isinstance(item, int) for item in obj):
            return ro.IntVector(obj)
        elif all(isinstance(item, float) for item in obj):
            return ro.FloatVector(obj)
        elif all(isinstance(item, str) for item in obj):
            return ro.StrVector(obj)
        else:
            raise ValueError("Conversion not possible for tuple/list with these element types.")
    elif isinstance(obj, pd.DataFrame):
        if isinstance(obj, gpd.geodataframe.GeoDataFrame):
            tmp_dir = pathlib.Path(tempfile.mkdtemp())
            obj.to_file(tmp_dir.joinpath("temp_conv_vector.gpkg"), layer = "1", driver = "GPKG")
            obj = sf.st_read(tmp_dir.joinpath("temp_conv_vector.gpkg").as_posix(), quiet = True)
            return obj
        else:
            with (ro.default_converter + pandas2ri.converter).context():
                return ro.conversion.get_conversion().py2rpy(obj)
    elif isinstance(obj, np.ndarray):
        with (ro.default_converter + numpy2ri.converter).context():
            return ro.conversion.get_conversion().py2rpy(obj)
    elif isinstance(obj, xr.core.dataset.Dataset):
        tmp_dir = pathlib.Path(tempfile.mkdtemp())
        obj.squeeze().rio.to_raster(tmp_dir.joinpath(".temp_conv_raster.tiff"))
        obj = terra.rast(tmp_dir.joinpath(".temp_conv_raster.tiff").as_posix())
        return obj
    else:
        raise ValueError("Conversion not possible for this object type.")

## Function to convert rpy2 lists and data.frames to python lists and pandas dataframes
def r_to_py(obj):
    """Convert R (rpy2) object to Python object.

    Parameters
    ----------
    obj : IntVector, FloatVector, StrVector, DataFrame
        rpy2 object to convert.
    
    Returns
    -------
    int, float, str, tuple, list, pandas.DataFrame
        Python object resulting from conversion.

    Examples
    --------
    >>> from rpy2.robjects import IntVector
    >>> R_vector = IntVector([1, 2, 3])
    >>> r_to_py(R_vector)
    [1, 2, 3]
    """
    if isinstance(obj, ro.DataFrame):
        i = ro.BoolVector([name == "erf_eq" for name in obj.names])
        obj.rx[i] = str(obj.rx(i)) ## prevents error related to Python function input
        
        with ro.conversion.localconverter(ro.default_converter + pandas2ri.converter):
            #return ro.conversion.rpy2py(obj)
            return ro.conversion.get_conversion().rpy2py(obj)
               
    elif isinstance(obj, ro.ListVector):
        if isinstance(obj.names, ro.StrVector):
            return {name: r_to_py(obj.rx2(name)) for name in obj.names}         
        else:
            return obj
    
    elif isinstance(obj, (ro.IntVector, ro.FloatVector, ro.StrVector, ro.BoolVector)):
        if len(obj) == 1:
            return obj[0]  # single elements
        else:
            return list(obj)  # multiple elements
    
    #elif isinstance(obj, SignatureTranslatedFunction):
    #    return str(obj)
    else:
        return obj  # fallback for other types