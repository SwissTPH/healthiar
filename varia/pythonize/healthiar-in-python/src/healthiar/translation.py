## Import modules
import rpy2.robjects as ro
import pandas as pd
import numpy as np

## Define functions
def py_to_r(obj): ## Converts Python data to R (rpy2) data 
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
        with (ro.default_converter + ro.pandas2ri.converter).context():
            return ro.conversion.get_conversion().py2rpy(obj)
    elif isinstance(obj, np.ndarray):
        with (ro.default_converter + ro.numpy2ri.converter).context():
            return ro.conversion.get_conversion().py2rpy(obj)
    else:
        raise ValueError("Conversion not possible for this object type.")

## Function to convert rpy2 lists and data.frames to python lists and pandas dataframes
def r_to_py(obj): ## Converts R (rpy2) data to Python data
    if isinstance(obj, ro.DataFrame):
        i = ro.BoolVector([name == "erf_eq" for name in obj.names])
        obj.rx[i] = str(obj.rx(i)) ## prevents error related to Python function input
        
        with ro.conversion.localconverter(ro.default_converter + ro.pandas2ri.converter):
            return ro.conversion.rpy2py(obj)
               
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