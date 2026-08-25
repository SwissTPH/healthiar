## Load OS module
import os

## Add environment variable for R installation
#os.environ['R_HOME'] = r"C:\Users\ArPa3547\AppData\Local\Programs\R\R-4.4.0"
os.environ["PATH"] = r"C:\Users\ArPa3547\AppData\Local\Programs\R\R-4.6.0\bin\x64"

## Import healthiar modules
import healthiar
from healthiar.healthiar import healthiar
from healthiar.conversion import py_to_r, r_to_py

## Import rpy2 modules
import rpy2.rinterface as ri
from rpy2.robjects.packages import importr

## Import other modules
import pandas as pd
import numpy as np
from scipy import interpolate
import geopandas as gpd
import xarray as xr
import rioxarray

## Import R packages
sf = importr("sf")
terra = importr("terra")

## Set path to data
from importlib.resources import files
path = files("healthiar.data")

import pathlib

## Test different inputs
def test_tuple_input():
    ## Define expected result
    expected = 0.936215963

    # Get Python data
    exp_central = (20, 20)
    prop_pop_exp = (0.5, 0.5)

    # Convert to R data
    r_exp_central = py_to_r(exp_central)
    r_prop_pop_exp = py_to_r(prop_pop_exp)
    r_cutoff_central = py_to_r(5)
    r_rr_central = py_to_r(1.08)
    r_rr_increment = py_to_r(10)
    r_erf_shape = py_to_r("log_log")
    r_bhd_central = py_to_r(10)

    # Call healthiar function
    result = healthiar.attribute_health(
        exp_central = r_exp_central,
        prop_pop_exp = r_prop_pop_exp,
        cutoff_central = r_cutoff_central,
        rr_central = r_rr_central,
        rr_increment = r_rr_increment,
        erf_shape = r_erf_shape,
        bhd_central = r_bhd_central
    )
    py_result = r_to_py(result)["health_main"]["impact"].iloc[0]
    actual = round(py_result, 9)

    ## Verify actual result
    assert actual == expected, "Test failed."

def test_list_input():
    ## Define expected result
    expected = np.float64(0.927071)

    # Get Python data
    exp_central = [20, 20]
    prop_pop_exp = [0.5, 0.5]
    bhd_central = [10]

    # Convert to R data
    r_exp_central = py_to_r(exp_central)
    r_prop_pop_exp = py_to_r(prop_pop_exp)
    r_cutoff_central = py_to_r(5)
    r_rr_central = py_to_r(1.08)
    r_rr_increment = py_to_r(10)
    r_erf_shape = py_to_r("linear_log")
    r_bhd_central = py_to_r(bhd_central)

    # Call healthiar function
    result = healthiar.attribute_health(
        exp_central = r_exp_central,
        prop_pop_exp = r_prop_pop_exp,
        cutoff_central = r_cutoff_central,
        rr_central = r_rr_central,
        rr_increment = r_rr_increment,
        erf_shape = r_erf_shape,
        bhd_central = r_bhd_central
    )
    py_result = r_to_py(result)['health_main']['impact'].iloc[0]
    actual = round(py_result, 6)

    ## Verify actual result
    assert actual == expected, "Test failed."

def test_list_input2():
    ## Define expected result
    expected = [round(x, 11) for x in [1116.41855325132, 466.433010062623, 134.881901125568]]

    # Get Python data
    geo_id_micro = ["Zürich", "Basel", "Geneva", "Ticino", "Jura"]
    geo_id_macro = ["German","German","French","Italian","French"]
    exp_central = [11, 11, 10, 8, 7]
    bhd_central = [4000, 2500, 3000, 1500, 500]

    # Convert to R data
    r_geo_id_micro = py_to_r(geo_id_micro)
    r_geo_id_macro = py_to_r(geo_id_macro)
    r_erf_shape = py_to_r("log_linear")
    r_rr_central = py_to_r(1.369)
    r_rr_increment = py_to_r(10)
    r_cutoff_central = py_to_r(5)
    r_exp_central = py_to_r(exp_central)
    r_bhd_central = py_to_r(bhd_central)

    # Call healthiar function
    result = healthiar.attribute_health(
        geo_id_micro = r_geo_id_micro,
        geo_id_macro = r_geo_id_macro,
        erf_shape = r_erf_shape,
        rr_central = r_rr_central,
        rr_increment = r_rr_increment,
        cutoff_central = r_cutoff_central,
        exp_central = r_exp_central,
        bhd_central = r_bhd_central
    )
    py_result = r_to_py(result)["health_main"]["impact"]
    actual = [round(x, 11) for x in py_result]

    ## Verify actual result
    assert actual == expected, "Test failed."

def test_pandas_input():
    ## Define expected result
    expected = 3502

    # Get Python data
    data = pd.DataFrame({
        "mean_concentration": [8.85],
        "cut_off_value": [5],
        "incidents_per_100_000_per_year": [357.27],
        "population_at_risk": [8606096],
        "relative_risk": [1.369],
        "pollutant": ["PM2.5"],
        "evaluation_name": ["GeLuft_COPD"],
        "estimated_number_of_attributable_cases_central": [3502]
    })    

    # Convert to R data
    r_approach_risk = py_to_r("relative_risk")
    r_exp_central = py_to_r(data["mean_concentration"])
    r_cutoff_central = py_to_r(data["cut_off_value"])
    r_bhd_central = py_to_r(data["incidents_per_100_000_per_year"] / 10**5 * data["population_at_risk"])
    r_rr_central = py_to_r(data["relative_risk"])
    r_rr_increment = py_to_r(10)
    r_erf_shape = py_to_r("log_linear")

    # Call healthiar function
    result = healthiar.attribute_health(
        approach_risk = r_approach_risk,
        exp_central = r_exp_central,
        cutoff_central = r_cutoff_central,
        bhd_central = r_bhd_central,
        rr_central = r_rr_central,
        rr_increment = r_rr_increment,
        erf_shape = r_erf_shape
    )
    actual = r_to_py(result)["health_main"]["impact_rounded"].iloc[0] 

    ## Verify actual result
    assert actual == expected, "Test failed."
    
def test_numpy_input():
    ## Define expected result
    expected = np.float64(0.927071)

    # Get Python data
    exp_central = np.array([20, 20])
    prop_pop_exp = np.array([0.5, 0.5])
    bhd_central = np.array([10])

    # Convert to R data
    r_exp_central = py_to_r(exp_central)
    r_prop_pop_exp = py_to_r(prop_pop_exp)
    r_cutoff_central = py_to_r(5)
    r_rr_central = py_to_r(1.08)
    r_rr_increment = py_to_r(10)
    r_erf_shape = py_to_r("linear_log")
    r_bhd_central = py_to_r(bhd_central)

    # Call healthiar function
    result = healthiar.attribute_health(
        exp_central = r_exp_central,
        prop_pop_exp = r_prop_pop_exp,
        cutoff_central = r_cutoff_central,
        rr_central = r_rr_central,
        rr_increment = r_rr_increment,
        erf_shape = r_erf_shape,
        bhd_central = r_bhd_central
    )
    py_result = r_to_py(result)['health_main']['impact'].iloc[0]
    actual = round(py_result, 6)

    ## Verify actual result
    assert actual == expected, "Test failed."

def test_function_input():
    ## Define expected result
    expected = [350, 267, 424, 313, 238, 379]
    
    ## Get Python data
    data = pd.read_csv(path.joinpath("LMU_O3_COPD_mort_2015_2016.csv"), skiprows = [1])

    ## Convert to R data
    @ri.rternalize
    def r_erf_eq_central(x):
        cs = interpolate.CubicSpline(data["x"][0:20], data["y"][0:20])
        return float(cs(x)[0])

    @ri.rternalize
    def r_erf_eq_lower(x):
        cs = interpolate.CubicSpline(data["x"][0:20], data["y_l"][0:20])
        return float(cs(x)[0])

    @ri.rternalize
    def r_erf_eq_upper(x):
        cs = interpolate.CubicSpline(data["x"][0:20], data["y_u"][0:20])
        return float(cs(x)[0])

    r_prop_pop_exp = py_to_r(data["Population.affected"])
    r_exp_central = py_to_r(data["Mean.O3"])
    r_cutoff_central = py_to_r(0)
    r_bhd_central =  py_to_r(data["bhd"])
    r_geo_id_micro = py_to_r(data["X"])

    ## Call healthiar function
    result = healthiar.attribute_health(
        erf_eq_central = r_erf_eq_central,
        erf_eq_lower = r_erf_eq_lower,
        erf_eq_upper = r_erf_eq_upper,
        prop_pop_exp = r_prop_pop_exp,
        exp_central = r_exp_central, # exposure distribution for ozone
        cutoff_central = r_cutoff_central,
        bhd_central =  r_bhd_central, #COPD mortality in Germany 2015 and 2016
        geo_id_micro = r_geo_id_micro
    )
    actual = r_to_py(result)['health_main']['impact_rounded']

    ## Verify actual result
    assert all(actual == expected), "Test failed."

def test_function_input2():
    ## Define expected result
    expected = 14136

    ## Get Python data
    data = pd.read_csv(path.joinpath("roadnoise_ha_Lden_StavangerandVicinity.csv"))
    info = pd.DataFrame({
        "pollutant": ["road_noise"],
        "outcome": ["highly_annoyance"]
    })

    ## Convert to R data
    r_approach_risk = py_to_r("absolute_risk")
    r_exp_central = py_to_r(data['average_cat'])
    r_population  = py_to_r(int(data['totpop'][0]))
    r_pop_exp = py_to_r(data['ANTALL_PER'])
    r_erf_eq_central = py_to_r("78.9270-3.1162*c+0.0342*c^2")
    r_info = py_to_r(info)

    ## Call healthiar function
    result = healthiar.attribute_health(
        approach_risk = r_approach_risk,
        exp_central = r_exp_central,
        population  = r_population,
        pop_exp = r_pop_exp,
        erf_eq_central = r_erf_eq_central,
        info = r_info
    )
    actual = r_to_py(result)["health_main"]["impact_rounded"].iloc[0]

    ## Verify actual result
    assert actual == expected, "Test failed."

def test_spatial_input():
    ## Define expected result
    expected = pd.read_csv(path.joinpath("exp_grid_results.csv"))
    expected = list(expected["exposure"])

    ## Get python data
    poll_grid = xr.open_dataset(path.joinpath("pm25.tif"), engine = "rasterio", masked = True)
    pop_grid = xr.open_dataset(path.joinpath("population.tif"), engine = "rasterio", masked = True)
    geo_units = gpd.read_file(path.joinpath("municipalities_brussels.gpkg"))

    ## Convert to R data
    r_poll_grid = py_to_r(poll_grid)
    r_pop_grid = py_to_r(pop_grid)
    r_geo_units = py_to_r(geo_units)
    r_geo_id_micro = py_to_r(geo_units["name"])

    ## Call healthiar function
    result = healthiar.prepare_exposure(
        poll_grid = r_poll_grid,
        geo_units = r_geo_units,
        pop_grid = r_pop_grid,
        geo_id_micro = r_geo_id_micro
    )
    py_result = r_to_py(result)["exposure_main"]["exposure_mean"]
    actual = [round(x, 13) for x in py_result]
    print(actual)
    print(expected)

    ## Verify actual result
    assert actual == expected, "Test failed."

def test_output_reuse():
    ## Define expected result
    expected = [774, 409, 1127]

    ## Get Python data

    ## Convert to R data
    
    ## Call healthiar function
    output_attribute_scen_1 = healthiar.attribute_health(
        exp_central = 8.85,
        cutoff_central = 5,
        bhd_central = 25000,
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
        rr_increment = 10,
        info = "PM2.5_mortality_2010"
    )

    output_attribute_scen_2 = healthiar.attribute_health(
        exp_central = 6,
        cutoff_central = 5,
        bhd_central = 25000,
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
        rr_increment = 10,
        info = "PM2.5_mortality_2020"
    )

    result = healthiar.compare(
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "delta"
    )
    actual = r_to_py(result)["health_main"]["impact_rounded"]
    
    ## Verify actual result
    assert all(actual == expected), "Test failed."
