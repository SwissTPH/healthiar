# exdat_cantons #####################################################################

#' PM2.5 exposure and COPD incidence in Switzerland

#' @description
#' This tibble contains PM2.5 exposure and COPD incidence data from Switzerland.

#' @format \code{exdat_cantons}
#' \describe{
#'   \item{year}{year}
#'   \item{canton}{abbreviation of Swiss cantons}
#'   \item{lung_cancer_incidence}{lung cancer incidence}
#'   \item{exposure}{mean country-wide population-weighted exposure level}
#'   \item{pollutant}{PM2.5}
#'   \item{exposure_type}{exposure type}
#'   \item{population}{number of inhabitants per canton}
#'   \item{rr}{central relative risk estimate}
#'   \item{rr_l}{lower 95\% confidence interval bound of the relative risk estimate}
#'   \item{rr_u}{upper 95\% confidence interval bound of the relative risk estimate}
#'   \item{increment}{exposure increment in \eqn{\mu g/m^3} for which the relative risk estimates are valid}
#'   \item{function_shape}{shape of the exposure-response function}
#'   \item{cutoff}{cutoff level below which no health effects are attributable to the exposure}
#'   \item{language_main}{language spoken by the majority of inhabitants in the canton}
#'   \item{canton_long}{full (English) name of the canton}
#' }
#' @source Real-world data

#' @usage base::data(exdat_cantons)

#' @docType data

#' @author Alberto Castro & Axel Luyten

#' @keywords internal

"exdat_cantons"

# exdat_lifetable ##############################################################

#' Population data per age and sex in Switzerland

#' @description
#' This tibble contains population per age and sex for Switzerland.

#' @format \code{exdat_lifetable}
#' \describe{
#'   \item{age_group}{single year age groups}
#'   \item{sex}{female or male}
#'   \item{midyear_population}{mid-year populations}
#'   \item{deaths}{annual deaths}
#' }
#' @source Real-world data

#' @usage base::data(exdat_lifetable)

#' @docType data

#' @author Alberto Castro & Axel Luyten

#' @keywords internal

"exdat_lifetable"

# exdat_noise ##################################################################

#' Noise exposure in urban and rural regions in Norway

#' @description
#' This tibble contains noise exposure data from urban and rural regions in Norway.

#' @format \code{exdat_noise}
#' \describe{
#'   \item{exposure_category}{noise exposure range of the exposure category}
#'   \item{exposure_mean}{mean noise exposure in the exposure category}
#'   \item{region}{region for which exposure is valid}
#'   \item{exposed}{number of exposed persons}
#' }
#' @source Real-world data

#' @usage base::data(exdat_noise)

#' @docType data

#' @author Anette Kocbach Bolling & Vázquez Fernández

#' @keywords internal

"exdat_noise"

# exdat_ozone ################################################################

#' PM2.5 exposure and COPD incidence in Switzerland

#' @description
#' This tibble contains modelled ozone (\eqn{O_3}) exposure and chronic obstructive pulmonary disease (COPD) incidence data from the Germany in 2016.

#' @format \code{exdat_ozone}
#' \describe{
#'   \item{pollutant}{\eqn{O_3}}
#'   \item{exposure}{mean exposure level in the exposure category}
#'   \item{exp_unit}{unit of the exposure}
#'   \item{proportion_population_exposed}{proportion of the total population exposed to each exposure category}
#'   \item{mortality_copd_tota_yearl}{mortality due to chronic obstructive pulmonary disease (ICD-10 J40-44)}
#'   \item{rr_central}{central relative risk estimate}
#'   \item{rr_lower}{lower 95\% confidence interval bound of the relative risk estimate}
#'   \item{rr_upper}{upper 95\% confidence interval bound of the relative risk estimate}
#'   \item{rr_increment}{exposure increment in \eqn{\mu g/m^3} for which the relative risk estimates are valid}
#'   \item{cutoff}{cutoff level below which no health effects are attributable to the exposure}
#'   \item{erf_shape}{shape of the exposure-response function}
#'   \item{exposure_type}{exposure type}
#'   \item{rr_source}{source of the relative risk estimates}
#'   \item{country}{country}
#'   \item{year}{year of the data}
#' }

#' @source Real-world data

#' @usage base::data(exdat_ozone)

#' @docType data

#' @author Alberto Castro & Axel Luyten

#' @keywords internal

"exdat_ozone"

# exdat_pm #####################################################################

#' PM2.5 exposure and COPD incidence in Switzerland

#' @description
#' This tibble contains PM2.5 exposure and COPD incidence data from Switzerland.

#' @format \code{exdat_pm}
#' \describe{
#'   \item{year_of_analysis}{year that the exposure and incidence data is from}
#'   \item{mean_concentration}{population-weighted annual mean concentration}
#'   \item{relative_risk}{central relative risk estimate}
#'   \item{relative_risk_lower}{lower 95\% confidence interval bound of the relative risk estimate}
#'   \item{relative_risk_upper}{upper 95\% confidence interval bound of the relative risk estimate}
#'   \item{erf_shape}{shape of the exposure-response function}
#'   \item{incidence}{COPD incidence in the year of analysis}
#'   \item{cutoff_value}{cut-off value}
#'   \item{rr_increment}{exposure increment in \eqn{\mu g/m^3} for which the relative risk estimates are valid}
#'   \item{rr_source}{source of the relative risk}
#'   \item{rr_doi}{DOI linking to the publication from which the relative risk was taken}
#' }
#' @source Real-world data

#' @usage base::data(exdat_pm)

#' @docType data

#' @author Alberto Castro & Axel Luyten

#' @keywords internal

"exdat_pm"

# exdat_prepare_mdi ##############################################################

#' Social indicators of the BEST-COST Multidimensional Deprivation Index (MDI)

#' @description
#' This tibble contains social indicators of the BEST-COST Multidimensional Deprivation Index (MDI) of geo units in Belgium.

#' @format \code{exdat_prepare_mdi}
#' \describe{
#'   \item{id}{id of the geographic unit}
#'   \item{geo_name}{name of the geographic unit}
#'   \item{edu, unemployed, single_parent, no_heating, pop_change}{single social indicators that make up the MDI}
#'   \item{norm_...}{normalized single social indicators of the MDI}
#'   \item{MDI}{BEST-COST Multidimensional Deprivation Index (MDI)}
#'   \item{MDI_decile}{decile of the MDI rankig}
#'   \item{MDI_quartile}{quartile of the MDI ranking}
#' }

#' @source Real-world data

#' @usage base::data(exdat_prepare_mdi)

#' @docType data

#' @author Arno Pauwels & Vanessa Gorasso

#' @keywords internal

"exdat_prepare_mdi"

# exdat_pwm_1 ##################################################################

#' Air pollution expsoure data of the Brussels-Capital region (Belgium)

#' @description
#' The data can be loaded using
#'
#' \code{exdat_pwm_1 <- terra::rast(}
#'
#' \code{system.file("extdata", "exdat_pwm_1.tif", package = "healthiar")}
#'
#' \code{)}
#'
#' (see Examples section below).
#'
#' When loaded it is a variable of class \code{SpatRaster}, which contains air pollution exposure levels
#' of municipalities in the Brussels-Capital region (Belgium).
#'
#' Because it is a \code{.tif} file it is stored in the package's \code{inst/extdata} directory.

#' @docType data

#' @format GeoTIFF raster

#' @source Real-world data

#' @name exdat_pwm_1

#' @examples
#' path <- system.file("extdata", "exdat_pwm_1.tif", package = "healthiar")
#' exdat_pwm_1 <- terra::rast(path)

#' @author Arno Pauwels

#' @keywords internal

NULL

# exdat_pwm_2 ##################################################################

#' Geospatial outlines and populations of the Brussels-Capital region (Belgium)

#' @description
#' This variable of class \code{sf} and \code{data.frame} contains the geospatial outlines and populations of
#' municipalities in the Brussels-Capital region (Belgium).

#' @docType data

#' @format \code{exdat_pwm_2}
#' \describe{
#'   \item{code}{NIS-codes (i.e. unique statistical code assigned to each geographic area in Belgium)}
#'   \item{name}{Dutch names of the municipalities}
#'   \item{population}{municipality populations}
#'   \item{region}{geographic position of each municipality (North, East, West, South) relative to Brussels (Center)}
#'   \item{geom}{geospatial outlines of the municipalities (MULTIPOLYGON)}
#' }

#' @source Real-world data

#' @usage base::data(exdat_pwm_2)

#' @docType data

#' @author Arno Pauwels

#' @keywords internal

"exdat_pwm_2"

# exdat_socialize ##############################################################

#' Municipalities in Belgium ranked by BEST-COST Multidimensional Deprivation Index (MDI)

#' @description
#' This tibble contains data for municipalities in Belgium ranked by BEST-COST Multidimensional Deprivation Index (MDI).

#' @format \code{exdat_socialize}
#' \describe{
#'   \item{CS01012020}{unique identifier of the geographic unit}
#'   \item{NUTS1}{NUTS1 region tag}
#'   \item{PM25_MEAN}{mean PM2.5 exposure}
#'   \item{RR}{relative risk estimate from the literature}
#'   \item{score}{BEST-COST Multidimensional Deprivation Index (MDI)}
#'   \item{rank}{rank of the observation based on column \emph{score}; note that the rank is not continuous, as some observations are missing}
#'   \item{deciles}{deciles of the geo units based on the MDI}
#'   \item{POPULATION_below_40}{(fake) populations up until and including 39 years of age}
#'   \item{POPULATION_40_plus}{(fake) populations from 40 years of age onwards}
#'   \item{MORTALITY_below_40}{(fake) mortality up until and including 39 years of age}
#'   \item{MORTALITY_40_plus}{(fake) mortality from 40 years of age onwards}
#' }

#' @source Real-world data combined with fake population and mortality data

#' @usage base::data(exdat_socialize)

#' @docType data

#' @author Arno Pauwels & Vanessa Gorasso

#' @keywords internal

"exdat_socialize"
