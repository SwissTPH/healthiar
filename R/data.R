# exdat_pm #####################################################################

#' PM2.5 exposure and COPD incidence in Switzerland

#' @description
#' This tibble contains PM2.5 exposure and COPD incidence data from Switzerland.

#' @format \code{exdat_pm}
#' \describe{
#'   \item{mean_concentration}{population-weighted annual mean concentration}
#'   \item{relative_risk}{central relative risk estimate}
#'   \item{relative_risk_lower}{lower 95\% confidence interval bound of the relative risk estimate}
#'   \item{relative_risk_upper}{upper 95\% confidence interval bound of the relative risk estimate}
#'   \itme{incidence}{COPD incidence in the year of analysis}
#'   \item{cut_off_value}{cut-off value}
#'   \item{rr_increment}{exposure increment in \eqn{µg/m^3} for which the relative risk estimates are valid}
#' }
#' @source Real-world data

#' @usage data(exdat_pm)

#' @docType data

#' @author Alberto Castro & Axel Luyten

#' @examples
#' #TBD
"exdat_pm"

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

#' @usage data(exdat_noise)

#' @docType data

#' @author Anette Kocbach Bolling & Vázquez Fernández

#' @examples
#' #TBD
"exdat_noise"

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

#' @usage data(exdat_lifetable)

#' @docType data

#' @author Alberto Castro & Axel Luyten

#' @examples
#' #TBD
"exdat_lifetable"

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

#' @usage data(exdat_prepare_mdi)

#' @docType data

#' @author Arno Pauwels & Vanessa Gorasso

#' @examples
#' #TBD
"exdat_prepare_mdi"

# exdat_socialize ##############################################################

#' Municipalities in Belgium ranked by BEST-COST Multidimensional Deprivation Index (MDI)

#' @description
#' This tibble contains data for municipalities in Belgium ranked by BEST-COST Multidimensional Deprivation Index (MDI).

#' @format \code{exdat_socialize}

#' @source Real-world data

#' @usage data(exdat_socialize)

#' @docType data

#' @author Arno Pauwels & Vanessa Gorasso

#' @examples
#' #TBD
"exdat_socialize"

# exdat_cantons ################################################################

#' PM2.5 exposure and COPD incidence in Switzerland

#' @description
#' This tibble contains PM2.5 exposure and lung cancer incidence data from the Swiss cantons.

#' @format \code{exdat_cantons}
#' \describe{
#'   \item{year}{population-weighted annual mean concentration}
#'   \item{canton}{canton abbreviations}
#'   \item{lung_cancer_incidence}{lung cancer incidence in the year of analysis}
#'   \item{exposure}{exposure level}
#'   \itme{pollutant}{pollutant}
#'   \item{exposure_type}{exposure type}
#'   \item{population}{cantonal population}
#'   \item{rr}{central relative risk estimate}
#'   \item{rr_l}{lower 95\% confidence interval bound of the relative risk estimate}
#'   \item{rr_u}{upper 95\% confidence interval bound of the relative risk estimate}
#'   \item{increment}{exposure increment in \eqn{µg/m^3} for which the relative risk estimates are valid}
#'   \item{function_shape}{shape of the exposure-response function}
#'   \item{cutoff}{cut-off value}
#' }
#' @source Real-world data

#' @usage data(exdat_cantons)

#' @docType data

#' @author Alberto Castro & Axel Luyten

#' @examples
#' #TBD
"exdat_cantons"
