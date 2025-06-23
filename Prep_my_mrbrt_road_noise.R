### My MRBRT for road noise


data <- read.csv(file = "./tests_data/roadnoise_HA_Lden_Stavanger_Bergen_.csv", header=T, sep=",")
setDT(data)

# We create the AR from GUSKI
data[, AR := 78.9270 - 3.1162 * average_cat + 0.0342 * average_cat^2]


# Parameters according to chatGPT

baseline_AR <- 0.05      # baseline absolute risk at cutoff dB (adjust if needed)
cutoff <- 50             # baseline noise level in dB
rr_central_10db <- 1.05  # RR per 10 dB
rr_lower_10db <- 1.03
rr_upper_10db <- 1.07

# Exposure range: 30 to 85 dB every 1 dB
dB <- 30:85

# Calculate RR and bounds relative to cutoff
RR_central <- rr_central_10db ^ ((dB - cutoff) / 10)
RR_lower <- rr_lower_10db ^ ((dB - cutoff) / 10)
RR_upper <- rr_upper_10db ^ ((dB - cutoff) / 10)

# Calculate AR and bounds
AR_central <- baseline_AR * RR_central
AR_lower <- baseline_AR * RR_lower
AR_upper <- baseline_AR * RR_upper



# Create data table with all
erf_df <- data.table(
  dB = dB,
  RR = RR_central,
  RR_lower = RR_lower,
  RR_upper = RR_upper,
  AR = AR_central,
  AR_lower = AR_lower,
  AR_upper = AR_upper
)

erf_df

# The lines cross at 50 dB, doesnt make sense

# Change the AR here
erf_df[ , AR:= 78.9270 - 3.1162 * dB + 0.0342 * dB^2]



healthiar::attribute_health(
  approach_risk = "absolute_risk",
  exp_central = data$average_cat,
  # absolute_risk_as_percent = data$AR,
  pop_exp     = data$ANTALL_PER,
  prop_pop_exp = data$prop_pop_exp,
  population  = data$totpop,
  geo_id_disaggregated = data$GEO_ID,
  geo_id_aggregated = "Norway", #,
  erf_eq_central =  approx(erf_df$dB, erf_df$AR)
  # bhd_central = 448216.7
)




approx_fun <- approxfun(
  x = erf_df$dB,
  y = erf_df$AR,
  method = "linear",
  rule = 2  # Extrapolate if out of bounds
)



# Test it 
approx_fun(data$average_cat)


healthiar::attribute_health(
  approach_risk = "absolute_risk",
  exp_central = data$average_cat,
  erf_eq_central = approx_fun,  
  pop_exp = data$ANTALL_PER,
  prop_pop_exp = data$prop_pop_exp,
  population = data$totpop,
  geo_id_disaggregated = data$GEO_ID,
  geo_id_aggregated = "Norway"
)

#?



healthiar::attribute_health(
  approach_risk = "absolute_risk",
  exp_central = data$average_cat,

  erf_eq_central = splinefun( 
    x = erf_df$dB, 
    y = erf_df$AR, 
    method = "natural" ),

  pop_exp     = data$ANTALL_PER,
  prop_pop_exp = data$prop_pop_exp,
  population  = data$totpop,
  geo_id_disaggregated = data$GEO_ID,
  geo_id_aggregated = "Norway"
)



# Create a spline function first
spline_fun <- splinefun(
  x = erf_df$dB,
  y = erf_df$AR,
  method = "natural"
)

# Wrap the spline function into a single-argument function
my_erf <- function(c) {
  spline_fun(c)
}



healthiar::attribute_health(
  approach_risk = "absolute_risk",
  exp_central = data$average_cat,
  population = data$totpop,
  prop_pop_exp = data$prop_pop_exp,
  pop_exp = data$ANTALL_PER,
  geo_id_disaggregated = data$GEO_ID,
  geo_id_aggregated = "Norway",
  erf_eq_central = my_erf,
  dw_central = 0.02,
  info = data.frame(
    pollutant = "road_noise",
    outcome = "highly_annoyance"
  )
)


################


# Step 1: Create your function for ERF (Exposure-Response Function)
erf_fun <- approxfun(
  x = erf_df$dB,
  y = erf_df$AR,  # in percent (e.g., 5 = 5%)
  method = "linear",
  rule = 2  # allow extrapolation if needed
)

# Step 2: Run attribute_health with function passed to erf_eq_central
result <- healthiar::attribute_health(
  approach_risk = "absolute_risk",
  exp_central = data$average_cat,
  erf_eq_central = erf_fun(data$average_cat),  # this is a function!
  pop_exp = data$ANTALL_PER,
  prop_pop_exp = data$prop_pop_exp,
  population = data$totpop,
  geo_id_disaggregated = data$GEO_ID,
  geo_id_aggregated = "Norway"
)




erf_df <- data.table(
  dB = seq(30, 85, by = 0.5)
)

# Apply the quadratic formula to compute AR
erf_df[, AR := 78.9270 - 3.1162 * dB + 0.0342 * dB^2]


erf <-splinefun(data$average_cat, data$AR, method="natural")

healthiar::attribute_health(
  approach_risk = "absolute_risk",
  exp_central = data$average_cat,
  # cutoff_central = 45,
  erf_eq_central = erf,
  # erf_eq_central =splinefun( 
  #                      x = erf_df$dB,
  #                      y = erf_df$AR,
  #                      method = "natural" ),  
  pop_exp = data$ANTALL_PER,
  prop_pop_exp = data$prop_pop_exp,
  population = data$totpop,
  geo_id_disaggregated = data$GEO_ID,
  geo_id_aggregated = "Norway"
)







