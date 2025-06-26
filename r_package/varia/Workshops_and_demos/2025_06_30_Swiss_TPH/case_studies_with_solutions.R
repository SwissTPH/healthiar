# healthiar CASE STUDIES WITH SOLUTIONS#############################################################

# SETUP
## INSTALL healthiar: we'll install healthiar all together.
## LOAD DATA SETS: run the 4 lines below to load the case study data
pm_lc_ch <- pm_lc_ch
pm_lc_cantons <- pm_lc_cantons
noise_ha_ch <- noise_ha_ch
noise_ha_cantons <- noise_ha_cantons

# CASE STUDY 1: AIR POLLUTION (PM2.5) & LUNG CANCER (LC) INCIDENCE IN SWITZERLAND (CH) #############
## Data sets to be used: pm_lc_ch & pm_lc_cantons

## Research questions:
### 1.1 How many LC cases were attributable to PM2.5 exposure in CH in 2023?
### 1.2 Did the number of attributable LC cases change between 2013 and 2023?
### 1.4 A policy to reduce PM2.5 exposure to 5 micrograms / m^3 of air in CH would cost XXX to implement. Would
###     it be financially advantageous to implement (considering only policy implementation and direct treatment costs of
###     20000 CHF per LC case)?
### 1.3 (ADVANCED) Which canton had the highest absolute number of attributable LC cases in 2023?
### 1.4 (ADVANCED) Which canton had the highest rate of attributable LC cases in 2023?

# CASE STUDY 2: NOISE & HIGH ANNOYANCE IN SWITZERLAND ##########################
