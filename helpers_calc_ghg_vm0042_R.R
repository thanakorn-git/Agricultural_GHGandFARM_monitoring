# helpers_calc_ghg_vm0042_R

#' Calculate fossil fuel CO2 emissions
#' @param fuel_type character: Diesel, Gasoline, LPG
#' @param fuel_volume numeric: in liters
calc_fossil_fuel_emissions <- function(fuel_type, fuel_volume) {
  ef_table <- list(
    Diesel   = 2.7,
    Gasoline = 2.3,
    LPG      = 1.5
  )
  ef <- ef_table[[fuel_type]]
  emissions <- fuel_volume * ef * 0.001  # Convert kg to tons
  return(emissions)  # in tCO2
}

#' Calculate manure CH4 emissions (default IPCC method)
#' @param animal_type character
#' @param animal_count integer
#' @param weight numeric in kg
calc_manure_emissions <- function(animal_type, animal_count, weight) {
  # Volatile Solids (VS) ~ 5.5 kg/day/animal for cattle @ 500kg — adjust as needed
  vs_per_day <- 0.011 * weight  # Rough default (IPCC Tier 1)
  ch4_per_kg_dm <- 0.0027  # CH₄ per kg dry matter
  total_vs <- vs_per_day * 365 * animal_count
  ch4_emissions <- total_vs * ch4_per_kg_dm * 0.001  # Convert to tons
  return(ch4_emissions)
}

#' Biomass Burning CH4 Emissions
calc_biomass_burning_emissions <- function(yield_t_ha, trash_ratio = 0.25,
                                           burn_fraction = 1.0,
                                           dry_matter = 0.45) {
  dm = yield_t_ha * trash_ratio * burn_fraction * dry_matter * 1000  # kg DM
  ch4_factor <- 2.7  # g CH₄/kg DM
  ch4_kg <- dm * ch4_factor / 1000  # in kg
  ch4_tonnes <- ch4_kg * 0.001
  return(ch4_tonnes)
}

#' Fertilizer N2O Emissions (Simplified Tier 1)
calc_fertilizer_n2o_emissions <- function(fertilizer_mass_kg, n_content_percent) {
  n_applied <- fertilizer_mass_kg * (n_content_percent / 100)
  # Direct N₂O: IPCC default 1% of N → as N₂O-N then convert to N₂O
  n2o_direct <- n_applied * 0.01 * (44 / 28)
  # Indirect volatilization + leaching (simplified estimate)
  n2o_indirect <- n_applied * 0.005 * (44 / 28)
  return((n2o_direct + n2o_indirect) * 0.001)  # tCO2e
}

#' Total GHG Emissions Summation Function
calc_total_ghg <- function(fuel_type, fuel_amount,
                           animal_type, 
                           animal_count, 
                           animal_weight,
                           sugarcane_yield,
                           burn_fraction,
                           dry_matter,
                           fert_mass, fert_n_pct) {
  
  co2_fuel <- calc_fossil_fuel_emissions(fuel_type, fuel_amount)
  ch4_manure <- calc_manure_emissions(animal_type, animal_count, animal_weight)
  ch4_burn <- calc_biomass_burning_emissions(yield_t_ha = sugarcane_yield,
                                             burn_fraction = burn_fraction,
                                             dry_matter = dry_matter)
  n2o_fert <- calc_fertilizer_n2o_emissions(fert_mass, fert_n_pct)
  
  # GWP conversions (AR5): CH4 = 28, N2O = 265
  ch4_total <- (ch4_manure + ch4_burn) * 28
  n2o_total <- n2o_fert * 265
  
  total_emissions <- co2_fuel + ch4_total + n2o_total
  return(list(
    CO2 = co2_fuel,
    CH4 = ch4_total,
    N2O = n2o_total,
    TOTAL = total_emissions
  ))
}
