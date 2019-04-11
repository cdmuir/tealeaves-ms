source("r/header.R")

cs <- make_constants()

# Extended example 1: leaf size and leaf-to-air temperature differential ----
lp  <- make_leafpar(
  replace = list(
    leafsize = set_units(c(0.005, 0.1, 0.4), "m")
  )
)

ep <- make_enviropar(
  replace = list(
    S_sw = set_units(660, "W/m^2"),
    T_air = set_units(seq(278.15, 308.15, 5), "K")
  )
)

exe1 <- tleaves(lp, ep, cs, progress = TRUE, quiet = TRUE, set_units = TRUE,
                parallel = TRUE)

write_rds(exe1, "data/exe1.rds")

# Extended example 2: Solar radiation and leaf-to-air temperature differential  ----
lp  <- make_leafpar(
  replace = list(
    g_sw = set_units(c(1, 3, 5), "umol/m^2/s/Pa")
  )
)

ep <- make_enviropar(
  replace = list(
    S_sw = set_units(seq(50, 950, 100), "W/m^2")
  )
)

exe2 <- tleaves(lp, ep, cs, progress = TRUE, quiet = TRUE, set_units = TRUE,
                parallel = TRUE)

write_rds(exe2, "data/exe2.rds")

# Extended example 3: Wind speed and leaf-to-air temperature differential ----
lp  <- make_leafpar(
  replace = list(
    leafsize = set_units(c(0.005, 0.1, 0.5), "m")
  )
)

ep <- make_enviropar(
  replace = list(
    wind = set_units(exp(seq(log(0.01), log(10), length.out = 1e2)), "m/s")
  )
)

exe3 <- tleaves(lp, ep, cs, progress = TRUE, quiet = TRUE, set_units = TRUE,
                parallel = TRUE)

write_rds(exe3, "data/exe3.rds")

# Extended example 4: Stomatal ratio and evaporation ----
lp  <- make_leafpar(
  replace = list(
    g_sw = set_units(c(0.4, 4), "umol/s/m^2/Pa"),
    logit_sr = set_units(seq(-10, 10, length.out = 1e2))
  )
)

ep <- make_enviropar(
  replace = list(
    RH = set_units(0.2),
    T_air = set_units(293.15, "K"),
    wind = set_units(c(0, 2), "m/s")
  )
)

exe4 <- tleaves(lp, ep, cs, progress = TRUE, quiet = TRUE, set_units = TRUE,
                parallel = TRUE)

write_rds(exe4, "data/exe4.rds")
