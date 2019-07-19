source("r/header.R")

exe1 <- read_rds("data/exe1.rds")
exe2 <- read_rds("data/exe2.rds")
exe3 <- read_rds("data/exe3.rds")
exe4 <- read_rds("data/exe4.rds")

# Extended example 1: leaf size and leaf-to-air temperature differential ----
# drop units for plotting
exe1$leafsize %<>% drop_units() 
exe1$T_air %<>% drop_units() 
exe1$T_leaf %<>% drop_units() 

exe1 %<>% mutate(leafsize1 = case_when(
  leafsize == 0.005 ~ "small (0.005 m)",
  leafsize == 0.1 ~ "medium (0.1 m)",
  leafsize == 0.4 ~ "large (0.4 m)"
))

gp_exe1 <- ggplot(exe1, aes(T_air, T_leaf - T_air, linetype = leafsize1)) +
  #scale_x_continuous(limits = c(278.15, 308.15)) +
  scale_y_continuous(limits = c(-3.5, 7.5)) +
  geom_line(size = 1.1) +
  xlab("Air Temperature [K]") +
  ylab("Leaf - Air Temperature [K]") +
  theme_bw() +
  scale_linetype_discrete(name  ="Leaf size",
                          breaks = c("large (0.4 m)", "medium (0.1 m)", "small (0.005 m)")) + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.key.width = unit(1, "cm"),
        legend.text.align = 1,
        legend.title.align = 1,
        legend.background = element_blank(),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.direction = "vertical",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  NULL

ggsave("figures/gp_exe1.pdf", width = 3.25, height = 3.25)

# Extended example 2: Solar radiation and leaf-to-air temperature differential  ----

exe2$g_sw %<>% drop_units() 
exe2$S_sw %<>% drop_units() 
exe2$T_air %<>% drop_units() 
exe2$T_leaf %<>% drop_units() 

exe2 %<>% mutate(g_sw1 = case_when(
  g_sw == 1 ~ "low",
  g_sw == 3 ~ "medium",
  g_sw == 5 ~ "high"
))

gp_exe2 <- ggplot(exe2, aes(S_sw, T_leaf - T_air, linetype = g_sw1)) +
  geom_line(size = 1.1) +
  scale_y_continuous(limits = c(-3.5, 7.5)) +
  xlab(expression(paste("Solar radiation [W ", m^-2, "]"))) +
  ylab("Leaf - Air Temperature [K]") +
  theme_bw() +
  scale_linetype_discrete(name  ="Stomatal conductance",
                          breaks = c("low", "medium", "high")) + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.key.width = unit(1, "cm"),
        legend.text.align = 0,
        legend.title.align = 0,
        legend.background = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.direction = "vertical",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  NULL

ggsave("figures/gp_exe2.pdf", width = 3.25, height = 3.25)

# Extended example 3: Wind speed and leaf-to-air temperature differential  ----

exe3$leafsize %<>% drop_units() 
exe3$T_air %<>% drop_units() 
exe3$T_leaf %<>% drop_units() 
exe3$wind %<>% drop_units() 

exe3 %<>% mutate(
  leafsize1 = case_when(
    leafsize == 0.005 ~ "small (0.005 m)",
    leafsize == 0.1 ~ "medium (0.1 m)",
    leafsize == 0.5 ~ "large (0.5 m)"
  ),
  flow = case_when(
    Re <= 4000 ~ "laminar (Re < 4000)",
    Re > 4000 ~ "turbulent (Re > 4000)"
  ))

gp_exe3 <- ggplot(exe3, aes(log10(Ar), T_leaf - T_air, linetype = leafsize1)) +
  geom_line(size = 1.1) +
  facet_grid(~ flow) +
  xlab("Archemides Number (log-scale)") +
  ylab("Leaf - Air Temperature [K]") +
  geom_vline(xintercept = log10(c(0.1, 10))) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  NULL

ggsave("figures/gp_exe3.pdf", width = 3.25, height = 3.25)

# Extended example 4: Stomatal ratio and evaporation ----
exe4$g_sw %<>% drop_units() 
exe4$E %<>% set_units("mmol/m^2/s") %>% drop_units() 
exe4$logit_sr %<>% drop_units() 
exe4$sr <- plogis(exe4$logit_sr)
exe4$T_leaf %<>% drop_units()
exe4$wind %<>% drop_units() 

exe4 %<>% mutate(
  g_sw1 = case_when(
    g_sw == 0.4 ~ "low",
    g_sw == 4 ~ "high"
  ),
  Convection = case_when(
    wind == 0 ~ "free\nu = 0 m/s",
    wind == 2 ~ "forced\nu = 2 m/s"
  ))

gp_exe4 <- ggplot(filter(exe4, !is.na(E)), 
                  aes(sr, E, linetype = g_sw1, color = Convection)) +
  geom_line(size = 1.1) +
  xlab("Stomatal Ratio") +
  ylab(expression(paste("Evaporation [mmol ", m^-2, " ", s^-1, "]"))) +
  theme_bw() +
  scale_linetype_discrete(name = "Stomatal\nconductance") + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.position = "right",
        legend.key.height = unit(1, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL

ggsave("figures/gp_exe4.pdf", width = 3.25, height = 3.25)

# Combine figure panels into single figure for publication ----
plot_grid(gp_exe1, gp_exe2, gp_exe3, gp_exe4, labels = LETTERS[1:4],
          ncol = 2)
ggsave("figures/fig2.pdf", width = 6.5, height = 6.5)
