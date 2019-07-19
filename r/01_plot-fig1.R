source("r/header.R")

cs <- make_constants()
lp  <- make_leafpar()
ep <- make_enviropar()
write_rds(c(lp, ep), "data/fig1_pars.rds")

# Find equilibrium leaf temperature
eq <- tleaf(lp, ep, cs, quiet = TRUE)

# Model energy balance over range of leaf temperatures
tleaf <- seq(290, 310, 1)
df <- tleaf %>%
  map_df(~ {
    eb <- energy_balance(.x, lp, ep, cs, quiet = TRUE, components = TRUE)
    nms <- c("energy_balance", names(eb$components))
    setNames(c(drop_units(eb$energy_balance), unlist(eb$components)), nms) %>%
      t() %>%
      as.data.frame()
  }) %>%
  select(-E) %>%
  mutate(tleaf = tleaf, H = -H, L = -L, S_r = -S_r) %>%
  gather(component, flux, -tleaf) %>%
  mutate(
    size = ifelse(component == "energy_balance", 3, 1.2),
    color = ifelse(component == "energy_balance", "steelblue", "tomato"),
    component = factor(component, levels = c("S_r", "R_abs", "H", "L", "energy_balance"))
  )

size_values <- summarize(group_by(df, component), size = first(size))
size_values <- set_names(size_values$size, size_values$component)
color_values <- summarize(group_by(df, component), color = first(color))
color_values <- set_names(color_values$color, color_values$component)
eq_point <- tibble(tleaf = drop_units(eq$T_leaf), flux = 0,
                   component = "energy_balance", label = "Equilibrium")

fig1a <- ggplot(df, aes(tleaf, flux, group = component, size = component, 
                        color = component)) +
  scale_size_manual(values = size_values) +
  scale_color_manual(values = color_values) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = drop_units(ep$T_air), color = "grey", 
             linetype = "dashed") +
  geom_vline(xintercept = drop_units(eq$T_leaf), color = "grey", 
             linetype = "dashed") +
  geom_line(lineend = "round") +
  geom_point(data = eq_point, stroke = 1.5, fill = "white", shape = 21) +
  xlab("Leaf Temperature [K]") +
  ylab(expression(paste("Energy flux [W ", m^-2, "]"))) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., breaks = drop_units(c(ep$T_air, eq$T_leaf)),
                        labels = c(expression(italic(T)[air]),
                                   expression(italic(T)[leaf])))
  ) +
  theme_bw() + 
  theme(
    axis.text = element_text(size = 10),
    axis.ticks.x.top = element_blank(),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  NULL

# Position labels for plotting
xr <- ggplot_build(fig1a)$layout$panel_scales_x[[1]]$range$range
yr <- ggplot_build(fig1a)$layout$panel_scales_y[[1]]$range$range
delta_y <- 1

while(delta_y > 1e-1) {
  
  labels <- df %>%
    mutate(x = 292.5) %>%
    filter(tleaf > x - 2.5, tleaf < x + 2.5) %>%
    group_by(component) %>%
    summarise(
      intercept = {
        fit <- lm(flux ~ tleaf)
        coef(fit)[1]
      },
      slope = {
        fit <- lm(flux ~ tleaf)
        coef(fit)[2]
      },
      angle = 180 / pi * atan(slope * (3 / 7.5) * diff(xr) / diff(yr)),
      tleaf = first(x),
    ) %>%
    mutate(
      flux = intercept + slope * tleaf,
      flux = flux + sign(flux) * 150,
      label = case_when(
        component == "R_abs" ~ "Absorbed radiation",
        component == "energy_balance" ~ "Net flux",
        component == "H" ~ "Sensible heat",
        component == "L" ~ "Latent heat",
        component == "S_r" ~ "Infrared radiation",
      )
    )
  
  gp <- fig1a + 
    geom_text(data = labels, 
              mapping = aes(tleaf, flux, label = label, angle = angle), 
              inherit.aes = FALSE)
  
  yr_new <- ggplot_build(gp)$layout$panel_scales_y[[1]]$range$range
  delta_y <- abs(diff(yr) - diff(yr_new))
  yr <- yr_new

}

rm(gp)

# Add labels to figure

fig1a <- fig1a + 
  geom_text(data = labels, 
            mapping = aes(tleaf, flux, label = label, angle = angle), 
            inherit.aes = FALSE) + 
  annotate("text", 
           x = eq_point$tleaf + 100 * (3 / 7.5) * diff(xr) / diff(yr), 
           y = eq_point$flux + 100, 
           label = eq_point$label, vjust = 0, hjust = 0)

# Import panel B
fig1b <- ggdraw() +
  draw_image("figures/fig1b.pdf")

# Combine and save
fig1 <- plot_grid(fig1a, fig1b, ncol = 1, rel_heights = c(3, 7.5), 
                  labels = "AUTO")

ggsave("figures/fig1.pdf", fig1, width = 6.5, height = 10.5, units = "in",)
