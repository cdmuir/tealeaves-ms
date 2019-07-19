source("r/header.R")

library(tealeaves)

cs <- make_constants()
ep <- make_enviropar()

# Leaf parameters ----

## g_sw ----
lp <- make_leafpar(replace = list(
  g_sw = set_units(10 ^ seq(-1, 1, 0.01), umol/m ^ 2/s/Pa)
))

tl_gsw <- tleaves(lp, ep, cs, parallel = TRUE, set_units = TRUE) %>%
  mutate_if(~ inherits(.x, "units"), drop_units)

gp_gsw <- ggplot(tl_gsw, aes(g_sw, T_leaf)) +
  geom_line(size = 2, lineend = "round") +
  xlab(expression(paste(italic(g)[sw], " [", mu, "mol ", m^-2~s^-1~Pa^-1, "]"))) +
  ylab("Leaf Temperature [K]") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  NULL

## leafsize ----
lp <- make_leafpar(replace = list(
  leafsize = set_units(0.4 * 10 ^ seq(-2, 0, 0.01), m)
))

tl_leafsize <- tleaves(lp, ep, cs, parallel = TRUE, set_units = TRUE) %>%
  mutate_if(~ inherits(.x, "units"), drop_units)

gp_leafsize <- ggplot(tl_leafsize, aes(leafsize, T_leaf)) +
  geom_line(size = 2, lineend = "round") +
  xlab("Leaf characterisic dimension [m]") +
  ylab("Leaf Temperature [K]") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  NULL

## stomatal ratio ----
lp <- make_leafpar(replace = list(
  logit_sr = set_units(seq(-10, 10, 0.1))
))

tl_sr <- tleaves(lp, ep, cs, parallel = TRUE, set_units = TRUE) %>%
  mutate_if(~ inherits(.x, "units"), drop_units)

gp_sr <- ggplot(tl_sr, aes(plogis(logit_sr), T_leaf)) +
  geom_line(size = 2, lineend = "round") +
  xlab("Stomatal ratio") +
  ylab("Leaf Temperature [K]") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  NULL

# Environmental parameters ----

## RH ----
lp <- make_leafpar()
ep <- make_enviropar(replace = list(
  RH = set_units(seq(0, 1, 0.01))
))

tl_rh <- tleaves(lp, ep, cs, parallel = TRUE, set_units = TRUE) %>%
  mutate_if(~ inherits(.x, "units"), drop_units)

gp_rh <- ggplot(tl_rh, aes(RH, T_leaf)) +
  geom_line(size = 2, lineend = "round") +
  xlab("Relative humidity") +
  ylab("Leaf Temperature [K]") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  NULL

## sunlight ----
ep <- make_enviropar(replace = list(
  S_sw = set_units(seq(0, 1000, 10), W / m ^ 2)
))

tl_ssw <- tleaves(lp, ep, cs, parallel = TRUE, set_units = TRUE) %>%
  mutate_if(~ inherits(.x, "units"), drop_units)

gp_ssw <- ggplot(tl_ssw, aes(S_sw, T_leaf)) +
  geom_line(size = 2, lineend = "round") +
  xlab(expression(paste("Solar radiation [W ", m^-2, "]"))) +
  ylab("Leaf Temperature [K]") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  NULL

## wind ----
ep <- make_enviropar(replace = list(
  wind = set_units(seq(0, 10, 0.1), m / s)
))

tl_wind <- tleaves(lp, ep, cs, parallel = TRUE, set_units = TRUE) %>%
  mutate_if(~ inherits(.x, "units"), drop_units)

gp_wind <- ggplot(tl_wind, aes(wind, T_leaf)) +
  geom_line(size = 2, lineend = "round") +
  xlab(expression(paste("Wind speed [m ", s^-1, "]"))) +
  ylab("Leaf Temperature [K]") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  NULL

## Combined plot ----

ly <- range(c(drop_units(ep$T_air), 
              tl_gsw$T_leaf, tl_leafsize$T_leaf, tl_sr$T_leaf,
              tl_rh$T_leaf, tl_ssw$T_leaf, tl_wind$T_leaf))
T_air <- as.character(round(first(tl_gsw$T_air), 2))
label <- bquote(italic(T)[air] == .(T_air) ~ K)
gp_gsw <- gp_gsw + scale_y_continuous(limits = ly) +
  geom_hline(yintercept = drop_units(ep$T_air), linetype = "dashed")  + 
  annotate("text", 0, first(tl_gsw$T_air), hjust = 0, vjust = -0.5,
           label = deparse(label), parse = TRUE)
gp_leafsize <- gp_leafsize + scale_y_continuous(limits = ly) +
  geom_hline(yintercept = drop_units(ep$T_air), linetype = "dashed")
gp_sr <- gp_sr + scale_y_continuous(limits = ly) +
  geom_hline(yintercept = drop_units(ep$T_air), linetype = "dashed")
gp_rh <- gp_rh + scale_y_continuous(limits = ly) +
  geom_hline(yintercept = drop_units(ep$T_air), linetype = "dashed")
gp_ssw <- gp_ssw + scale_y_continuous(limits = ly) +
  geom_hline(yintercept = drop_units(ep$T_air), linetype = "dashed")
gp_wind <- gp_wind + scale_y_continuous(limits = ly) +
  geom_hline(yintercept = drop_units(ep$T_air), linetype = "dashed")

plot_grid(gp_gsw, gp_leafsize, gp_sr, 
          gp_rh, gp_ssw, gp_wind, labels = LETTERS[1:6], nrow = 2)

ggsave("figures/figS1.pdf", width = 9, height = 6)

## Export results ----

tl_gsw %<>% mutate(variable = "g_sw")
tl_leafsize %<>% mutate(variable = "leafsize")
tl_sr  %<>% mutate(variable = "logit_sr")
tl_rh %<>% mutate(variable = "RH")
tl_ssw %<>% mutate(variable = "S_sw")
tl_wind  %<>% mutate(variable = "wind")
write_rds(bind_rows(tl_gsw, tl_leafsize, tl_sr, tl_rh, tl_ssw, tl_wind), 
          "data/pars.rds")
