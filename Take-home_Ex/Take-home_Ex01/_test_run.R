# =============================================================================
# R Validation Script — verify wrangling + key visualizations work
# Runs the core logic from index.qmd standalone, generates PNG previews
# =============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(forcats); library(stringr); library(purrr); library(tibble)
  library(scales); library(glue); library(patchwork); library(ggrepel)
  library(ggridges)
})


# ==== Common theme ====
theme_wrc <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.05), color = "#111111"),
      plot.subtitle    = element_text(color = "#555555", size = rel(0.9)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#EAEAEA"),
      axis.text        = element_text(color = "#333333"),
      strip.text       = element_text(face = "bold", color = "#111111"),
      strip.background = element_rect(fill = "#F0F0F0", color = NA)
    )
}

# ==== 1. Data import ====
cat("\n[1] Loading data...\n")
raw_path <- "data/insurance_raw.csv"
first_line <- readr::read_lines(raw_path, n_max = 1)
delim <- if (stringr::str_detect(first_line, ";")) ";" else ","
raw_df <- readr::read_delim(raw_path, delim = delim,
                            locale = locale(decimal_mark = ".", grouping_mark = ""),
                            show_col_types = FALSE)
cat(sprintf("    Loaded %d rows x %d cols (delim='%s')\n", nrow(raw_df), ncol(raw_df), delim))

# ==== 2. Wrangling ====
cat("\n[2] Wrangling...\n")
sandbox <- raw_df |>
  rename(years_licence_held = age_driving_licence) |>
  filter(!(total_premium == 0 & total_exposure == 0)) |>
  mutate(
    fuel_type = forcats::fct_na_value_to_level(factor(fuel_type), level = "Unknown"),
    policy_type      = factor(policy_type,
                              levels = c("TP","TPG","CC","COMP_E","COMP_N"),
                              labels = c("TP (basic)","TPG (+glass)",
                                         "CC (combined)","COMP-E (excess)",
                                         "COMP-N (no excess)")),
    policy_status    = factor(policy_status, levels = c("A","C"),
                              labels = c("Active","Cancelled")),
    business_type    = factor(business_type, levels = c("NB","P"),
                              labels = c("New Business","Portfolio")),
    payment_frequency= factor(payment_frequency, levels = c("A","S","Q"),
                              labels = c("Annual","Semi-annual","Quarterly")),
    bonus_score      = factor(bonus_score, levels = c("G","N","B"),
                              labels = c("Good","Neutral","Bad")),
    municipality_type= factor(municipality_type, levels = c("I","C","IS"),
                              labels = c("Inland","Coastal","Islands")),
    circulation_area = factor(circulation_area, levels = c("U","R"),
                              labels = c("Urban","Rural")),
    year_f           = factor(year),
    pure_premium    = total_incurred / pmax(total_exposure, 1e-6),
    claim_frequency = total_claims   / pmax(total_exposure, 1e-6),
    severity        = ifelse(total_claims > 0, total_incurred / total_claims, NA_real_),
    loss_ratio      = total_incurred / pmax(total_premium, 1e-6),
    has_claim       = total_claims > 0,
    large_loss      = total_incurred > 3784,
    driver_age_band = cut(driver_age, breaks = c(17,25,35,45,55,65,Inf),
                          labels = c("18-25","26-35","36-45","46-55","56-65","65+")),
    vehicle_age_band = cut(vehicle_age, breaks = c(-1,5,10,15,20,30,Inf),
                           labels = c("0-5","6-10","11-15","16-20","21-30","30+")),
    veh_value_band   = cut(vehicle_value, breaks = c(0,10000,20000,30000,50000,Inf),
                           labels = c("<10k","10-20k","20-30k","30-50k","50k+")),
    licence_band     = cut(years_licence_held, breaks = c(-1,5,15,25,35,Inf),
                           labels = c("0-5","6-15","16-25","26-35","35+"))
  )

cat(sprintf("    Sandbox: %d rows x %d cols\n", nrow(sandbox), ncol(sandbox)))
saveRDS(sandbox, "data/analytical_sandbox.rds")
cat("    Saved analytical_sandbox.rds\n")

# ==== 3. Generate preview PNGs of key figures ====
cat("\n[3] Generating key chart previews...\n")
dir.create("img", showWarnings = FALSE)

# --- Chart A: portfolio LR by category (key bivariate chart) ---
portfolio_lr <- with(sandbox, sum(total_incurred) / sum(total_premium))
cat(sprintf("    Portfolio LR = %s\n", percent(portfolio_lr, 0.1)))

lr_by_cat <- function(df, var) {
  df |>
    group_by(level = .data[[var]]) |>
    summarise(premium = sum(total_premium),
              incurred = sum(total_incurred),
              n = n(), .groups = "drop") |>
    mutate(LR = incurred / premium, var = var, level = as.character(level))
}
cat_for_lr <- c("policy_type","business_type","payment_frequency","bonus_score",
                "fuel_type","municipality_type","circulation_area","policy_status")
lr_panel <- purrr::map_dfr(cat_for_lr, ~lr_by_cat(sandbox, .x)) |>
  group_by(var) |>
  mutate(level = forcats::fct_reorder(level, LR)) |>
  ungroup() |>
  mutate(bar_col = ifelse(LR > portfolio_lr, "#C8102E", "#2E7D32"))

p_lr <- ggplot(lr_panel, aes(x = LR, y = level, fill = bar_col)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_vline(xintercept = portfolio_lr, linetype = "dashed", color = "#222222") +
  geom_text(aes(label = percent(LR, 0.1)), hjust = -0.1, size = 3, color = "#333333") +
  scale_fill_identity() +
  scale_x_continuous(labels = percent_format(1), expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~ var, scales = "free_y", ncol = 2) +
  labs(title = "Loss Ratio by categorical driver",
       subtitle = glue("Dashed line: portfolio LR ({percent(portfolio_lr, 0.1)}) - Red=worse, Green=better"),
       x = "Loss Ratio", y = NULL) +
  theme_wrc()
ggsave("img/preview_lr_by_cat.png", p_lr, width = 12, height = 10, dpi = 120)
cat("    + img/preview_lr_by_cat.png\n")

# --- Chart B: Lorenz curve ---
lorenz_df <- sandbox |>
  arrange(total_incurred) |>
  mutate(cum_pop = row_number() / n(),
         cum_loss = cumsum(total_incurred) / sum(total_incurred))
gini <- 1 - 2 * sum((lorenz_df$cum_pop - dplyr::lag(lorenz_df$cum_pop, default = 0)) *
                    (lorenz_df$cum_loss + dplyr::lag(lorenz_df$cum_loss, default = 0)) / 2)
cat(sprintf("    Gini = %.3f\n", gini))

lorenz_plot <- lorenz_df |>
  slice(round(seq(1, n(), length.out = 5000)))

p_lorenz <- ggplot(lorenz_plot, aes(x = cum_pop, y = cum_loss)) +
  geom_ribbon(aes(ymin = cum_pop, ymax = cum_loss), fill = "#C8102E", alpha = 0.18) +
  geom_abline(slope = 1, intercept = 0, color = "#888888", linetype = "dashed") +
  geom_line(color = "#222222", linewidth = 1.1) +
  annotate("text", x = 0.55, y = 0.18,
           label = glue("Gini coefficient = {round(gini, 3)}"),
           color = "#C8102E", fontface = "bold", size = 4.5) +
  scale_x_continuous(labels = percent_format(1)) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(title = "Lorenz curve - extreme loss concentration",
       subtitle = "Top 1% of policies generate ~50% of total losses",
       x = "Cumulative share of policies",
       y = "Cumulative share of losses") +
  theme_wrc()
ggsave("img/preview_lorenz.png", p_lorenz, width = 8, height = 6, dpi = 120)
cat("    + img/preview_lorenz.png\n")

# --- Chart C: Vehicle heatmap ---
heat <- sandbox |>
  filter(!is.na(vehicle_age_band), !is.na(veh_value_band)) |>
  group_by(vehicle_age_band, veh_value_band) |>
  summarise(LR = sum(total_incurred) / sum(total_premium),
            n  = n(), .groups = "drop") |>
  filter(n >= 100)

p_heat <- ggplot(heat, aes(x = veh_value_band, y = vehicle_age_band, fill = LR)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = percent(LR, 0.1)),
            color = "white", fontface = "bold", size = 3.3) +
  scale_fill_gradient(low = "#9CCC65", high = "#B71C1C",
                      labels = percent_format(1), name = "Loss Ratio") +
  labs(title = "Vehicle profile heatmap",
       subtitle = "Cells with n<100 suppressed",
       x = "Vehicle value band", y = "Vehicle age band") +
  theme_wrc() +
  theme(panel.grid = element_blank())
ggsave("img/preview_heatmap.png", p_heat, width = 8, height = 5, dpi = 120)
cat("    + img/preview_heatmap.png\n")

# --- Chart D: year x policy type heat ---
yp <- sandbox |>
  group_by(year, policy_type) |>
  summarise(LR = sum(total_incurred) / sum(total_premium), .groups = "drop")

p_yp <- ggplot(yp, aes(x = factor(year), y = policy_type, fill = LR)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(LR, 0.1),
                color = ifelse(LR > 0.85, "white", "#222222")),
            fontface = "bold", size = 3.6) +
  scale_color_identity() +
  scale_fill_gradient2(low = "#2E7D32", mid = "#FFF59D", high = "#B71C1C",
                       midpoint = 0.75, labels = percent_format(1), name = "LR") +
  labs(title = "LR trajectory by policy type",
       subtitle = "COMP-N crosses 100% in 2024",
       x = NULL, y = NULL) +
  theme_wrc() +
  theme(panel.grid = element_blank())
ggsave("img/preview_year_policy.png", p_yp, width = 7, height = 4.5, dpi = 120)
cat("    + img/preview_year_policy.png\n")

# --- Chart E: trend index ---
trend <- sandbox |>
  group_by(year) |>
  summarise(LR        = sum(total_incurred) / sum(total_premium),
            frequency = sum(total_claims) / sum(total_exposure),
            severity  = sum(total_incurred[total_claims > 0]) /
                        pmax(sum(total_claims[total_claims > 0]), 1),
            .groups   = "drop") |>
  pivot_longer(-year, names_to = "metric", values_to = "value") |>
  group_by(metric) |>
  mutate(index = value / value[year == 2022] * 100) |>
  ungroup() |>
  mutate(metric = factor(metric, levels = c("LR","frequency","severity"),
                          labels = c("Loss Ratio","Frequency","Severity")))

p_trend <- ggplot(trend, aes(x = year, y = index, color = metric, group = metric)) +
  geom_hline(yintercept = 100, color = "#888888", linetype = "dashed") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = number(index, 1)), vjust = -1, fontface = "bold", size = 3.5) +
  scale_color_manual(values = c("Loss Ratio" = "#C8102E",
                                "Frequency" = "#888888",
                                "Severity" = "#222222")) +
  scale_x_continuous(breaks = 2022:2024) +
  labs(title = "Where is the deterioration coming from?",
       subtitle = "All metrics rebased to 100 in 2022",
       x = NULL, y = "Index (2022 = 100)", color = NULL) +
  theme_wrc() +
  theme(legend.position = "bottom")
ggsave("img/preview_trend.png", p_trend, width = 8, height = 5, dpi = 120)
cat("    + img/preview_trend.png\n")

cat("\n[DONE] All key visualizations validated.\n\n")
