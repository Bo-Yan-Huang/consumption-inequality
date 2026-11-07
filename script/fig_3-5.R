# Calculate Var. and Gini coef. etc.
# Author: BoYanHuang
# Created Jun 12, 2025
# Last modify Jun 13, 2025 by BoYanHuang

library(tidyverse)
library(DescTools)
library(modi)

# read the cross sectional all file
df_all <- read.csv("HH_inc_exp//HH_all.csv") %>% as_tibble()
df_all <- df_all %>% filter(year != 2012)

# !!NOTE: some NA are zero and some NA are missing val.
# keep strictly positive earnings; zero and non-positive become NA
df_all <- df_all %>% mutate(inc_earnings = ifelse(inc_earnings <= 0, NA, inc_earnings))
df_all <- df_all %>% mutate(across(starts_with('inc_earnings_'), ~ ifelse(. <= 0, NA, .)))

# Each earnings series keeps values of at least 5% of that year's median of
# the same series (script/sample_floor.R). The floor is set series by series:
# a household below it for the main earner still counts for the household,
# and the reverse.
source("script//sample_floor.R")
df_floor <- df_all %>% group_by(year) %>%
    mutate(across(c(inc_earnings, inc_earnings_max, inc_earnings_max_raw,
                    inc_earnings_main_raw, inc_earnings_spouse_raw), ~ floor_na(.x, weight))) %>%
    ungroup()

# Fig 5 From individual to HH inequality
df_Fig_3_indHH <- df_floor %>% group_by(year) %>% 
    summarise(Var_earnings_max = weighted.var(log(inc_earnings_max, exp(1)), weight, na.rm = T),
              Var_earnings_HH = weighted.var(log(inc_earnings, exp(1)), weight, na.rm = T),
              Gini_earnings_max = Gini(inc_earnings_max, weight, na.rm = T),
              Gini_earnings_HH = Gini(inc_earnings, weight, na.rm = T))
# plot
colors <- c("Main Earner" = "blue", "Household" = "red")
ggplot(df_Fig_3_indHH) +
    geom_line(aes(x = year, y = Var_earnings_max, colour = "Main Earner"), linewidth = 1) +
    geom_line(aes(x = year, y = Var_earnings_HH, colour = "Household"), linewidth = 1) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Variance of Log") +
    labs(title = "Variance of Log Earnings") +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("Main Earner", "Household"),
        labels = c("Main Earner", "Household")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.17, 0.75))
ggsave("figures/Fig_3/Fig_3a_Var_indHH.png", dpi = 600, width = 5, height = 3)
ggplot(df_Fig_3_indHH) +
    geom_line(aes(x = year, y = Gini_earnings_max, colour = "Main Earner"), linewidth = 1) +
    geom_line(aes(x = year, y = Gini_earnings_HH, colour = "Household"), linewidth = 1) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Gini Coefficient") +
    labs(title = "Gini Coefficient of Earnings") +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("Main Earner", "Household"),
        labels = c("Main Earner", "Household")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.17, 0.75))
ggsave("figures/Fig_3/Fig_3b_Gini_indHH.png", dpi = 600, width = 5, height = 3)

# Robustness for the appendix: the same comparison with the main earner left on
# the per-earner scale, which is how Figure 3a and 3b were drawn before the
# equivalization was made consistent. Household stays per adult equivalent.
df_Fig_3_indHH_raw <- df_floor %>% group_by(year) %>%
    summarise(Var_earnings_max = weighted.var(log(inc_earnings_max_raw, exp(1)), weight, na.rm = T),
              Var_earnings_HH = weighted.var(log(inc_earnings, exp(1)), weight, na.rm = T),
              Gini_earnings_max = Gini(inc_earnings_max_raw, weight, na.rm = T),
              Gini_earnings_HH = Gini(inc_earnings, weight, na.rm = T))
colors <- c("Main Earner" = "blue", "Household" = "red")
ggplot(df_Fig_3_indHH_raw) +
    geom_line(aes(x = year, y = Var_earnings_max, colour = "Main Earner"), linewidth = 1) +
    geom_line(aes(x = year, y = Var_earnings_HH, colour = "Household"), linewidth = 1) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Variance of Log") +
    labs(title = "Variance of Log Earnings \n (Main Earner Not Equivalized)") +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("Main Earner", "Household"),
        labels = c("Main Earner, per earner", "Household, per adult equiv.")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.72, 0.18))
ggsave("figures/Fig_3/Fig_3g_Var_indHH_raw.png", dpi = 600, width = 5, height = 3)
ggplot(df_Fig_3_indHH_raw) +
    geom_line(aes(x = year, y = Gini_earnings_max, colour = "Main Earner"), linewidth = 1) +
    geom_line(aes(x = year, y = Gini_earnings_HH, colour = "Household"), linewidth = 1) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Gini Coefficient") +
    labs(title = "Gini Coefficient of Earnings \n (Main Earner Not Equivalized)") +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("Main Earner", "Household"),
        labels = c("Main Earner, per earner", "Household, per adult equiv.")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.72, 0.18))
ggsave("figures/Fig_3/Fig_3h_Gini_indHH_raw.png", dpi = 600, width = 5, height = 3)

# single/married
colors <- c("0" = "blue", "1" = "red")
df_Fig_3_single <- df_floor %>% group_by(year, marriage) %>% 
    summarise(Var_earnings = weighted.var(log(inc_earnings, exp(1)), weight, na.rm = T)) %>%
    filter(!is.na(marriage))
ggplot(df_Fig_3_single, aes(x = year, y = Var_earnings, colour = factor(marriage))) +
    geom_line(linewidth = 1) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Variance of Log") +
    labs(title = "Variance of Log Earnings") +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("0", "1"),
        labels = c("Single", "Married")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.17, 0.75))
ggsave("figures/Fig_3/Fig_3c_Var_single.png", dpi = 600, width = 5, height = 3)
# Household structure (3d-3f) describes all households. A head or spouse
# counts as an earner when their own earnings clear the floor for their
# series, the same rule as every other earnings statistic.
df_Fig_3_single <- df_floor %>% filter(year >= 1988) %>% 
    group_by(year) %>%
    summarise(married_ratio = sum(marriage == 1, na.rm = T) / sum(!is.na(marriage)),
              two_earner_ratio = sum(!is.na(inc_earnings_spouse_raw) & !is.na(inc_earnings_main_raw)) / sum(!is.na(spouse_id)))
ggplot(df_Fig_3_single, aes(x = year)) +
    geom_line(aes(y = married_ratio), colour = "blue", linewidth = 1) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Ratio") +
    labs(title = "Fraction of Married Households \n Among All Households") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/Fig_3/Fig_3d_married_ratio.png", dpi = 600, width = 5, height = 3)
ggplot(df_Fig_3_single, aes(x = year)) +
    geom_line(aes(y = two_earner_ratio), colour = "blue", linewidth = 1) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Ratio") +
    labs(title = "Fraction of Two-Earner Households \n Among Cohabiting Couples") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/Fig_3/Fig_3e_two_earner_ratio.png", dpi = 600, width = 5, height = 3)
# f between-spouse correlation among Two-Earner Cohabiting Couples
# NOTE: use the per-earner series here. Dividing both spouses by the same eqi_w
# adds a common -log(eqi_w) to both logs, which inflates the correlation.
df_Fig_3_correlation <- df_floor %>% filter(year >= 1988, !is.na(inc_earnings_spouse_raw), !is.na(inc_earnings_main_raw)) %>%
    group_by(year) %>%
    summarise(correlation = cor(log(inc_earnings_main_raw, exp(1)), log(inc_earnings_spouse_raw, exp(1)), use = "pairwise.complete.obs"))
ggplot(df_Fig_3_correlation, aes(x = year, y = correlation)) +
    geom_line(colour = "blue", linewidth = 1) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Correlation") +
    labs(title = "Between-Spouse Corr. of Log Earnings \n Among Two-Earner Cohabiting Couples") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/Fig_3/Fig_3f_correlation.png", dpi = 600, width = 5, height = 3)

# Fig6 Private transfers and asset income
colors <- c("HH Earnings" = "blue", "HH Earnings + Priv. Transf." = "red", "HH Earnings + Asset Income" = "green", "HH Earnings + Priv. Transf. + Asset Income" = "purple")
# Each line is its own series with its own floor. The sums are formed from the
# unfloored components, so a household with no earnings but some asset income
# is in the asset-income line, and each sum is then floored on its own median.
# Without a floor, households with no earnings and a few dollars of interest
# pushed the asset-income line's variance of logs to about 4.
# fill 0 for NA inc_earnings, inc_ptransfer, inc_investment
df_Fig_4 <- df_all %>% mutate(across(c(inc_earnings, inc_ptransfer, inc_investment), ~ ifelse(is.na(.), 0, .)))
df_Fig_4 <- df_Fig_4 %>% mutate(inc_earnings_ptransfer = inc_earnings + inc_ptransfer,
                                inc_earnings_investment = inc_earnings + inc_investment,
                                inc_earnings_ptransfer_investment = inc_earnings + inc_ptransfer + inc_investment)
# floor each of the four series; non-positive values become NA too
df_Fig_4 <- df_Fig_4 %>% group_by(year) %>%
    mutate(across(c(inc_earnings, inc_earnings_ptransfer, inc_earnings_investment, inc_earnings_ptransfer_investment),
                  ~ floor_na(.x, weight))) %>%
    ungroup()
# calculate variance and Gini coefficient for each year
df_Fig_4_inc <- df_Fig_4 %>% group_by(year) %>%
    summarise(Var_1 = weighted.var(log(inc_earnings, exp(1)), weight, na.rm = T),
              Var_2 = weighted.var(log(inc_earnings_ptransfer, exp(1)), weight, na.rm = T),
              Var_3 = weighted.var(log(inc_earnings_investment, exp(1)), weight, na.rm = T),
              Var_4 = weighted.var(log(inc_earnings_ptransfer_investment, exp(1)), weight, na.rm = T),
              Gini_1 = Gini(inc_earnings, weight, na.rm = T),
              Gini_2 = Gini(inc_earnings_ptransfer, weight, na.rm = T),
              Gini_3 = Gini(inc_earnings_investment, weight, na.rm = T),
              Gini_4 = Gini(inc_earnings_ptransfer_investment, weight, na.rm = T))
# plot
ggplot(df_Fig_4_inc) +
    geom_line(aes(x = year, y = Var_1, colour = "HH Earnings"), linewidth = 1) +
    geom_line(aes(x = year, y = Var_2, colour = "HH Earnings + Priv. Transf."), linewidth = 1) +
    geom_line(aes(x = year, y = Var_3, colour = "HH Earnings + Asset Income"), linewidth = 1) +
    geom_line(aes(x = year, y = Var_4, colour = "HH Earnings + Priv. Transf. + Asset Income"), linewidth = 1) +
    labs(title = "Var. of Log Income", x = "Year", y = "Variance of Log") +
    # headroom for the legend; expand_limits widens the axis without dropping data
    expand_limits(y = 0.85) +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("HH Earnings", "HH Earnings + Priv. Transf.", "HH Earnings + Asset Income", "HH Earnings + Priv. Transf. + Asset Income"),
        labels = c("HH Earnings", "+ Priv. Transf.", "+ Asset Income", "+ Priv. Transf. + Asset Income")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.32, 0.83))
ggsave("figures/Fig_4/Fig_4a_Var_inc.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_4_inc) +
    geom_line(aes(x = year, y = Gini_1), colour = colors[1], linewidth = 1) +
    geom_line(aes(x = year, y = Gini_2), colour = colors[2], linewidth = 1) +
    geom_line(aes(x = year, y = Gini_3), colour = colors[3], linewidth = 1) +
    geom_line(aes(x = year, y = Gini_4), colour = colors[4], linewidth = 1) +
    labs(title = "Gini of Income", x = "Year", y = "Gini Coefficient") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.17, 0.75))
ggsave("figures/Fig_4/Fig_4b_Gini_inc.png", dpi = 600, width = 4, height = 4)
# correlation between inc_earnings_investment and private transfers
df_TEMP <- df_all %>% mutate(inc_ptransfer = ifelse(is.na(inc_ptransfer), 0, inc_ptransfer),
                              inc_investment = ifelse(is.na(inc_investment), 0, inc_investment))
df_TEMP <- df_TEMP %>% filter(inc_ptransfer > 0 | inc_investment > 0)
df_Fig_4_correlation <- df_TEMP %>% 
    group_by(year) %>%
    summarise(correlation = cor(inc_ptransfer, inc_investment, use = "pairwise.complete.obs"))
print(df_Fig_4_correlation, n=100)
# reported in the appendix: Pearson correlation of the two levels among
# households with either a private transfer or asset income
ggplot(df_Fig_4_correlation, aes(x = year, y = correlation)) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_line(colour = "blue", linewidth = 1) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Correlation") +
    labs(title = "Correlation of Private Transfers and Asset Income") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/Fig_4/Fig_4e_corr_transfer_asset.png", dpi = 600, width = 5, height = 3)
# plot
ggplot(df_Fig_4_inc) +
    geom_line(aes(x = year, y = Var_1), colour = colors[1], linewidth = 1) +
    geom_line(aes(x = year, y = Var_2), colour = colors[2], linewidth = 1) +
    geom_line(aes(x = year, y = Var_4), colour = colors[4], linewidth = 1) +
    labs(title = "Var. of Log Income", x = "Year", y = "Variance of Log") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.42, 0.85))
ggsave("figures/Fig_4/Fig_4c_Var_inc.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_4_inc) +
    geom_line(aes(x = year, y = Gini_1), colour = colors[1], linewidth = 1) +
    geom_line(aes(x = year, y = Gini_2), colour = colors[2], linewidth = 1) +
    geom_line(aes(x = year, y = Gini_4), colour = colors[4], linewidth = 1) +
    labs(title = "Gini of Income", x = "Year", y = "Gini Coefficient") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.17, 0.75))
ggsave("figures/Fig_4/Fig_4d_Gini_inc.png", dpi = 600, width = 4, height = 4)


# Fig5 Government benefits and tax
# top: Pre-Govt. Income, + Govt. Benefits
# bottom: Pre-Tax Income, Disposable Income
colors <- c("Pre-Govt. Income" = "blue", "Pre-Tax Income" = "red", "Disposable Income" = "green")
# fill 0 for NA inc_gtransfer, inc_total, exp_tax
df_all <- df_all %>% mutate(across(c(inc_gtransfer, inc_total, exp_tax), ~ ifelse(is.na(.), 0, .)))
df_all <- df_all %>% mutate(Pre_Govt = inc_total - inc_gtransfer,
                            Pre_Tax = inc_total,
                            Disposable = inc_total - exp_tax)
# turn 0 to NA inc_gtransfer, inc_total, exp_tax, Pre_Govt, Pre_Tax, Disposable
df_all <- df_all %>% mutate(across(c(inc_gtransfer, inc_total, exp_tax, Pre_Govt, Pre_Tax, Disposable), 
                                   ~ ifelse(. <= 0, NA, .)))
# each income concept keeps values of at least 5% of that year's median of
# the same concept (script/sample_floor.R)
df_all <- df_all %>% group_by(year) %>%
    mutate(across(c(Pre_Govt, Pre_Tax, Disposable), ~ floor_na(.x, weight))) %>%
    ungroup()
# calculate variance and Gini coefficient for each year
df_Fig_5_gov <- df_all %>% group_by(year) %>% 
    summarise(Var_1 = weighted.var(log(Pre_Govt, exp(1)), weight, na.rm = T),
              Var_2 = weighted.var(log(Pre_Tax, exp(1)), weight, na.rm = T),
              Var_3 = weighted.var(log(Disposable, exp(1)), weight, na.rm = T),
              Gini_1 = Gini(Pre_Govt, weight, na.rm = T),
              Gini_2 = Gini(Pre_Tax, weight, na.rm = T),
              Gini_3 = Gini(Disposable, weight, na.rm = T))
# plot
ggplot(df_Fig_5_gov) +
    geom_line(aes(x = year, y = Var_1, colour = "Pre-Govt. Income"), linewidth = 1) +
    geom_line(aes(x = year, y = Var_2, colour = "Pre-Tax Income"), linewidth = 1) +
    labs(title = "Var. of Log Income", x = "Year", y = "Variance of Log") +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("Pre-Govt. Income", "Pre-Tax Income"),
        labels = c("Pre-Govt. Income", "+ Govt. Benefits")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.24, 0.87))
ggsave("figures/Fig_5/Fig_5a_Var_inc.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_5_gov) +
    geom_line(aes(x = year, y = Gini_1, colour = "Pre-Govt. Income"), linewidth = 1) +
    geom_line(aes(x = year, y = Gini_2, colour = "Pre-Tax Income"), linewidth = 1) +
    labs(title = "Gini of Income", x = "Year", y = "Gini Coefficient") +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("Pre-Govt. Income", "Pre-Tax Income"),
        labels = c("Pre-Govt. Income", "+ Govt. Benefits")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.24, 0.87))
ggsave("figures/Fig_5/Fig_5b_Gini_inc.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_5_gov) +
    geom_line(aes(x = year, y = Var_2, colour = "Pre-Tax Income"), linewidth = 1) +
    geom_line(aes(x = year, y = Var_3, colour = "Disposable Income"), linewidth = 1) +
    labs(title = "Var. of Log Income", x = "Year", y = "Variance of Log") +
    coord_cartesian(ylim = c(0.21, 0.31)) +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("Pre-Tax Income", "Disposable Income"),
        labels = c("Pre-Tax Income", "Disposable Income")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.24, 0.87))
ggsave("figures/Fig_5/Fig_5c_Var_inc.png", dpi = 600, width = 4, height = 4)
ggplot(df_Fig_5_gov) +
    geom_line(aes(x = year, y = Gini_2, colour = "Pre-Tax Income"), linewidth = 1) +
    geom_line(aes(x = year, y = Gini_3, colour = "Disposable Income"), linewidth = 1) +
    labs(title = "Gini of Income", x = "Year", y = "Gini Coefficient") +
    coord_cartesian(ylim = c(0.26, 0.34)) +
    scale_color_manual(
        guide = guide_legend(title = NULL),
        values = colors,
        breaks = c("Pre-Tax Income", "Disposable Income"),
        labels = c("Pre-Tax Income", "Disposable Income")
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.24, 0.87))
ggsave("figures/Fig_5/Fig_5d_Gini_inc.png", dpi = 600, width = 4, height = 4)
