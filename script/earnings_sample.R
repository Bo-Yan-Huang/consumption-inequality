# Earnings sample, shared by fig_1-2.R and fig_3-5.R
# Author: BoYanHuang
# Created Nov 6, 2026

# Keeps households whose equivalized household earnings are at least `share`
# of that year's weighted median, the median taken over households with
# positive earnings. Expects inc_earnings to be NA wherever it is not
# strictly positive, which both figure scripts set up before calling this.
#
# Why: the variance of log earnings is dominated by a small and growing
# number of households with near-zero earnings, while the Gini on the same
# data barely moves. A floor tied to each year's median removes those
# households without fixing the share dropped in advance. See the Data
# section and the appendix of the paper.
earnings_sample <- function(df, share = 0.05) {
    df %>%
        filter(!is.na(inc_earnings)) %>%
        group_by(year) %>%
        mutate(earn_floor = share * weighted.quantile(inc_earnings, weight, prob = 0.5)) %>%
        ungroup() %>%
        filter(inc_earnings >= earn_floor) %>%
        select(-earn_floor)
}
