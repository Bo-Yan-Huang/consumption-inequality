# Sample floor, shared by every figure script
# Author: BoYanHuang
# Created Nov 6, 2026

# floor_na(x, w) returns x with NA wherever x is not strictly positive or is
# below `share` of the weighted median of its positive values. Call it within
# year, after group_by(year), on one series at a time.
#
# Why: the variance of logs is dominated by a small and growing number of
# households with near-zero values, while the Gini on the same data barely
# moves. A floor tied to each year's median removes those values without
# fixing the share dropped in advance. See the Data section and the appendix
# of the paper.
floor_na <- function(x, w, share = 0.05) {
    pos <- !is.na(x) & x > 0
    if (!any(pos)) return(rep(NA_real_, length(x)))
    x_floor <- share * weighted.quantile(x[pos], w[pos], prob = 0.5)
    ifelse(pos & x >= x_floor, x, NA)
}
