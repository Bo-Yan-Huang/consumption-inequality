# Colors and background for every figure, defined once
# Author: BoYanHuang
# Created Nov 10, 2026

# The Okabe-Ito palette (Okabe & Ito 2008; Wong 2011, Nature Methods 8:441),
# designed to stay distinguishable under the common forms of color blindness.
# Base R ships it as palette.colors(palette = "Okabe-Ito"). Its yellow is
# left out because it is too faint to read as a line.
# Every set below was checked for protanopia and deuteranopia: any two lines
# that can touch stay distinguishable.

# Series colors, used in this order.
pal_1 <- "#0072B2"    # blue; also every single-line figure
pal_2 <- "#D55E00"    # vermillion
pal_3 <- "#009E73"    # bluish green
pal_ink <- "#000000"  # black, for a baseline the other lines are compared with
pal_rule <- "grey40"  # grey, for a guide such as a zero line, which is not a series

# Percentiles: seven distinct colors, ordered so that neighboring percentiles
# differ as much as possible, with the median in black.
pal_pct <- c("P5"  = "#D55E00",   # vermillion
             "P10" = "#0072B2",   # blue
             "P25" = "#009E73",   # bluish green
             "P50" = "#000000",   # black
             "P75" = "#56B4E9",   # sky blue
             "P90" = "#E69F00",   # orange
             "P95" = "#CC79A7")   # reddish purple

# White panel with light gridlines instead of ggplot's default grey panel. The
# legends sit inside the plot area, so they get a thin border to separate them
# from the lines behind. Scripts only add the title and legend position.
theme_set(theme_bw() +
          theme(legend.background = element_rect(fill = "white", colour = "grey70",
                                                 linewidth = 0.3)))
