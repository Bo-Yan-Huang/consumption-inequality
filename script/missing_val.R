# show missing values in the dataset
# Author: BoYanHuang
# Created Jun 13, 2025
# Last modify Jun 13, 2025 by BoYanHuang

library(tidyverse)
library(haven)
library(ggplot2)

years <- c(70:112)

df <- tibble()

for(y in years){
    inc_y <- read_dta(paste0("inc//inc", y, ".dta")) %>% as_tibble()

    # we want na count of itm190 and itm240, and nrow count
    na_count <- inc_y %>% summarise(
        itm190_na = sum(is.na(itm190)),
        itm240_na = sum(is.na(itm240)),
        total_rows = n()
    )
    na_count <- na_count %>% mutate(year = 1911 + y)
    na_count <- na_count %>% select(year, everything())
    na_count <- na_count %>% mutate(
        itm190_na_ratio = itm190_na / total_rows,
        itm240_na_ratio = itm240_na / total_rows
    )

    df <- bind_rows(df, na_count)
    print(paste("Processed year:", y))
}

# 2012 has demographics but no income or expenditure data, so its missing
# rate is not comparable; blank it rather than drawing a spurious spike.
df <- df %>% mutate(itm190_na_ratio = ifelse(year == 2012, NA, itm190_na_ratio),
                    itm240_na_ratio = ifelse(year == 2012, NA, itm240_na_ratio))

ggplot(df, aes(x = year, y = itm190_na_ratio)) +
    geom_line(color = "blue") +
    labs(title = "Missing Value Ratio of Labor Earnings", x = "Year", y = "Ratio") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/missing/itm190.png", dpi = 600, width = 5, height = 3)
ggplot(df, aes(x = year, y = itm240_na_ratio)) +
    geom_line(color = "blue") +
    labs(title = "Missing Value Ratio of Industry Earnings", x = "Year", y = "Ratio") +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/missing/itm240.png", dpi = 600, width = 5, height = 3)
