# Analyse Customer Survey Data (Involvement)

## Cleaning customer survey data
library(tidyverse)

# Suburb dimension table
suburbs_dim <- tibble(suburb = 1:3,
                      suburb_name = c("Merton", 
                                      "Tarnstead", 
                                      "Wakefield"))

# Clean data
customers <- read_csv("data/customer_survey.csv")[-1, ] |>
  type_convert() |>
  filter(is.na(term)) |>
  left_join(suburbs_dim) |>
  rename(customer_id = V1) |>
  select(c(1, 52, 21:51, -33))

# Personal Involvement Index
pii <- select(customers, customer_id, starts_with("p")) |>
  mutate(p01 = 8 - p01,
         p02 = 8 - p02,
         p07 = 8 - p07,
         p08 = 8 - p08,
         p09 = 8 - p09,
         p10 = 8 - p10) 

pii_long <- pii[complete.cases(pii_raw), ] |>
  pivot_longer(-customer_id, names_to = "Item", values_to = "Response")

# Visualise PII
ggplot(pii_long) +
  aes(Item, Response) +
  geom_boxplot(fill = "lightgrey") +
  theme_minimal(base_size = 40) +
  labs(caption = paste("n =", nrow(pii) / 10))

# Correlation between items
pii_cor <- round(cor(pii_raw[, -1], use = "complete.obs"), 2)

library(ggcorrplot)

ggcorrplot(pii_cor, show.diag = FALSE,
           type = "lower",
           outline.color = "white",
           show.legend = FALSE,
           lab = TRUE) +
  labs(x = NULL, y = NULL) + 
  theme_minimal(base_size = 26) +
  theme(legend.position = "bottom")

# Reliability (Cronbach's Alpha)

cronback_alpha <- function(items) {
  cov <- cov(items, use = "complete.obs")
  k <- ncol(cov)
  v <- mean(diag(cov))
  c <- mean(pii_cov[lower.tri(cov)])
  (k * c) / (v + (k - 1) * c) 
}

# Whole PII construct
cronback_alpha(pii_raw[, -1])

# Subconstructs
cronback_alpha(pii_raw[, 2:6])
cronback_alpha(pii_raw[, 7:11])

library(psych)
pii_fa <- fa(pii[, -1], nfactors = 2, rotate = "oblimin", fm = "ml")
fa.diagram(pii_fa, main = NULL)

library(tidyr)
pii_scores <- pii %>%
  mutate(cognitive = p01 + p02 + p03 + p04 + p05,
         affective = p06 + p07 + p08 + p09 + p10) %>%
  select(customer_id, cognitive, affective)

pivot_longer(pii_scores, cols = -customer_id) %>%
  ggplot(aes(value)) +
  geom_histogram(fill = "dodgerblue 4") +
  facet_wrap(~name) +
  theme_minimal(base_size = 12)
