library(magrittr)

metadata_train <- read.csv(file = "data/train.csv", header = T,
                           stringsAsFactors = F) %>%
  tibble::as_tibble()


# Right-skewed with Heavy tails
metadata_train$Pawpularity %>% hist()

metadata_train[, sapply(metadata_train, is.numeric)] %>%
  summary

metadata_train[, sapply(metadata_train, is.numeric)] %>%
  dplyr::glimpse() %>%
  purrr::map(table)

# Table of the variables
metadata_train %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-Pawpularity) %>%
  tidyr::pivot_longer(cols = tidyselect::everything(),
                      names_to = "variable") %>%
  dplyr::group_by(variable, value) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  tidyr::pivot_wider(names_from = value, values_from = n)

# Transformation of the variable
metadata_train %<>%
  dplyr::mutate(dplyr::across(
    .cols = -c(Pawpularity, Id),
    .fns = factor, levels = c(0, 1), labels = c("missing", "present"))
  )

# Linear model for HT
form <- setdiff(names(metadata_train), c("Id", "Pawpularity")) %>%
  paste(collapse = "+") %>%
  paste("Pawpularity", ., sep = "~") %>%
  formula()

mod_full_lm <- lm(form, data = metadata_train)

summary(mod_full_lm)
anova(mod_full_lm)

plot(mod_full_lm)


