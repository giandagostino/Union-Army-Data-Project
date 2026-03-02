###############################################################################

# Matching

match_full <- surv_tb |>
  select(
    recidnum, TB_yn, BYR, initstat, DYR, 
  ) |>
  filter(
    if_all(where(is.numeric), is.finite)
  ) |>
  drop_na() |>
  mutate(
    BYR = round(BYR, 5)
  )

sum(is.na(match_full$BYR))
sum(!is.finite(match_full$BYR))

match_full <- matchit(TB_yn ~ BYR + initstat,
                      data = match_full,
                      distance = "probit")

match_df <- match.data(match_full)

# Add constant status for lack of censoring
match_df$status <- 1

# Create index date for non-TB
matched_df <- matched_df |>
  group_by(subclass) |>
  mutate(indexdate)


match_df$survival_time <- as.numeric

km_fit <- survfit(Surv())


################################################################################












