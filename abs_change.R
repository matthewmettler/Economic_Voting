library(dplyr)
library(tibble)

get_abs_change_at_full_acc <- function(df_base, mode = c("pbias", "resid", "both")) {
  mode <- match.arg(mode)
  
  target_acc <- 1
  df_cf <- df_base
  
  if (mode == "resid") {
    df_cf <- df_cf %>%
      mutate(
        max_acc = 1 - PRB_D,
        JA_D = pmin(target_acc, max_acc),
        RR_D = 1 - PRB_D - JA_D
      )
  }
  
  if (mode == "pbias") {
    df_cf <- df_cf %>%
      mutate(
        max_acc = 1 - RR_D,
        JA_D = pmin(target_acc, max_acc),
        PRB_D = 1 - RR_D - JA_D
      )
  }
  
  if (mode == "both") {
    df_cf <- df_cf %>%
      mutate(
        err = PRB_D + RR_D,
        share_pb = ifelse(err > 0, PRB_D / err, 0),
        share_res = ifelse(err > 0, RR_D / err, 1),
        JA_D = target_acc,
        PRB_D = (1 - JA_D) * share_pb,
        RR_D = (1 - JA_D) * share_res
      )
  }
  
  soc_pred_fi <- predict(lm_soc, newdata = df_cf, type = "response")
  
  df_cf2 <- df_base %>%
    mutate(
      soc_bt = soc_pred_fi,
      p_obs = reg,
      p_cf = predict(x0, newdata = mutate(df_base, soc_bt = soc_pred_fi), type = "response"),
      abs_pp_change = abs(p_cf - p_obs) * 100
    )
  
  bind_rows(
    df_cf2 %>%
      summarise(
        group = "All",
        avg_abs_change_pp = weighted.mean(abs_pp_change, weight_w2, na.rm = TRUE)
      ),
    df_cf2 %>%
      group_by(pidf) %>%
      summarise(
        avg_abs_change_pp = weighted.mean(abs_pp_change, weight_w2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(group = pidf)
  ) %>%
    mutate(mode = mode)
}

full_acc_pbias <- get_abs_change_at_full_acc(df_base, "pbias")
full_acc_resid <- get_abs_change_at_full_acc(df_base, "resid")
full_acc_both  <- get_abs_change_at_full_acc(df_base, "both")

full_acc_results <- bind_rows(full_acc_pbias, full_acc_resid, full_acc_both) %>%
  mutate(
    mode = factor(
      mode,
      levels = c("resid", "pbias", "both"),
      labels = c("Residual response", "Partisan response bias", "Both")
    ),
    group = factor(group, levels = c("All", "Dem", "Ind", "Rep"))
  )

full_acc_results
