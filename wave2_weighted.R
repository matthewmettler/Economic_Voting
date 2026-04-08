
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(scales)
source("/Users/mattmettler/Box Sync/Data_2024/EV/w2_data.R")

df0 <- w2 %>%
  mutate(
    pidf = case_when(
      pidf %in% c("5", "6", "7") ~ "Rep",
      pidf %in% c("1", "2", "3") ~ "Dem",
      pidf %in% c("4")           ~ "Ind",
      #pidf == "8"                ~ "Other",
      TRUE                       ~ NA_character_
    )
  ) %>%
  filter(!is.na(weight_w2), weight_w2 > 0)

# first-stage complete-case sample
df_soc <- df0 %>%
  filter(
    complete.cases(
      soc_bt, pidf, econ_ptbias, econ_resid,
      age, black, hispanic, female, weight_w2
    )
  )

des_w2_full <- survey::svydesign(
  ids = ~1,
  weights = ~weight_w2,
  data = df_soc
)

lm_soc <- survey::svyglm(
  soc_bt ~ pidf * (econ_ptbias + econ_resid) + age + black + hispanic + female,
  design = des_w2_full
)

# vote model complete-case sample
df_vote <- df_soc %>%
  filter(
    VOTE2_w2 < 3,
    complete.cases(vbiden, soc_bt, pidf, age, black, hispanic, female, weight_w2)
  )

des_w2_vote <- survey::svydesign(
  ids = ~1,
  weights = ~weight_w2,
  data = df_vote
)

x0 <- survey::svyglm(
  vbiden ~ soc_bt * pidf + age + black + hispanic + female,
  design = des_w2_vote,
  family = quasibinomial()
)

df_vote$reg <- predict(x0, newdata = df_vote, type = "response")
df_base <- df_vote

run_recovery_cf <- function(df_base, acc_seq, mode = c("pbias", "resid", "both"), weight_var) {
  mode <- match.arg(mode)
  v <- rep(NA_real_, length(acc_seq))
  
  for (i in seq_along(acc_seq)) {
    
    target_acc <- acc_seq[i]
    df_cf <- df_base
    
    if (mode == "resid") {
      df_cf <- df_cf %>%
        mutate(
          max_acc = 1 - econ_ptbias,
          econ_acc = pmin(target_acc, max_acc),
          econ_resid = 1 - econ_ptbias - econ_acc
        )
    }
    
    if (mode == "pbias") {
      df_cf <- df_cf %>%
        mutate(
          max_acc = 1 - econ_resid,
          econ_acc = pmin(target_acc, max_acc),
          econ_ptbias = 1 - econ_resid - econ_acc
        )
    }
    
    if (mode == "both") {
      df_cf <- df_cf %>%
        mutate(
          err = econ_ptbias + econ_resid,
          share_pb = ifelse(err > 0, econ_ptbias / err, 0),
          share_res = ifelse(err > 0, econ_resid / err, 1),
          econ_acc = target_acc,
          econ_ptbias = (1 - econ_acc) * share_pb,
          econ_resid = (1 - econ_acc) * share_res
        )
    }
    
    soc_pred_fi <- predict(lm_soc, newdata = df_cf, type = "response")
    
    df_cf2 <- df_base %>%
      mutate(soc_bt = soc_pred_fi)
    
    df_cf2$nobias_fi <- predict(x0, newdata = df_cf2, type = "response")
    
    flip_df <- df_cf2 %>%
      mutate(
        vote_reg = as.integer(df_base$reg >= 0.5),
        vote_cf  = as.integer(nobias_fi >= 0.5),
        flip_to_biden   = as.integer(vote_reg == 0 & vote_cf == 1),
        flip_from_biden = as.integer(vote_reg == 1 & vote_cf == 0)
      )
    
    #w <- flip_df[[weight_var]]
    
    #keep <- complete.cases(flip_df$vote_reg, flip_df$vote_cf, w)
    
    obs_biden_count <- sum(flip_df$weight_w2 * flip_df$vote_reg, na.rm = TRUE)
    cf_biden_count  <- sum(flip_df$weight_w2 * flip_df$vote_cf,  na.rm = TRUE)
    total_weighted_n <- sum(flip_df$weight_w2, na.rm = TRUE)
    
    v[i] <- (cf_biden_count - obs_biden_count) / total_weighted_n
  }
  
  tibble(
    target_acc = acc_seq,
    delta_vote_share = v,
    mode = mode
  )
}

x <- seq(0.00, 1.00, by = 0.01)

out_pbias <- run_recovery_cf(df_base, x, mode = "pbias", weight_var = "weight_w2")
out_resid <- run_recovery_cf(df_base, x, mode = "resid", weight_var = "weight_w2")
out_both  <- run_recovery_cf(df_base, x, mode = "both", weight_var = "weight_w2")

out_all_w2 <- bind_rows(out_pbias, out_resid, out_both) %>%
  mutate(
    mode = factor(
      mode,
      levels = c("resid", "pbias", "both"),
      labels = c("Residual response", "Partisan response bias", "Both")
    )
  )

# =========================================================
# Threshold lines and labels
# =========================================================

hdf_red <- data.frame(
  y   = c(0.00437, 0.0072, 0.0111, 0.0163, 0.0279),
  lab = c("WI","MI","PA-GA","NC-NV","AZ"),
  grp = "red"
)

hdf_blue <- data.frame(
  y   = c(-0.0278, -0.0424, -0.0591, -0.0694),
  lab = c("NH", "MN", "VA-NJ-NM","ME"),
  grp = "blue"
)

hdf_all <- bind_rows(hdf_red, hdf_blue)

hdf_lab <- hdf_all %>%
  mutate(
    y_text = y + case_when(
      lab %in% c("NJ", "NM") ~  0.0007,
      lab %in% c("VA", "NJ") ~ -0.0007,
      TRUE ~ 0
    )
  )

df_line <- out_all_w2 %>%
  filter(mode == "Both") %>%
  arrange(target_acc)

get_cross_x <- function(y0, df = df_line) {
  d <- df$delta_vote_share - y0
  idx <- which(d[-1] * d[-length(d)] <= 0)
  if (length(idx) == 0) return(NA_real_)
  i <- idx[1]
  x1 <- df$target_acc[i]
  x2 <- df$target_acc[i + 1]
  y1 <- df$delta_vote_share[i]
  y2 <- df$delta_vote_share[i + 1]
  if (isTRUE(all.equal(y1, y2))) return(x1)
  x1 + (y0 - y1) * (x2 - x1) / (y2 - y1)
}

x_left  <- 0
x_right <- 1

df_cross <- hdf_all %>%
  mutate(
    x_cross = purrr::map_dbl(y, get_cross_x),
    x_cross = pmin(pmax(x_cross, x_left), x_right)
  ) %>%
  filter(!is.na(x_cross))

seg_left <- df_cross %>%
  transmute(x = x_left, xend = x_cross, y = y, yend = y)

seg_right <- df_cross %>%
  transmute(x = x_cross, xend = x_right, y = y, yend = y)

# -------------------------
# 6) Plot
# -------------------------
bt <- ggplot(
  out_all_w2,
  aes(x = target_acc, y = delta_vote_share, color = mode, linetype = mode)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_segment(
    data = seg_left,
    aes(x = x, xend = xend, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.5,
    linetype = "dotted",
    color = "red"
  ) +
  geom_segment(
    data = seg_right,
    aes(x = x, xend = xend, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.5,
    linetype = "dotted",
    color = "blue"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = -0.0591, yend = -0.0591),
    inherit.aes = FALSE,
    linewidth = 0.5,
    linetype = "dotted",
    color = "blue"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = -0.0424, yend = -0.0424),
    inherit.aes = FALSE,
    linewidth = 0.5,
    linetype = "dotted",
    color = "blue"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = -0.0694, yend = -0.0694),
    inherit.aes = FALSE,
    linewidth = 0.5,
    linetype = "dotted",
    color = "blue"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = 0.0557, yend = 0.0557),
    inherit.aes = FALSE,
    linewidth = 0.5,
    linetype = "dotted",
    color = "red"
  ) +
  geom_line(linewidth = 1.8, alpha = 1) +
  scale_color_manual(
    breaks = c("Residual response", "Partisan response bias", "Both"),
    values = c(
      "Residual response" = "#FF5F05",
      "Partisan response bias" = "#4877B8",
      "Both" = "#13294B"
    )
  ) +
  scale_linetype_manual(
    breaks = c("Residual response", "Partisan response bias", "Both"),
    values = c(
      "Residual response" = "twodash",
      "Partisan response bias" = "longdash",
      "Both" = "solid"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::number_format(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    limits = c(-0.07, 0.07),
    breaks = seq(-0.07, 0.07, by = 0.01),
    labels = scales::percent_format(accuracy = 0.01)
  ) +
  geom_text(
    data = hdf_lab,
    aes(x = 1.02, y = y_text, label = lab),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0,
    color = "blue",
    fontface = "bold",
    size = 2.8
  ) +
  geom_text(
    aes(x = 1.02, y = 0.0557, label = "OH"),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0,
    color = "blue",
    fontface = "bold",
    size = 2.8
  ) +
  ggtitle("A. Biden, continuous measure") +
  coord_cartesian(xlim = c(0, 1.08), clip = "off") +
  theme_classic(base_size = 13) +
  labs(
    x = "Judgmental accuracy",
    y = "Change in Biden vote share",
    color = NULL,
    linetype = NULL
  ) 


bt





df0 <- w2 %>%
  mutate(
    pidf = case_when(
      pidf %in% c("5", "6", "7") ~ "Rep",
      pidf %in% c("1", "2", "3") ~ "Dem",
      pidf %in% c("4")           ~ "Ind",
      #pidf == "8"                ~ "Other",
      TRUE                       ~ NA_character_
    )
  ) %>%
  filter(!is.na(weight_w2), weight_w2 > 0)

# first-stage complete-case sample
df_soc <- df0 %>%
  filter(
    complete.cases(
      soc_bt, pidf, PRB_D, RR_D,
      age, black, hispanic, female, weight_w2
    )
  )

# vote sample
df_vote <- df_soc %>%
  filter(VOTE2_w2 < 3) %>%
  mutate(
    vbiden = ifelse(VOTE2_w2 == 1, 1, 0)
  ) %>%
  filter(
    complete.cases(
      vbiden, soc_bt, pidf,
      age, black, hispanic, female, weight_w2
    )
  )

# -------------------------
# 2) Survey-weighted models
# -------------------------
des_soc <- svydesign(
  ids = ~1,
  weights = ~weight_w2,
  data = df_soc
)

m_soc <- svyglm(
  soc_bt ~ pidf * (PRB_D + RR_D) + age + black + hispanic + female,
  design = des_soc
)

des_vote <- svydesign(
  ids = ~1,
  weights = ~weight_w2,
  data = df_vote
)

m_vote <- svyglm(
  vbiden ~ soc_bt * pidf + age + black + hispanic + female,
  design = des_vote,
  family = quasibinomial()
)

# observed predicted probabilities
df_vote$reg <- as.numeric(predict(m_vote, newdata = df_vote, type = "response"))
df_base <- df_vote

# -------------------------
# 3) Counterfactual function
# -------------------------
run_cf_w2 <- function(df_base, acc_seq, mode = c("pbias", "resid", "both"), weight_var = "weight_w2") {
  mode <- match.arg(mode)
  v <- rep(NA_real_, length(acc_seq))
  
  for (i in seq_along(acc_seq)) {
    target_acc <- acc_seq[i]
    df_cf <- df_base
    
    if (mode == "resid") {
      # hold partisan bias fixed; recover accuracy from residual
      df_cf <- df_cf %>%
        mutate(
          max_acc = 1 - PRB_D,
          JA_D = pmin(target_acc, max_acc),
          RR_D = 1 - PRB_D - JA_D
        )
    }
    
    if (mode == "pbias") {
      # hold residual fixed; recover accuracy from partisan bias
      df_cf <- df_cf %>%
        mutate(
          max_acc = 1 - RR_D,
          JA_D = pmin(target_acc, max_acc),
          PRB_D = 1 - RR_D - JA_D
        )
    }
    
    if (mode == "both") {
      # preserve observed mix of error, shrink both as accuracy rises
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
    
    soc_pred <- as.numeric(predict(m_soc, newdata = df_cf, type = "response"))
    
    df_cf2 <- df_base %>%
      mutate(soc_bt = soc_pred)
    
    df_cf2$pred_cf <- as.numeric(predict(m_vote, newdata = df_cf2, type = "response"))
    
    flip_df <- df_cf2 %>%
      mutate(
        vote_reg = as.integer(df_base$reg >= 0.5),
        vote_cf  = as.integer(pred_cf >= 0.5),
        flip_to_biden   = as.integer(vote_reg == 0 & vote_cf == 1),
        flip_from_biden = as.integer(vote_reg == 1 & vote_cf == 0)
      )
    
    #w <- flip_df[[weight_var]]
    
    #keep <- complete.cases(flip_df$vote_reg, flip_df$vote_cf, w)
    
    obs_biden_count <- sum(flip_df$weight_w2 * flip_df$vote_reg, na.rm = TRUE)
    cf_biden_count  <- sum(flip_df$weight_w2  * flip_df$vote_cf,  na.rm = TRUE)
    total_weighted_n <- sum(flip_df$weight_w2 , na.rm = TRUE)
    
    v[i] <- (cf_biden_count - obs_biden_count) / total_weighted_n
  }
  
  tibble(
    target_acc = acc_seq,
    delta_vote_share = v,
    mode = mode
  )
}

# -------------------------
# 4) Run all three curves
# -------------------------
x <- c(0, .5, 1)

out_pbias <- run_cf_w2(df_base, x, mode = "pbias")
out_resid <- run_cf_w2(df_base, x, mode = "resid")
out_both  <- run_cf_w2(df_base, x, mode = "both")

out_all_w2 <- bind_rows(out_pbias, out_resid, out_both) %>%
  mutate(
    mode = factor(
      mode,
      levels = c("resid", "pbias", "both"),
      labels = c("Residual response", "Partisan response bias", "Both")
    )
  )

# -------------------------
# 5) Threshold lines / labels
# red = Trump-won states Biden could flip
# blue = Biden-won states Trump could flip
# replace these with your Wave 2 thresholds if needed
# -------------------------
hdf_red <- data.frame(
  y   = c(0.00437, 0.0072, 0.0111, 0.0163),
  lab = c("WI","MI","PA-GA","NC-NV"),
  grp = "red"
)

hdf_blue <- data.frame(
  y   = c(-0.0278, -0.0424, -0.0591, -0.0694),
  lab = c("NH", "MN", "VA-NJ-NM","ME"),
  grp = "blue"
)

hdf_all <- bind_rows(hdf_red, hdf_blue)

hdf_lab <- hdf_all %>%
  mutate(y_text = y)

# IMPORTANT: use the SAME data object you are plotting
df_line <- out_all_w2 

get_cross_x <- function(y0, df = df_line) {
  d <- df$delta_vote_share - y0
  idx <- which(d[-1] * d[-length(d)] <= 0)
  
  if (length(idx) == 0) return(NA_real_)
  
  i <- idx[1]
  x1 <- df$target_acc[i]
  x2 <- df$target_acc[i + 1]
  y1 <- df$delta_vote_share[i]
  y2 <- df$delta_vote_share[i + 1]
  
  if (isTRUE(all.equal(y1, y2))) return(x1)
  
  x1 + (y0 - y1) * (x2 - x1) / (y2 - y1)
}

x_left  <- 0
x_right <- 1

df_cross <- hdf_all %>%
  mutate(
    x_cross = purrr::map_dbl(y, get_cross_x),
    x_cross = pmin(pmax(x_cross, x_left), x_right)
  )

seg_left <- df_cross %>%
  filter(!is.na(x_cross)) %>%
  transmute(x = x_left, xend = x_cross, y = y, yend = y)

seg_right <- df_cross %>%
  filter(!is.na(x_cross)) %>%
  transmute(x = x_cross, xend = x_right, y = y, yend = y)

bt_d <- out_all_w2 %>%
  #filter(mode == "Any") %>%
  ggplot(aes(x = target_acc, y = delta_vote_share, color = mode,linetype=mode)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_segment(
    data = seg_left,
    aes(x = x, xend = xend, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = .5,
    linetype = "dotted",
    color = "red"
  ) +
  geom_segment(
    data = seg_right,
    aes(x = x, xend = xend, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = .5,
    linetype = "dotted",
    color = "blue"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = 0.0279, yend = 0.0279),
    inherit.aes = FALSE,
    linewidth = .5,
    linetype = "dotted",
    color = "red"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = -0.0424, yend = -0.0424),
    inherit.aes = FALSE,
    linewidth = 0.5,
    linetype = "dotted",
    color = "blue"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = -0.0591, yend = -0.0591),
    inherit.aes = FALSE,
    linewidth = .5,
    linetype = "dotted",
    color = "blue"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = -0.0694, yend = -0.0694),
    inherit.aes = FALSE,
    linewidth = .5,
    linetype = "dotted",
    color = "blue"
  ) + 
  geom_segment(
    aes(x = 0, xend = 1, y = 0.0557, yend = 0.0557),
    inherit.aes = FALSE,
    linewidth = .5,
    linetype = "dotted",
    color = "red"
  ) + 
  geom_segment(
    aes(x = 0, xend = 1, y = 0.0279, yend = 0.0279),
    inherit.aes = FALSE,
    linewidth = .5,
    linetype = "dotted",
    color = "red"
  ) +   
  geom_line(linewidth = 1.8, alpha = 1) +
  scale_color_manual(
    breaks = c("Residual response", "Partisan response bias", "Both"),
    values = c(
      "Residual response" = "#FF5F05",
      "Partisan response bias" = "#4877B8",
      "Both" = "#13294B"
    )
  ) +
  scale_linetype_manual(
    breaks = c("Residual response", "Partisan response bias", "Both"),
    values = c(
      "Residual response" = "twodash",
      "Partisan response bias" = "longdash",
      "Both" = "solid"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::number_format(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    limits = c(-0.07, 0.07),
    breaks = seq(-0.07, 0.07, by = 0.01),
    labels = scales::percent_format(accuracy = 0.01)
  ) +
  geom_text(
    data = hdf_lab,
    aes(x = 1.02, y = y_text, label = lab),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0,
    color = "blue",
    fontface = "bold",
    size = 2.8
  ) +
  geom_text(
    aes(x = 1.02, y = 0.0557, label = "OH"),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0,
    color = "red",
    fontface = "bold",
    size = 2.8
  ) +
  geom_text(
    aes(x = 1.02, y = 0.0279, label = "AZ"),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0,
    color = "red",
    fontface = "bold",
    size = 2.8
  ) +
  ggtitle("B. Biden, discrete measure") +
  coord_cartesian(xlim = c(0, 1.08), clip = "off") +
  theme_classic(base_size = 13) +
  labs(
    x = "Judgmental accuracy",
    y = "Change in Biden vote share",
    color = NULL
  )
bt_d


ggarrange(bt,bt_d,ht,ht_d,ncol=2,nrow=2,legend="bottom",common.legend = TRUE)

ggsave("results.png",width=18,height=12,dpi=200)
