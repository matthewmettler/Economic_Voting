library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

# =========================================================
# Prepare data
# =========================================================
df0 <- w2 %>%
  mutate(
    pidf = case_when(
      pidf %in% c("5", "6", "7") ~ "Rep",
      pidf %in% c("1", "2", "3") ~ "Dem",
      pidf == "4"                ~ "Other",
      pidf == "8"                ~ NA_character_,
      TRUE                       ~ NA_character_
    )
  )

d0 <- df0 %>%
  filter(
    !is.na(pidf),
    !is.na(soc_bt),
    !is.na(econ_ptbias),
    !is.na(econ_resid),
    !is.na(PRB_D),
    !is.na(RR_D),
    !is.na(affpold),
    !is.na(affpolr)
  ) %>%
  mutate(
    pidf = factor(pidf, levels = c("Dem", "Rep", "Other"))
  )

# =========================================================
# PANEL A: Continuous measure
# =========================================================
lm_soc_cont <- lm(
  soc_bt ~ pidf * (econ_ptbias + econ_resid) + affpold + affpolr,
  data = d0,
  na.action = na.exclude
)

grp_cont <- d0 %>%
  group_by(pidf) %>%
  summarise(
    n = n(),
    soc_bt_obs_mean  = mean(soc_bt, na.rm = TRUE),
    econ_ptbias_mean = mean(econ_ptbias, na.rm = TRUE),
    econ_resid_mean  = mean(econ_resid, na.rm = TRUE),
    affpold_mean     = mean(affpold, na.rm = TRUE),
    affpolr_mean     = mean(affpolr, na.rm = TRUE),
    .groups = "drop"
  )

base_pred_cont <- tibble(
  pidf = factor(c("Dem", "Rep", "Other"), levels = c("Dem", "Rep", "Other")),
  affpold = c(grp_cont$affpold_mean[grp_cont$pidf == "Dem"], 0, 0),
  affpolr = c(0, grp_cont$affpolr_mean[grp_cont$pidf == "Rep"], 0)
)

# Observed
new_obs_cont <- base_pred_cont %>%
  mutate(
    econ_ptbias = grp_cont$econ_ptbias_mean[match(pidf, grp_cont$pidf)],
    econ_resid  = grp_cont$econ_resid_mean[match(pidf, grp_cont$pidf)]
  )

new_obs_cont$pred_obs_mean <- predict(lm_soc_cont, newdata = new_obs_cont)

# No judgmental accuracy: set econ_acc = 0 and preserve pbias/resid ratio
new_cf_noacc_cont <- base_pred_cont %>%
  mutate(
    err_total = grp_cont$econ_ptbias_mean[match(pidf, grp_cont$pidf)] +
      grp_cont$econ_resid_mean[match(pidf, grp_cont$pidf)],
    share_pb = ifelse(
      err_total > 0,
      grp_cont$econ_ptbias_mean[match(pidf, grp_cont$pidf)] / err_total,
      0
    ),
    share_res = ifelse(
      err_total > 0,
      grp_cont$econ_resid_mean[match(pidf, grp_cont$pidf)] / err_total,
      1
    ),
    econ_ptbias = share_pb,
    econ_resid  = share_res
  )

new_cf_noacc_cont$pred_noacc_mean <- predict(lm_soc_cont, newdata = new_cf_noacc_cont)

# No residual response
new_cf_resid0_cont <- base_pred_cont %>%
  mutate(
    econ_ptbias = grp_cont$econ_ptbias_mean[match(pidf, grp_cont$pidf)],
    econ_resid  = 0
  )

new_cf_resid0_cont$pred_resid0_mean <- predict(lm_soc_cont, newdata = new_cf_resid0_cont)

# No partisan response bias
new_cf_pbias0_cont <- base_pred_cont %>%
  mutate(
    econ_ptbias = 0,
    econ_resid  = grp_cont$econ_resid_mean[match(pidf, grp_cont$pidf)]
  )

new_cf_pbias0_cont$pred_pbias0_mean <- predict(lm_soc_cont, newdata = new_cf_pbias0_cont)

# No error
new_cf_noerror_cont <- base_pred_cont %>%
  mutate(
    econ_ptbias = 0,
    econ_resid  = 0
  )

new_cf_noerror_cont$pred_noerror_mean <- predict(lm_soc_cont, newdata = new_cf_noerror_cont)

out_cont <- grp_cont %>%
  left_join(new_obs_cont %>% dplyr::select(pidf, pred_obs_mean), by = "pidf") %>%
  left_join(new_cf_noacc_cont %>% dplyr::select(pidf, pred_noacc_mean), by = "pidf") %>%
  left_join(new_cf_resid0_cont %>% dplyr::select(pidf, pred_resid0_mean), by = "pidf") %>%
  left_join(new_cf_pbias0_cont %>% dplyr::select(pidf, pred_pbias0_mean), by = "pidf") %>%
  left_join(new_cf_noerror_cont %>% dplyr::select(pidf, pred_noerror_mean), by = "pidf")

out1_cont <- out_cont[, c(1, 2, 8, 9, 10, 11, 12)]

colnames(out1_cont) <- c(
  "pidf", "n", "observed",
  "No judgmental accuracy",
  "No residual response",
  "No partisan response bias",
  "No error"
)

out1_long_cont <- out1_cont %>%
  pivot_longer(
    cols = c(
      observed,
      `No judgmental accuracy`,
      `No residual response`,
      `No partisan response bias`,
      `No error`
    ),
    names_to = "scenario",
    values_to = "value"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c(
        "No judgmental accuracy",
        "observed",
        "No partisan response bias",
        "No residual response",
        "No error"
      )
    ),
    pidf = ifelse(pidf == "Other", "Independents",
                  ifelse(pidf == "Rep", "Republicans", "Democrats")),
    pidf = factor(pidf, levels = c("Democrats", "Republicans", "Independents"))
  )

A <- ggplot(out1_long_cont, aes(x = scenario, y = value, fill = pidf)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(
    values = c(
      "Democrats" = "#2171B5",
      "Republicans" = "#CB181D",
      "Independents" = "#969696"
    )
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +
  theme_classic(base_size = 13) +
  labs(
    x = NULL,
    y = "Sociotropic perception (Biden - Trump)",
    fill = NULL
  ) +
  ggtitle("A. Continuous measure") +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

# =========================================================
# PANEL B: Discrete measure
# =========================================================
lm_soc_disc <- lm(
  soc_bt ~ pidf * (PRB_D + RR_D) + affpold + affpolr,
  data = d0,
  na.action = na.exclude
)

grp_disc <- d0 %>%
  group_by(pidf) %>%
  summarise(
    n = n(),
    soc_bt_obs_mean = mean(soc_bt, na.rm = TRUE),
    PRB_D_mean      = mean(PRB_D, na.rm = TRUE),
    RR_D_mean       = mean(RR_D, na.rm = TRUE),
    affpold_mean    = mean(affpold, na.rm = TRUE),
    affpolr_mean    = mean(affpolr, na.rm = TRUE),
    .groups = "drop"
  )

base_pred_disc <- tibble(
  pidf = factor(c("Dem", "Rep", "Other"), levels = c("Dem", "Rep", "Other")),
  affpold = c(grp_disc$affpold_mean[grp_disc$pidf == "Dem"], 0, 0),
  affpolr = c(0, grp_disc$affpolr_mean[grp_disc$pidf == "Rep"], 0)
)

# Observed
new_obs_disc <- base_pred_disc %>%
  mutate(
    PRB_D = grp_disc$PRB_D_mean[match(pidf, grp_disc$pidf)],
    RR_D  = grp_disc$RR_D_mean[match(pidf, grp_disc$pidf)]
  )

new_obs_disc$pred_obs_mean <- predict(lm_soc_disc, newdata = new_obs_disc)

# No judgmental accuracy: set JA_D = 0 and preserve PRB_D/RR_D ratio
new_cf_noacc_disc <- base_pred_disc %>%
  mutate(
    err_total = grp_disc$PRB_D_mean[match(pidf, grp_disc$pidf)] +
      grp_disc$RR_D_mean[match(pidf, grp_disc$pidf)],
    share_pb = ifelse(
      err_total > 0,
      grp_disc$PRB_D_mean[match(pidf, grp_disc$pidf)] / err_total,
      0
    ),
    share_res = ifelse(
      err_total > 0,
      grp_disc$RR_D_mean[match(pidf, grp_disc$pidf)] / err_total,
      1
    ),
    PRB_D = share_pb,
    RR_D  = share_res
  )

new_cf_noacc_disc$pred_noacc_mean <- predict(lm_soc_disc, newdata = new_cf_noacc_disc)

# No residual response
new_cf_resid0_disc <- base_pred_disc %>%
  mutate(
    PRB_D = grp_disc$PRB_D_mean[match(pidf, grp_disc$pidf)],
    RR_D  = 0
  )

new_cf_resid0_disc$pred_resid0_mean <- predict(lm_soc_disc, newdata = new_cf_resid0_disc)

# No partisan response bias
new_cf_pbias0_disc <- base_pred_disc %>%
  mutate(
    PRB_D = 0,
    RR_D  = grp_disc$RR_D_mean[match(pidf, grp_disc$pidf)]
  )

new_cf_pbias0_disc$pred_pbias0_mean <- predict(lm_soc_disc, newdata = new_cf_pbias0_disc)

# No error
new_cf_noerror_disc <- base_pred_disc %>%
  mutate(
    PRB_D = 0,
    RR_D  = 0
  )

new_cf_noerror_disc$pred_noerror_mean <- predict(lm_soc_disc, newdata = new_cf_noerror_disc)

out_disc <- grp_disc %>%
  left_join(new_obs_disc %>% dplyr::select(pidf, pred_obs_mean), by = "pidf") %>%
  left_join(new_cf_noacc_disc %>% dplyr::select(pidf, pred_noacc_mean), by = "pidf") %>%
  left_join(new_cf_resid0_disc %>% dplyr::select(pidf, pred_resid0_mean), by = "pidf") %>%
  left_join(new_cf_pbias0_disc %>% dplyr::select(pidf, pred_pbias0_mean), by = "pidf") %>%
  left_join(new_cf_noerror_disc %>% dplyr::select(pidf, pred_noerror_mean), by = "pidf")

out1_disc <- out_disc[, c(1, 2, 8, 9, 10, 11, 12)]

colnames(out1_disc) <- c(
  "pidf", "n", "observed",
  "No judgmental accuracy",
  "No residual response",
  "No partisan response bias",
  "No error"
)

out1_long_disc <- out1_disc %>%
  pivot_longer(
    cols = c(
      observed,
      `No judgmental accuracy`,
      `No residual response`,
      `No partisan response bias`,
      `No error`
    ),
    names_to = "scenario",
    values_to = "value"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c(
        "No judgmental accuracy",
        "observed",
        "No partisan response bias",
        "No residual response",
        "No error"
      )
    ),
    pidf = ifelse(pidf == "Other", "Independents",
                  ifelse(pidf == "Rep", "Republicans", "Democrats")),
    pidf = factor(pidf, levels = c("Democrats", "Republicans", "Independents"))
  )

B <- ggplot(out1_long_disc, aes(x = scenario, y = value, fill = pidf)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(
    values = c(
      "Democrats" = "#2171B5",
      "Republicans" = "#CB181D",
      "Independents" = "#969696"
    )
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +
  theme_classic(base_size = 13) +
  labs(
    x = NULL,
    y = "Sociotropic perception (Biden - Trump)",
    fill = NULL
  ) +
  ggtitle("B. Discrete measure") +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

# =========================================================
# Show plots
# =========================================================
A
B

ggarrange(A,B,ncol = 1,nrow=2,legend="bottom",common.legend = TRUE)
ggsave("two_panel.png",width = 8,height=8,dpi=200)
