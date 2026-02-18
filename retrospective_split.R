rm(list=ls())

library(haven)
library(tidyverse)
library(nnet)
library(stargazer)
library(estimatr)
library(zCompositions)
library(compositions)

source("/Users/mattmettler/Box Sync/Data_2024/EV/w2_data.R")

#---- workflow as follows ------------------------------------------------------
# 1. fit model for Biden and Trump economic perceptions, soc_b & soc_t = 0-1
# 2. Create simulation where respondents are "near fully accurate on economic facts"
# 3. Predict Soc_bt based on simulation
# 4. fit voting logit model with empirical soc_bt then fit with simulated soc_bt
# 5. Plot empirical vs simulation of "fully informed"
# 6. 1-5 again but partisan bias removed
# 7. 1-5 again but residual error removed

#-------------------------------------------------------------------------------
df <- w2

########
# 1.   
########
lm_soc_b <- lm(
  soc_b ~ (econ_ptbias + econ_resid) * pidf + age + female + black + hispanic,
  data = df
)
lm_soc_t <- lm(
  soc_t ~ (econ_ptbias + econ_resid) * pidf + age + female + black + hispanic,
  data = df
)

summary(lm_soc_b)
summary(lm_soc_t)

########
# 2.   
########
target_acc <- as.numeric(quantile(df$econ_acc, 0.95, na.rm = TRUE))
econ_acc = pmin(pmax(df$econ_acc, target_acc), 0.95)

df_cf <- df %>%
  mutate(
    err = 1 - econ_acc,
    w_p = ifelse(err > 0, econ_ptbias / err, 0),
    w_r = ifelse(err > 0, econ_resid  / err, 1),
    
    # raise accuracy up to target_acc (do not lower those already above)
    # accuracy rests between 95% and 100% quantile. Errors are recomputed 
    # with respondents original weighting
    econ_acc = pmin(pmax(econ_acc, target_acc), 0.95),
    
    econ_ptbias = (1 - econ_acc) * w_p,
    econ_resid  = (1 - econ_acc) * w_r
  )


########
# 3.   
########

df_preds <- tibble(
  caseid = df$caseid,
  soc_b_pred_fi = as.numeric(predict(lm_soc_b, newdata = df_cf)),
  soc_t_pred_fi = as.numeric(predict(lm_soc_t, newdata = df_cf))
)

df <- df %>%
  left_join(df_preds, by = "caseid")

# -------------------------
# Step 4: Vote simulation sample (Biden vs Trump voters only)
# -------------------------
w2_v <- df[df$VOTE2_w2 < 3 & df$pidf != "8", ]
w2_v$vbiden <- ifelse(w2_v$VOTE2_w2 == 1, 1, 0)

# Vote model with BOTH perceptions (and pid interactions)
x0_bt <- glm(
  vbiden ~ soc_b*pidf + soc_t*pidf + age + black + hispanic + female +
    pkprop + analyprop + currprop + educ,
  family = binomial,
  data = w2_v
)
summary(x0_bt)

# baseline predicted probability
w2_v$reg <- predict(x0_bt, type = "response", newdata = w2_v)

# counterfactual probability: replace soc_b and soc_t with predicted counterfactuals
w2_v_cf <- w2_v
w2_v_cf$soc_b <- w2_v_cf$soc_b_pred_fi
w2_v_cf$soc_t <- w2_v_cf$soc_t_pred_fi
w2_v$nobias_fi <- predict(x0_bt, type = "response", newdata = w2_v_cf)

# expected vote share change (mean probabilities)
mean(w2_v$reg, na.rm = TRUE)
mean(w2_v$nobias_fi, na.rm = TRUE)
mean(w2_v$nobias_fi, na.rm = TRUE) - mean(w2_v$reg, na.rm = TRUE)

########
# 5.   
########

pid_labs <- c(
  "1"="Strong Democrat",
  "2"="Moderate Democrat",
  "3"="Lean Democrat",
  "4"="Independent",
  "5"="Lean Republican",
  "6"="Moderate Republican",
  "7"="Strong Republican"
)

# (A) soc_b plot
w2_v %>%
  dplyr::select(pidf, soc_b, soc_b_pred_fi) %>%
  filter(pidf != 8) %>%
  mutate(pidf = recode(as.character(pidf), !!!pid_labs)) %>%
  group_by(pidf) %>%
  summarize(
    Empirical = mean(soc_b, na.rm = TRUE),
    `Fully informed` = mean(soc_b_pred_fi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Empirical, `Fully informed`),
               names_to = "category", values_to = "mean_value") %>%
  ggplot(aes(
    x = factor(pidf, levels = unname(pid_labs)),
    y = mean_value,
    fill = category
  )) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(mean_value, 2),
        vjust = ifelse(mean_value < 0, 1.5, -0.5)),
    position = position_dodge(0.9)
  ) +
  labs(
    title = "Change in economic perception of Biden (soc_b)",
    x = "",
    y = "",
    fill = "Perception type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# (B) soc_t plot
w2_v %>%
  dplyr::select(pidf, soc_t, soc_t_pred_fi) %>%
  filter(pidf != 8) %>%
  mutate(pidf = recode(as.character(pidf), !!!pid_labs)) %>%
  group_by(pidf) %>%
  summarize(
    Empirical = mean(soc_t, na.rm = TRUE),
    `Fully informed` = mean(soc_t_pred_fi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Empirical, `Fully informed`),
               names_to = "category", values_to = "mean_value") %>%
  ggplot(aes(
    x = factor(pidf, levels = unname(pid_labs)),
    y = mean_value,
    fill = category
  )) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(mean_value, 2),
        vjust = ifelse(mean_value < 0, 1.5, -0.5)),
    position = position_dodge(0.9)
  ) +
  labs(
    title = "Change in economic perception of Trump (soc_t)",
    x = "",
    y = "",
    fill = "Perception type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



########
# 6.1.   
########
source("/Users/mattmettler/Box Sync/Data_2024/EV/w2_data.R")

df <- w2


lm_soc_b <- lm(
  soc_b ~ (econ_ptbias + econ_resid) * pidf + age + female + black + hispanic,
  data = df
)
lm_soc_t <- lm(
  soc_t ~ (econ_ptbias + econ_resid) * pidf + age + female + black + hispanic,
  data = df
)

summary(lm_soc_b)
summary(lm_soc_t)


########
# 6.2   
########
target_acc <- as.numeric(quantile(df$econ_acc, 0.95, na.rm = TRUE))
target_acc <- min(target_acc, 0.95)

df_cf <- df %>%
  mutate(
    target_acc_cap   = pmin(target_acc, 0.95),
    delta_acc_desired = pmax(0, target_acc_cap - econ_acc),
    delta_acc         = pmin(delta_acc_desired, econ_ptbias),
    
    econ_acc    = econ_acc + delta_acc,
    econ_ptbias = econ_ptbias - delta_acc,
    econ_resid  = econ_resid
  )

########
# 6.3.   
########

df_preds <- tibble(
  caseid = df$caseid,
  soc_b_pred_fi = as.numeric(predict(lm_soc_b, newdata = df_cf)),
  soc_t_pred_fi = as.numeric(predict(lm_soc_t, newdata = df_cf))
)

df <- df %>%
  left_join(df_preds, by = "caseid")

########
# 6.4   
########
w2_v <- df[df$VOTE2_w2 < 3 & df$pidf != "8", ]
w2_v$vbiden <- ifelse(w2_v$VOTE2_w2 == 1, 1, 0)

# Vote model with BOTH perceptions (and pid interactions)
x0_bt <- glm(
  vbiden ~ soc_b*pidf + soc_t*pidf + age + black + hispanic + female +
    pkprop + analyprop + currprop + educ,
  family = binomial,
  data = w2_v
)
summary(x0_bt)

# baseline predicted probability
w2_v$reg <- predict(x0_bt, type = "response", newdata = w2_v)

# counterfactual probability: replace soc_b and soc_t with predicted counterfactuals
w2_v_cf <- w2_v
w2_v_cf$soc_b <- w2_v_cf$soc_b_pred_fi
w2_v_cf$soc_t <- w2_v_cf$soc_t_pred_fi
w2_v$nobias_fi <- predict(x0_bt, type = "response", newdata = w2_v_cf)

# expected vote share change (mean probabilities)
mean(w2_v$reg, na.rm = TRUE)
mean(w2_v$nobias_fi, na.rm = TRUE)
mean(w2_v$nobias_fi, na.rm = TRUE) - mean(w2_v$reg, na.rm = TRUE)

########
# 6.5   
########

pid_labs <- c(
  "1"="Strong Democrat",
  "2"="Moderate Democrat",
  "3"="Lean Democrat",
  "4"="Independent",
  "5"="Lean Republican",
  "6"="Moderate Republican",
  "7"="Strong Republican"
)

# (A) soc_b plot
w2_v %>%
  dplyr::select(pidf, soc_b, soc_b_pred_fi) %>%
  filter(pidf != 8) %>%
  mutate(pidf = recode(as.character(pidf), !!!pid_labs)) %>%
  group_by(pidf) %>%
  summarize(
    Empirical = mean(soc_b, na.rm = TRUE),
    `No partisan bias` = mean(soc_b_pred_fi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Empirical, `No partisan bias`),
               names_to = "category", values_to = "mean_value") %>%
  ggplot(aes(
    x = factor(pidf, levels = unname(pid_labs)),
    y = mean_value,
    fill = category
  )) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(mean_value, 2),
        vjust = ifelse(mean_value < 0, 1.5, -0.5)),
    position = position_dodge(0.9)
  ) +
  labs(
    title = "Change in economic perception of Biden (soc_b)",
    x = "",
    y = "",
    fill = "Perception type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# (B) soc_t plot
w2_v %>%
  dplyr::select(pidf, soc_t, soc_t_pred_fi) %>%
  filter(pidf != 8) %>%
  mutate(pidf = recode(as.character(pidf), !!!pid_labs)) %>%
  group_by(pidf) %>%
  summarize(
    Empirical = mean(soc_t, na.rm = TRUE),
    `No Partisan bias` = mean(soc_t_pred_fi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Empirical, `No Partisan bias`),
               names_to = "category", values_to = "mean_value") %>%
  ggplot(aes(
    x = factor(pidf, levels = unname(pid_labs)),
    y = mean_value,
    fill = category
  )) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(mean_value, 2),
        vjust = ifelse(mean_value < 0, 1.5, -0.5)),
    position = position_dodge(0.9)
  ) +
  labs(
    title = "Change in economic perception of Trump (soc_t)",
    x = "",
    y = "",
    fill = "Perception type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




########
# 7.1.   
########
source("/Users/mattmettler/Box Sync/Data_2024/EV/w2_data.R")

df <- w2


lm_soc_b <- lm(
  soc_b ~ (econ_ptbias + econ_resid) * pidf + age + female + black + hispanic,
  data = df
)
lm_soc_t <- lm(
  soc_t ~ (econ_ptbias + econ_resid) * pidf + age + female + black + hispanic,
  data = df
)

summary(lm_soc_b)
summary(lm_soc_t)



########
# 7.2   
########
target_acc <- as.numeric(quantile(df$econ_acc, 0.95, na.rm = TRUE))
target_acc <- min(target_acc, 0.95)

df_cf <- df %>%
  mutate(
    target_acc_cap   = pmin(target_acc, 0.95),
    delta_acc_desired = pmax(0, target_acc_cap - econ_acc),
    delta_acc         = pmin(delta_acc_desired, econ_resid),
    
    econ_acc    = econ_acc + delta_acc,
    econ_ptbias = econ_ptbias,
    econ_resid  = econ_resid- delta_acc
  )

########
# 7.3.   
########

df_preds <- tibble(
  caseid = df$caseid,
  soc_b_pred_fi = as.numeric(predict(lm_soc_b, newdata = df_cf)),
  soc_t_pred_fi = as.numeric(predict(lm_soc_t, newdata = df_cf))
)

df <- df %>%
  left_join(df_preds, by = "caseid")

########
# 7.4   
########
w2_v <- df[df$VOTE2_w2 < 3 & df$pidf != "8", ]
w2_v$vbiden <- ifelse(w2_v$VOTE2_w2 == 1, 1, 0)

# Vote model with BOTH perceptions (and pid interactions)
x0_bt <- glm(
  vbiden ~ soc_b*pidf + soc_t*pidf + age + black + hispanic + female +
    pkprop + analyprop + currprop + educ,
  family = binomial,
  data = w2_v
)
summary(x0_bt)

# baseline predicted probability
w2_v$reg <- predict(x0_bt, type = "response", newdata = w2_v)

# counterfactual probability: replace soc_b and soc_t with predicted counterfactuals
w2_v_cf <- w2_v
w2_v_cf$soc_b <- w2_v_cf$soc_b_pred_fi
w2_v_cf$soc_t <- w2_v_cf$soc_t_pred_fi
w2_v$nobias_fi <- predict(x0_bt, type = "response", newdata = w2_v_cf)

# expected vote share change (mean probabilities)
mean(w2_v$reg, na.rm = TRUE)
mean(w2_v$nobias_fi, na.rm = TRUE)
mean(w2_v$nobias_fi, na.rm = TRUE) - mean(w2_v$reg, na.rm = TRUE)

########
# 7.5   
########

pid_labs <- c(
  "1"="Strong Democrat",
  "2"="Moderate Democrat",
  "3"="Lean Democrat",
  "4"="Independent",
  "5"="Lean Republican",
  "6"="Moderate Republican",
  "7"="Strong Republican"
)

# (A) soc_b plot
w2_v %>%
  dplyr::select(pidf, soc_b, soc_b_pred_fi) %>%
  filter(pidf != 8) %>%
  mutate(pidf = recode(as.character(pidf), !!!pid_labs)) %>%
  group_by(pidf) %>%
  summarize(
    Empirical = mean(soc_b, na.rm = TRUE),
    `No residual error` = mean(soc_b_pred_fi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Empirical, `No residual error`),
               names_to = "category", values_to = "mean_value") %>%
  ggplot(aes(
    x = factor(pidf, levels = unname(pid_labs)),
    y = mean_value,
    fill = category
  )) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(mean_value, 2),
        vjust = ifelse(mean_value < 0, 1.5, -0.5)),
    position = position_dodge(0.9)
  ) +
  labs(
    title = "Change in economic perception of Biden (soc_b)",
    x = "",
    y = "",
    fill = "Perception type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# (B) soc_t plot
w2_v %>%
  dplyr::select(pidf, soc_t, soc_t_pred_fi) %>%
  filter(pidf != 8) %>%
  mutate(pidf = recode(as.character(pidf), !!!pid_labs)) %>%
  group_by(pidf) %>%
  summarize(
    Empirical = mean(soc_t, na.rm = TRUE),
    `No residual error` = mean(soc_t_pred_fi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Empirical, `No residual error`),
               names_to = "category", values_to = "mean_value") %>%
  ggplot(aes(
    x = factor(pidf, levels = unname(pid_labs)),
    y = mean_value,
    fill = category
  )) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(mean_value, 2),
        vjust = ifelse(mean_value < 0, 1.5, -0.5)),
    position = position_dodge(0.9)
  ) +
  labs(
    title = "Change in economic perception of Trump (soc_t)",
    x = "",
    y = "",
    fill = "Perception type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

