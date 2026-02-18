rm(list=ls())

library(haven)
library(tidyverse)
library(nnet)
library(stargazer)
library(estimatr)
library(zCompositions)
library(compositions)

source("/Users/mattmettler/Box Sync/Data_2024/EV/w2_data.R")
#w2<-w2[w2$VOTE2_w2<3& w2$pidf!="8",] # subset to only those voting for Biden or Trump

# -------------------------
# Step 0: Setup
# -------------------------
#comp_vars <- c("econ_acc","econ_resid","econ_ptbias")
#pred_vars <- c("pidf", "age", "soc_b","soc_t","soc_bt","VOTE2_w2","educ","pkprop","analyprop","currprop",
#               "female","analytical", "black","hispanic","caseid")

df<-w2
# -------------------------
# Step 3: Fit model (soc_bt)
# -------------------------
lm_soc <- lm(
  soc_bt ~ (econ_ptbias+econ_resid) * pidf + age + female + black + hispanic,
  data = df
)
summary(lm_soc)

# predict fully informed i.e. no residual and partisan response
target_acc <- as.numeric(quantile(df$econ_acc, 0.95, na.rm = TRUE))
# keep away from the boundary to avoid extreme ILR instability
econ_acc = pmin(pmax(df$econ_acc, target_acc), 0.95)

df_cf <- df %>%
  mutate(
    err = 1 - econ_acc,
    w_p = ifelse(err > 0, econ_ptbias / err, 0),
    w_r = ifelse(err > 0, econ_resid  / err, 1),
    
    # raise accuracy up to target_acc (do not lower those already above)
    econ_acc = pmin(pmax(econ_acc, target_acc), 0.95),
    
    econ_ptbias = (1 - econ_acc) * w_p,
    econ_resid  = (1 - econ_acc) * w_r
  )

db1 <- data.frame(caseid = df$caseid, pv<-predict(lm_soc, newdata = df_cf))

colnames(db1)<-c("caseid","soc_bt_pred_fi")

df <- df %>%
  left_join(db1, by = "caseid")
# -------------------------
# Step 5: Vote simulation (Biden vs Trump voters only)
# -------------------------
w2_v <- df[df$VOTE2_w2 < 3 & df$pidf != "8", ]

w2_v$vbiden <- ifelse(w2_v$VOTE2_w2 == 1, 1, 0)

x0 <- glm(
  vbiden ~ soc_bt*pidf + age + black + hispanic + female + pkprop + analyprop + currprop+educ,
  family = binomial,
  data = w2_v
)
summary(x0)

# baseline predicted probability
w2_v$reg <- predict(x0, type = "response", newdata = w2_v)

# counterfactual probability: replace soc_bt with predicted counterfactual soc_bt
w2_v_cf <- w2_v
w2_v_cf$soc_bt <- w2_v_cf$soc_bt_pred_fi
w2_v$nobias_fi <- predict(x0, type = "response", newdata = w2_v_cf)

# optional: threshold classification (secondary)
w2_v$b_vote_reg       <- ifelse(w2_v$reg >= .5, "Biden", "Trump")
w2_v$b_vote_nobias_fi  <- ifelse(w2_v$nobias_fi >= .5, "Biden", "Trump")

table(w2_v$b_vote_reg)
table(w2_v$b_vote_nobias_fi)

# preferred: expected vote share change (mean probabilities)
mean(w2_v$reg, na.rm = TRUE)
mean(w2_v$nobias_fi, na.rm = TRUE)
mean(w2_v$nobias_fi, na.rm = TRUE) - mean(w2_v$reg, na.rm = TRUE)

# -------------------------
# Step 6: Figure empirical vs counterfactual (by PID)
# -------------------------
w2_v%>%
  dplyr::select(pidf,soc_bt,soc_bt_pred_fi)%>%
  filter(pidf!=8)%>%
  mutate(pidf=case_when(pidf==1 ~"Strong Democrat",
                        pidf==2 ~"Moderate Democrat",
                        pidf==3 ~"Lean Democrat",
                        pidf==4 ~"Independent",
                        pidf==5 ~"Lean Republican",
                        pidf==6 ~"Moderate Republican",
                        pidf==7 ~"Strong Republican"
  ))%>%
  group_by(pidf)%>%
  summarize(
    Empirical=mean(soc_bt,na.rm=TRUE),
    `Fully informed`=mean(soc_bt_pred_fi,na.rm=TRUE))%>%
  ungroup()%>%
  pivot_longer(cols = c(Empirical, `Fully informed`), names_to = "category", values_to = "mean_value")%>%
  ggplot(., aes(x = factor(pidf,levels = c("Strong Democrat","Moderate Democrat","Lean Democrat","Independent","Lean Republican","Moderate Republican","Strong Republican")), y = mean_value, group = category,fill=category))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  geom_text(aes(label = round(mean_value, 2), 
                vjust = ifelse(mean_value < 0, 1.5, -0.5)),  # Adjust label placement
            position = position_dodge(0.9)) +    
  labs(title = "Change in economic perception",
       x = "",
       y = "",
       fill="Perception type")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


