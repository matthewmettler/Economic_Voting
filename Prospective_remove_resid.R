library(haven)
library(tidyverse)
library(nnet)
library(stargazer)
rm(list=ls())

source("/Users/mattmettler/Box Sync/Data_2024/EV/w3_data.R")

#w2<-w2[w2$VOTE2_w2<3& w2$pidf!="8",] # subset to only those voting for Biden or Trump

df<-w3




lm_soc <- lm(
  soc_ht ~ (w3_econ_ptbias+w3_econ_resid) * pidf + age + female + black + hispanic,
  data = df
)
summary(lm_soc)


########
# 2.   
########



# 95th percentile target (a floor)

target_acc <- as.numeric(quantile(df$w3_econ_acc, 0.95, na.rm = TRUE))

df_cf <- df %>%
  dplyr::mutate(
    target_acc_cap = pmin(target_acc, 0.95),
    
    # desired post-shift accuracy (floor at p95, but cap at .95)
    acc_floor = pmin(pmax(w3_econ_acc, target_acc_cap), 0.95),
    
    delta_acc_desired = pmax(0, acc_floor - w3_econ_acc),
    delta_acc         = pmin(delta_acc_desired, w3_econ_resid),
    
    w3_econ_acc    = w3_econ_acc + delta_acc,
    w3_econ_resid = w3_econ_resid - delta_acc
    # w3_econ_resid unchanged
  ) %>%
  dplyr::select(-acc_floor, -delta_acc_desired)


########
# 3.   
########

db1 <- tibble::tibble(
  caseid = df$caseid,
  soc_ht_pred_fi = predict(lm_soc, newdata = df_cf)
)

df <- df %>% left_join(db1, by = "caseid")
########
# 4.   
########
w3_v<-df[df$TWVOTE2_w3<3& df$pidf!="8",]

w3_v$vharris <- ifelse(w3_v$TWVOTE2_w3 == 1, 1, 0)

x0 <- glm(
  vharris ~ soc_ht*pidf + age + black + hispanic + female + pkprop + analyprop + currprop+educ,
  family = binomial,
  data = w3_v
)
summary(x0)

# baseline predicted probability
w3_v$reg <- predict(x0, type = "response", newdata = w3_v)

# counterfactual probability: replace soc_bt with predicted counterfactual soc_bt
w3_v_cf <- w3_v
w3_v_cf$soc_ht <- w3_v_cf$soc_ht_pred_fi
w3_v$nobias_fi <- predict(x0, type = "response", newdata = w3_v_cf)

# optional: threshold classification (secondary)
w3_v$b_vote_reg       <- ifelse(w3_v$reg >= .5, "Harris", "Trump")
w3_v$b_vote_nobias_fi  <- ifelse(w3_v$nobias_fi >= .5, "Harris", "Trump")

table(w3_v$b_vote_reg)
table(w3_v$b_vote_nobias_fi)

# preferred: expected vote share change (mean probabilities)
mean(w3_v$reg, na.rm = TRUE)
mean(w3_v$nobias_fi, na.rm = TRUE)
mean(w3_v$nobias_fi, na.rm = TRUE) - mean(w3_v$reg, na.rm = TRUE)

########
# 5.   
########
w3_v%>%
  dplyr::select(pidf,soc_ht,soc_ht_pred_fi)%>%
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
    Empirical=mean(soc_ht,na.rm=TRUE),
    `No residual error`=mean(soc_ht_pred_fi,na.rm=TRUE))%>%
  ungroup()%>%
  pivot_longer(cols = c(Empirical, `No residual error`), names_to = "category", values_to = "mean_value")%>%
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


