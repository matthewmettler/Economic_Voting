
df<-w2



comp_cols <- c("econ_acc", "econ_ptbias", "econ_resid")  
pred_cols <- c("repdum","demdum", "affpolr", "affpold","pkprop","analyprop","currprop","educ","female","age")


dfz<- df[, c(comp_cols, pred_cols)]


dfz <- dfz[complete.cases(dfz), ]


Y <- as.matrix(dfz[, comp_cols])
X<-data.frame(dfz[,pred_cols])





vars_z <- c("affpold","affpolr","pkprop","analyprop","currprop","educ","age")


z_info <- lapply(vars_z, function(v) {
  x <- dfz[[v]]
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  list(mean = m, sd = s)
})
names(z_info) <- vars_z

## add z-scored versions in dfz
for (v in vars_z) {
  m <- z_info[[v]]$mean
  s <- z_info[[v]]$sd
  dfz[[paste0(v, "_z")]] <- (dfz[[v]] - m) / s
}

comp_cols <- c("econ_acc", "econ_ptbias", "econ_resid") 
pred_cols <- c("affpolr_z", "affpold_z","repdum","pkprop_z","analyprop_z","currprop_z","educ_z","age_z","female")

dfz<- dfz[, c(comp_cols, pred_cols)]


dfz <- dfz[complete.cases(dfz), ]




Y <- as.matrix(dfz[, comp_cols])
X<-data.frame(dfz[,pred_cols])

# grid of values at their mean other than current events that varies across standardized scale
grid_pk <- data.frame(
  affpolr_z  = mean(dfz$affpolr_z[dfz$repdum==1], na.rm = TRUE), 
  affpold_z  = min(dfz$affpold_z[dfz$repdum==1], na.rm = TRUE),
  repdum     = 1,
  pkprop_z   = mean(dfz$pkprop_z,na.rm=TRUE),
  currprop_z = sort(unique(dfz$currprop_z)),
  analyprop_z= mean(dfz$analyprop_z, na.rm = TRUE),
  educ_z     = mean(dfz$educ_z, na.rm = TRUE),
  female     = mean(dfz$female,na.rm=TRUE),
  age_z      = mean(dfz$age_z, na.rm = TRUE)) 

grid_pk <- grid_pk[, names(X)]


fit_z <- Compositional::zadr(y = Y_adj, x = X, con = TRUE, xnew = grid_pk)
fit_z # fit model 


# steps below create bootstrapped CI for predicted means 
B <- 1000

est0 <- Compositional::zadr(y = Y, x = X, xnew = grid_pk, con = TRUE)$est
colnames(est0) <- comp_cols

nG <- nrow(est0)
nC <- ncol(est0)

boot_draws <- array(NA_real_, dim = c(nG, nC, B),
                    dimnames = list(NULL, comp_cols, NULL))

for (b in 1:B) {
  ii <- sample.int(nrow(X), replace = TRUE)
  estb <- Compositional::zadr(y = Y[ii, , drop = FALSE],
                              x = X[ii, , drop = FALSE],
                              xnew = grid_pk, con = TRUE)$est
  colnames(estb) <- comp_cols
  boot_draws[,,b] <- estb
}

Mean <- apply(boot_draws, c(1,2), mean)
Lwr  <- apply(boot_draws, c(1,2), quantile, probs = 0.025)
Upr  <- apply(boot_draws, c(1,2), quantile, probs = 0.975)

ci_df <- bind_rows(lapply(seq_along(comp_cols), function(k) {
  tibble(
    currprop_z   = grid_pk$currprop_z,
    Component= comp_cols[k],
    Mean     = Mean[, k],
    Lower    = Lwr[, k],
    Upper    = Upr[, k]
  )
}))
ci_df$Component<-ifelse(ci_df$Component=="econ_acc","Judgmental Accuracy",ifelse(ci_df$Component=="econ_ptbias","Partisan response bias","Residual response"))

pk_effect <- bind_rows(lapply(seq_along(comp_cols), function(k) {
  
  diffs <- sapply(1:B, function(b) {
    boot_draws[nG, k, b] - boot_draws[1, k, b]
  })
  
  tibble(
    Component = comp_cols[k],
    Effect = mean(diffs, na.rm = TRUE),
    Lower  = quantile(diffs, 0.025, na.rm = TRUE),
    Upper  = quantile(diffs, 0.975, na.rm = TRUE),
    p_two  = 2 * min(
      mean(diffs <= 0, na.rm = TRUE),
      mean(diffs >= 0, na.rm = TRUE)
    )
  )
}))

pk_effect$Component <- ifelse(
  pk_effect$Component == "econ_acc", "Judgmental Accuracy",
  ifelse(pk_effect$Component == "econ_ptbias", "Partisan response bias", "Residual response")
)

ce_Dr_effect<-pk_effect


ci_w2<-ci_df

ce_r_con<-ggplot(ci_w2,aes(x = currprop_z, y = Mean, color = Component, fill = Component)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.25, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(
    "Judgmental Accuracy"    = "#1b9e77",
    "Partisan response bias" = "pink2",
    "Residual response"      = "skyblue"
  ), drop = FALSE) +
  scale_fill_manual(values = c(
    "Judgmental Accuracy"    = "#1b9e77",
    "Partisan response bias" = "pink2",
    "Residual response"      = "skyblue"
  ), drop = FALSE) +
  labs(x = "Current events", y = NULL,title = "",color = NULL, fill = NULL) +
  scale_y_continuous(limits = c(.1, .7),breaks = c(.1,.2,.3,.4,.5,.6,.7)) +
  ggtitle(label="",subtitle = "Republican")+
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.margin = margin(t = 6),
    legend.box.margin = margin(t = 4),
    
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.text = element_text(color = "grey20"),
    
    
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 16)
  )


# re-run for current events with demdum for democrats and repdum for republicans 
# then run for affpolr and affpold with repdum and demdum respectively 


ggarrange(ce_d_con,ce_r_con,aff_d,aff_r,nrow=2,ncol=2,common.legend = TRUE,legend = "bottom")
