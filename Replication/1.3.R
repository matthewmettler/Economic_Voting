library(dplyr)
library(ggplot2)
library(ggtern)
library(patchwork)
library(magick)
# -------------------------------------------------
# 1) Prepare ternary data once
# -------------------------------------------------

w2 <- w2 %>%
  mutate(
    pidf = case_when(
      pidf %in% c("5", "6", "7") ~ "Rep",
      pidf %in% c("1", "2", "3") ~ "Dem",
      pidf %in% c("4")           ~ "Ind",
      #pidf == "8"                ~ "Other",
      TRUE                       ~ NA_character_
    )
  )

df_tern_all <- w2 %>%
  dplyr::select(
    econ_acc, econ_ptbias, econ_resid, pidf
  ) %>%
  filter(
    complete.cases(econ_acc, econ_ptbias, econ_resid),
    !is.na(pidf)
  ) 
# -------------------------------------------------
# 2) Reusable plotting function
# -------------------------------------------------
make_tern_plot <- function(data, title_text) {
  
  mean_points <- data %>%
    summarise(
      JA  = mean(econ_acc, na.rm = TRUE),
      PRB = mean(econ_ptbias, na.rm = TRUE),
      RR  = mean(econ_resid, na.rm = TRUE)
    ) %>%
    mutate(type = "Mean continuous")
  
  
  ggtern(
    data = data,
    aes(x = econ_ptbias, y = econ_acc, z = econ_resid)
  ) +
    geom_point(
      alpha = 0.25,
      size = 1,
      color = "gray30"
    ) +
    geom_point(
      data = mean_points,
      aes(x = PRB, y = JA, z = RR, fill = type, shape = type),
      inherit.aes = FALSE,
      size = 4,
      color = "black",
      stroke = 0.8
    ) +
    scale_fill_manual(
      values = c(
        "Mean discrete"   = "lightblue",
        "Mean continuous" = "lightgreen"
      )
    ) +
    scale_shape_manual(
      values = c(
        "Mean discrete"   = 24,
        "Mean continuous" = 21
      )
    ) +
    xlab("Partisan\nresponse\nbias") +
    ylab("Judgmental\naccuracy") +
    zlab("Residual\nresponse") +
    labs(
      fill = NULL,
      shape = NULL,
      title = title_text
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 16),
      title_text   =element_text(size = 16),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x  = element_text(size = 12),
      axis.text.y  = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 12),
      tern.axis.title.T = element_text(size = 12),
      tern.axis.title.L = element_text(size = 12),
      tern.axis.title.R = element_text(size = 12),
      legend.key.size = unit(1.2, "lines")
    ) +
    guides(
      fill = guide_legend(nrow = 1),
      shape = guide_legend(nrow = 1)
    )
}

# -------------------------------------------------
# 2) Filtered datasets
# -------------------------------------------------
df_tern_ind <- df_tern_all %>% filter(pidf == "Ind")
df_tern_rep <- df_tern_all %>% filter(pidf == "Rep")
df_tern_dem <- df_tern_all %>% filter(pidf == "Dem")

# -------------------------------------------------
# 3) Build plots
# -------------------------------------------------

p_dem <- make_tern_plot(df_tern_dem, "Democrats")
p_ind <- make_tern_plot(df_tern_ind, "Independents")
p_rep <- make_tern_plot(df_tern_rep, "Republicans")


