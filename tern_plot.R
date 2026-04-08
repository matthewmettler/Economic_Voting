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
    econ_acc, econ_ptbias, econ_resid,
    JA_D, PRB_D, RR_D, pidf
  ) %>%
  filter(
    complete.cases(econ_acc, econ_ptbias, econ_resid),
    complete.cases(JA_D, PRB_D, RR_D),
    !is.na(pidf)
  ) %>%
  filter(
    abs(econ_acc + econ_ptbias + econ_resid - 1) < 1e-6,
    abs(JA_D + PRB_D + RR_D - 1) < 1e-6
  )

# -------------------------------------------------
# 2) Reusable plotting function
# -------------------------------------------------
make_tern_plot <- function(data, title_text) {
  
  mean_points <- bind_rows(
    data %>%
      summarise(
        JA  = mean(JA_D, na.rm = TRUE),
        PRB = mean(PRB_D, na.rm = TRUE),
        RR  = mean(RR_D, na.rm = TRUE)
      ) %>%
      mutate(type = "Mean discrete"),
    
    data %>%
      summarise(
        JA  = mean(econ_acc, na.rm = TRUE),
        PRB = mean(econ_ptbias, na.rm = TRUE),
        RR  = mean(econ_resid, na.rm = TRUE)
      ) %>%
      mutate(type = "Mean continuous")
  )
  
  ggtern(
    data = data,
    aes(x = econ_ptbias, y = econ_acc, z = econ_resid)
  ) +
    geom_point(
      alpha = 0.35,
      size = 1.5,
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
        "Mean discrete"   = 21,
        "Mean continuous" = 24
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
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.text.x  = element_text(size = 18),
      axis.text.y  = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.text  = element_text(size = 18),
      tern.axis.title.T = element_text(size = 18),
      tern.axis.title.L = element_text(size = 18),
      tern.axis.title.R = element_text(size = 18),
      legend.key.size = unit(1.2, "lines")
    ) +
    guides(
      fill = guide_legend(nrow = 1),
      shape = guide_legend(nrow = 1)
    )
}

# -------------------------------------------------
# 2) Filtered datasets
# assumes df_tern_all already exists
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

# versions without legends so all three panels align
p_dem_noleg <- p_dem + theme(legend.position = "none")
p_ind_noleg <- p_ind + theme(legend.position = "none")
p_rep_noleg <- p_rep + theme(legend.position = "none")

# -------------------------------------------------
# 4) Save panel images at identical size
# -------------------------------------------------
ggsave("p_tern_dem.png", p_dem_noleg, width = 10, height = 12, dpi = 200, bg = "white")
ggsave("p_tern_ind.png", p_ind_noleg, width = 10, height = 12, dpi = 200, bg = "white")
ggsave("p_tern_rep.png", p_rep_noleg, width = 10, height = 12, dpi = 200, bg = "white")

# -------------------------------------------------
# 5) Create one clean standalone legend
#    suppress shape guide, but show shapes through fill legend
# -------------------------------------------------
p_dem_leg <- p_dem +
  guides(
    fill = guide_legend(
      nrow = 1,
      override.aes = list(
        shape  = c(21, 24),
        color  = "black",
        size   = 4,
        stroke = 0.8
      )
    ),
    shape = "none"
  )

legend_grob <- cowplot::get_legend(p_dem_leg)
legend_plot <- cowplot::ggdraw(legend_grob)

ggsave("tern_legend.png", legend_plot, width = 8, height = 1.5, dpi = 200, bg = "white")

# -------------------------------------------------
# 6) Read saved images
# -------------------------------------------------
img_dem <- image_read("p_tern_dem.png")
img_ind <- image_read("p_tern_ind.png")
img_rep <- image_read("p_tern_rep.png")
img_leg <- image_read("tern_legend.png")

# -------------------------------------------------
# 7) Combine three panels horizontally
# -------------------------------------------------
panel_row <- image_append(c(img_dem, img_ind, img_rep), stack = FALSE)

# -------------------------------------------------
# 8) Add spacer and center legend under full row
# -------------------------------------------------
panel_info <- image_info(panel_row)
leg_info   <- image_info(img_leg)

# resize legend canvas to match full panel width
img_leg_centered <- image_extent(
  img_leg,
  geometry = paste0(panel_info$width, "x", leg_info$height),
  gravity = "center",
  color = "white"
)

# optional white spacer between panels and legend
spacer <- image_blank(
  width = panel_info$width,
  height = 80,
  color = "white"
)

# stack panels + spacer + legend
final_plot <- image_append(c(panel_row, spacer, img_leg_centered), stack = TRUE)

# -------------------------------------------------
# 9) Resize final output to 12 x 15 inches at 300 dpi
# -------------------------------------------------
final_plot_resized <- image_resize(final_plot)

# -------------------------------------------------
# 10) Write final files
# -------------------------------------------------
image_write(final_plot_resized, "ternary_3panel.png")
image_write(final_plot_resized, "ternary_3panel.pdf", format = "pdf")
