# Set up ======================================================================
suppressPackageStartupMessages({
  # Generic
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(writexl)
  
  # Spatial analysis
  library(sp)
  library(sf)
  
  # plot function
  library(ggplot2)
  library(ggprism) # Doesn't seems to works
  library(RColorBrewer)
  library(cowplot)
  library(ggpubr)
  library(ggbeeswarm)
  library(plotly)
})



wd <- "/Users/chad/Lab/dev/embryo_IF_analyzer"
script_dir <- paste0(wd, "/scripts/R")

# NG_cond_vec <- c("No Primary", "anti-NR5A2", "anti-GATA6", "anti-NR5A2 +\nanti-GATA6")
# NK_cond_vec <- c("No Primary", "anti-NR5A2", "anti-KLF5", "anti-NR5A2 +\nanti-KLF5")
# cond_color_map <- c(
#   `No Primary` = "gray50",
#   `anti-NR5A2` = "goldenrod2",
#   `anti-GATA6` = "darkturquoise",
#   `anti-NR5A2 +\nanti-GATA6` = "coral2",
#   `anti-KLF5` = "aquamarine3",
#   `anti-NR5A2 +\nanti-KLF5` = "coral2",
#   character(0)
# )
# # For black and white plot
# cond_color_map_BW <- setNames(rep("gray50", length(cond_color_map)), nm=names(cond_color_map))
# 
# cond_compare_list <- list(
#   G1 = list(
#     c(cond_vec[4], cond_vec[3]),
#     c(cond_vec[4], cond_vec[2]),
#     c(cond_vec[4], cond_vec[1])
#   ),
#   G2 = list(
#     c(cond_vec[2], cond_vec[3]),
#     c(cond_vec[1], cond_vec[2]),
#     c(cond_vec[1], cond_vec[3])
#   )
# )

pla_res_path_df <- tribble(
  ~expr_set, ~rep, ~path,
  "Nr5a2_Gata6", "rep1", paste0(wd, "/data/PLA_Nr5a2_Gata6_rep1/output/Nr5a2_Gata6_rep1-count_df.rds"),
  "Nr5a2_Gata6", "rep2", paste0(wd, "/data/PLA_Nr5a2_Gata6_rep2/output/Nr5a2_Gata6_rep2-count_df.rds"),
  "Nr5a2_Klf5", "rep1", paste0(wd, "/data/PLA_Nr5a2_Klf5_rep1/output/Nr5a2_Klf5_rep1-count_df.rds"),
  "Nr5a2_Klf5", "rep2", paste0(wd, "/data/PLA_Nr5a2_Klf5_rep2/pool_res/output/Nr5a2_Klf5_rep2-count_df.rds")
)

pla_df <- tibble()
for(i in seq_along(pla_res_path_df$path)){
  df <- readRDS(pla_res_path_df$path[[i]])
  df$expr_set <- pla_res_path_df$expr_set[i]
  df$rep <- pla_res_path_df$rep[i]
  df$condition <- as.character(df$condition)
  df <- dplyr::select(df, expr_set, rep, position, pos, everything())
  pla_df <- bind_rows(pla_df, df)
}


PLA_theme <- theme_classic() +
  theme(title=element_text(color="black", size=15, family="Helvetica"),
        axis.line=element_line(color="black", linewidth=1),
        axis.ticks.length.x = unit(2, "mm"),
        axis.ticks.length.y = unit(4, "mm"),
        axis.ticks=element_line(color="black", linewidth=1),
        axis.text=element_text(color="black", size=15, family="Helvetica"),
        axis.text.x=element_text(color="black", size=15, family="Helvetica"),
        axis.title=element_text(color="black", size=15, family="Helvetica"),
        legend.text=element_text(color="black", size=15, family="Helvetica"),
        legend.title=element_text(color="black", size=15, family="Helvetica"))


PLA_cond_vec <- c("No Primary", "anti-NR5A2", "anti-GATA6", "anti-KLF5", "anti-NR5A2 +\nanti-GATA6", "anti-NR5A2 +\nanti-KLF5")
cond_compare_list <- list(
  # No primary vs. others
  c(PLA_cond_vec[1], PLA_cond_vec[2]),
  c(PLA_cond_vec[1], PLA_cond_vec[3]),
  c(PLA_cond_vec[1], PLA_cond_vec[4]),
  c(PLA_cond_vec[1], PLA_cond_vec[5]),
  c(PLA_cond_vec[1], PLA_cond_vec[6]),
  
  # NG condition vs others
  c(PLA_cond_vec[5], PLA_cond_vec[2]),
  c(PLA_cond_vec[5], PLA_cond_vec[3]),
  
  # NK condition vs others
  c(PLA_cond_vec[6], PLA_cond_vec[2]),
  c(PLA_cond_vec[6], PLA_cond_vec[4])
)

unique(pla_df$condition)

pla_df %>% 
  group_by(expr_set, rep, condition) %>% 
  reframe(n=length(unique(position))) %>% 
  dplyr::mutate(condition = str_replace_all(condition, "((\\s+)|(\\-)|(\\\\n)|(\\+))+", "_")) %>% 
  dplyr::filter(condition)
  tidyr::spread(key="condition", value=n)

colnames(pla_df)
plot_x_dodge = 0.6
plot_filter_vec <- unique(pla_df$expr_set)
plot_y_break_strep = c(1, 2)
p_list <- list()
for(i in seq_along(plot_filter_vec)){
  p_df <- pla_df %>% 
    dplyr::filter(expr_set == plot_filter_vec[i]) %>% 
    dplyr::mutate(condition_f = factor(condition, levels=PLA_cond_vec[PLA_cond_vec %in% unique(condition)]),
                  label = paste0(position, " - ", nucleus_id)) 
  
  cur_compare_list <- cond_compare_list[sapply(cond_compare_list, function(x){all(x %in% unique(p_df$condition))})]
  y_break_max <- ceiling(max(p_df$pla_density, na.rm=TRUE))*2
  y_break_vec <- seq(min(p_df$pla_density, na.rm=TRUE), y_break_max, by=plot_y_break_strep[i])
  
  p_list[[plot_filter_vec[i]]] <- p_df %>% 
    ggplot(aes(x = condition_f, y=pla_density, group=interaction(condition_f, rep), label=label)) +
    # geom_quasirandom(dodge.width=plot_x_dodge, width=0.15, color="gray40", size=4, alpha=0.8, shape=1) +
    geom_quasirandom(aes(color=rep), dodge.width=plot_x_dodge, width=0.15, size=4, alpha=0.8, shape=20) + 
    scale_color_manual(values=c(rep1="coral2", rep2="cyan3")) +
    geom_boxplot(position=position_dodge(width=plot_x_dodge), width=0.3, fill=NA, outlier.alpha=0, linewidth=1) +
    stat_compare_means(comparisons=cur_compare_list,
                       method="wilcox.test", tip.length=0, step.increase=0.12, hide.ns=TRUE, size=7) +
    labs(title=paste0(plot_filter_vec[i], " (per nucleus)"), 
         x="Antibody", y="PLA Density") +
    scale_y_continuous(breaks=y_break_vec, , expand=expansion(mult=c(0, 0.07), add=c(0, 1)), limits=c(0, NA)) +
    PLA_theme +
    theme(legend.position="bottom")
  
  
  p_list[[paste0(plot_filter_vec[i], "_embryo")]] <- p_df %>%
    group_by(expr_set, rep, position, condition, condition_f) %>%
    dplyr::reframe(pla_density = mean(pla_density),
                   label = unique(position)) %>% 
    ggplot(aes(x = condition_f, y=pla_density, group=interaction(condition_f, rep), label=label)) +
    # geom_quasirandom(dodge.width=plot_x_dodge, width=0.15, color="gray40", size=4, alpha=0.8, shape=1) +
    geom_quasirandom(aes(color=rep), dodge.width=plot_x_dodge, width=0.15, size=4, alpha=0.8, shape=20) + 
    scale_color_manual(values=c(rep1="coral2", rep2="cyan3")) +
    geom_boxplot(position=position_dodge(width=plot_x_dodge), width=0.3, fill=NA, outlier.alpha=0, linewidth=1) +
    stat_compare_means(comparisons=cur_compare_list,
                       method="wilcox.test", tip.length=0, step.increase=0.12, hide.ns=TRUE, size=7) +
    labs(title=paste0(plot_filter_vec[i], " (per embryo)"), 
         x="Antibody", y="PLA Density") +
    # scale_y_continuous(breaks=y_break_vec, , expand=expansion(mult=c(0, 0.07), add=c(0, 1)), limits=c(0, NA)) +
    PLA_theme +
    theme(legend.position="bottom")
}

# ggplotly(p_list$Nr5a2_Gata6 + facet_grid(rep~., scales="free_y"))

# Save plots
{
  pdf(file="/Users/chad/Lab/dev/embryo_IF_analyzer/data/PLA_plot_all.pdf", width=6, height=8)
  for(i in seq_along(p_list)){
    print(p_list[[i]])
  }
  dev.off()
}
