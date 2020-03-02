source('common.R')

rows_to_keep <- function() {
  c(
    'instance',
    'solved',
    'seqs',
    'restarts',
    'total_astar_time',
    'total_solve_time',
    'planner_memory',
    'mean_ops_by_constraint',
    'best_bound'
  )
}

filename_df <- 'results/12_17/5_emphasis_default.xlsx'
filename_bb <- 'results/12_17/5_emphasis_bestbound.xlsx'

our_df <- read_all_results(filename_df, 'LMCUT_T3', rows_to_keep = rows_to_keep)
our_bb <- read_all_results(filename_bb, 'LMCUT_T3', rows_to_keep = rows_to_keep)
sat_df <- read_all_results(filename_df, 'SAT', rows_to_keep = rows_to_keep)
sat_bb <- read_all_results(filename_bb, 'SAT', rows_to_keep = rows_to_keep)

seqs_our <- tibble(df = our_df$seqs, bb = our_bb$seqs, type = 'OpSearch') %>%
  filter(!is.na(df), !is.na(bb))
seqs_sat <- tibble(df = sat_df$seqs, bb = sat_bb$seqs, type = 'OpSeq') %>%
  filter(!is.na(df), !is.na(bb))
all <- bind_rows(seqs_our, seqs_sat)

all <- ggplot(all, aes(log2(df), log2(bb))) +
  xlab('balanced') + ylab('best bound') +
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal() +
  theme(
    text = element_text(size = 80),
    axis.text = element_text(color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size = 1),
    plot.subtitle = element_text(hjust = 0.5),
    panel.spacing = unit(20, "lines")
  ) +
  geom_point(size = 15, shape = 1, fill = "white", alpha = 1) +
  geom_point(size = 15, shape = 16, fill = "black", alpha = 0.3) +
  facet_wrap(~type, scales = "fixed") +
  coord_fixed() + xlim(0, 15) + ylim(0, 15)
ggsave('figs/mip_scatter.pdf', plot = all, family = 'Times', 
       width = 31, height = 15)


#g1 <- scatter_plot(log2(seqs_our$df), log2(seqs_our$bb),
#             x_label = "balanced",
#             y_label = "best bound",
#             subtitle = 'OpSearch') + xlim(0, 12) + ylim(0, 12)
#g2 <- scatter_plot(log2(seqs_sat$df), log2(seqs_sat$bb),
#                   subtitle = 'OpSeq') + xlim(0, 12) + ylim(0, 12)

#all <- ggarrange(g1, g2, nrow = 1)
#ggsave('figs/mip_scatter2.pdf', plot = all, family = 'Times', 
#       width = 20, height = 10)
