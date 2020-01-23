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

filename <- 'results/12_17/4_get_best_bounds_1.xlsx'

our <- read_all_results(filename, 'LMCUT_T3', rows_to_keep = rows_to_keep)
sat <- read_all_results(filename, 'SAT', rows_to_keep = rows_to_keep)

seqs <- tibble(our = our$seqs, sat = sat$seqs) %>%
  filter(!is.na(our), !is.na(sat)) %>% mutate(our = our + 1, sat = sat + 1)
g1 <- scatter_plot(log2(seqs$sat), log2(seqs$our),
             x_label = 'OpSeq', y_label = 'OpSearch',
             subtitle = TeX("$S$"), fontsize = 80, pointsize=20) + xlim(0, 13) + ylim(0, 13)

planner_memory <- tibble(our = our$planner_memory, sat = sat$planner_memory) %>%
  filter(!is.na(our), !is.na(sat)) %>% mutate(our = our + 1, sat = sat + 1)
g2 <- scatter_plot(log2(planner_memory$sat), log2(planner_memory$our),
             x_label = 'OpSeq', y_label = 'OpSearch',
             subtitle = TeX("$M$"), fontsize = 80, pointsize=20) + xlim(6, 13) + ylim(6, 13)

mean_ops_by_constraint <- tibble(our = our$mean_ops_by_constraint, sat = sat$mean_ops_by_constraint) %>%
  filter(!is.na(our), !is.na(sat)) %>% mutate(our = our + 1, sat = sat + 1)
g3 <- scatter_plot(log2(mean_ops_by_constraint$sat), log2(mean_ops_by_constraint$our), 
             x_label = 'OpSeq', y_label = 'OpSearch',
             subtitle = TeX("$\\bar{u}$"), fontsize = 80, pointsize=20) + xlim(0, 7) + ylim(0, 7)

seqs_time <- tibble(our = our$total_seq_time, sat = sat$total_seq_time) %>%
  filter(!is.na(our), !is.na(sat)) %>% mutate(our = our + 1, sat = sat + 1)
g4 <- scatter_plot(log2(seqs_time$sat), log2(seqs_time$our),
             x_label = 'OpSeq', y_label = 'OpSearch',
             subtitle = TeX("$S_t$"), fontsize = 80, pointsize=20) + xlim(0, 12) + ylim(0, 12)

total_time <- tibble(our = our$total_solve_time, sat = sat$total_solve_time) %>%
  filter(!is.na(our), !is.na(sat)) %>% mutate(our = our + 1, sat = sat + 1)
g5 <- scatter_plot(log2(total_time$sat), log2(total_time$our), 
             x_label = 'OpSeq', y_label = 'OpSearch',
             subtitle = TeX("$T_t$"), fontsize = 80, pointsize=20) + xlim(0, 13) + ylim(0, 13)

ggsave('figs/main_scatter_1.pdf', plot = g1, family = 'Times', width = 25, height = 25)
ggsave('figs/main_scatter_2.pdf', plot = g2, family = 'Times', width = 25, height = 25)
ggsave('figs/main_scatter_3.pdf', plot = g3, family = 'Times', width = 25, height = 25)
ggsave('figs/main_scatter_4.pdf', plot = g4, family = 'Times', width = 25, height = 25)
ggsave('figs/main_scatter_5.pdf', plot = g5, family = 'Times', width = 25, height = 25)

#ggsave('figs/main_scatter_1.pdf', plot = ggarrange(g1, g2, nrow = 1), family = 'Times', width = 35, height = 15)
#ggsave('figs/main_scatter_2.pdf', plot = ggarrange(g3, g4, nrow = 1), family = 'Times', width = 35, height = 15)
#ggsave('figs/main_scatter_3.pdf', plot = g5, family = 'Times', width = 20, height = 15)
