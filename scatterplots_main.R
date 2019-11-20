source('common.R')

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

our <- read_all_results(filename, 'LMCUT_T3')
sat <- read_all_results(filename, 'SAT')

seqs <- tibble(our = our$seqs, sat = sat$seqs) %>%
  filter(!is.na(our), !is.na(sat))
g1 <- scatter_plot(log2(seqs$our), log2(seqs$sat),
             y_label = 'OpSeq',
             subtitle = TeX("$S$")) + xlim(0, 13) + ylim(0, 13)

planner_memory <- tibble(our = our$planner_memory, sat = sat$planner_memory) %>%
  filter(!is.na(our), !is.na(sat))
g2 <- scatter_plot(log2(planner_memory$our), log2(planner_memory$sat),
             subtitle = TeX("$M$")) + xlim(6, 13) + ylim(6, 13)

mean_ops_by_constraint <- tibble(our = our$mean_ops_by_constraint, sat = sat$mean_ops_by_constraint) %>%
  filter(!is.na(our), !is.na(sat))
g3 <- scatter_plot(log2(mean_ops_by_constraint$our), log2(mean_ops_by_constraint$sat), 
             x_label = 'OpSearch',
             subtitle = TeX("$\\bar{u}$"))

seqs_time <- tibble(our = our$total_seq_time, sat = sat$total_seq_time) %>%
  filter(!is.na(our), !is.na(sat))
g4 <- scatter_plot(log2(seqs_time$our), log2(seqs_time$sat),
             subtitle = TeX("$S_t$")) + xlim(-15, 10) + ylim(-15, 10)

total_time <- tibble(our = our$total_solve_time, sat = sat$total_solve_time) %>%
  filter(!is.na(our), !is.na(sat))
g5 <- scatter_plot(log2(total_time$our), log2(total_time$sat), 
             subtitle = TeX("$T_t$")) + xlim(-8, 13) + ylim(-8, 13)

all <- ggarrange(g1, g2, g3, g4, g5, nrow = 1)
ggsave('figs/main_scatter.pdf', plot = all, family = 'Times', 
       width = 30, height = 6)
