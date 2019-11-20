source('common.R')

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

our <- read_all_results(filename, 'LMCUT_T3')
sat <- read_all_results(filename, 'SAT')

seqs <- tibble(our = our$seqs, sat = sat$seqs) %>%
  filter(!is.na(our), !is.na(sat))
g1 <- scatter_plot(seqs$our, seqs$sat, 0, 6000,
             y_label = 'OpSeq',
             subtitle = TeX("$S$"))

planner_memory <- tibble(our = our$planner_memory, sat = sat$planner_memory) %>%
  filter(!is.na(our), !is.na(sat))
g2 <- scatter_plot(planner_memory$our, planner_memory$sat, 0, 4200,
             subtitle = TeX("$M$"))

mean_ops_by_constraint <- tibble(our = our$mean_ops_by_constraint, sat = sat$mean_ops_by_constraint) %>%
  filter(!is.na(our), !is.na(sat))
g3 <- scatter_plot(mean_ops_by_constraint$our, mean_ops_by_constraint$sat, 0, 71,
             x_label = 'OpSearch',
             subtitle = TeX("$\\bar{u}$"))

seqs_time <- tibble(our = our$total_seq_time, sat = sat$total_seq_time) %>%
  filter(!is.na(our), !is.na(sat))
g4 <- scatter_plot(seqs_time$our, seqs_time$sat, 0, 3000,
             subtitle = TeX("$S_t$"))

total_time <- tibble(our = our$total_solve_time, sat = sat$total_solve_time) %>%
  filter(!is.na(our), !is.na(sat))
g5 <- scatter_plot(total_time$our, total_time$sat, 0, 3700,
             subtitle = TeX("$T_t$"))

#ggsave('figs/seqs.pdf', plot = g1, family = 'Times')
#ggsave('figs/memory.pdf', plot = g2, family = 'Times')
#ggsave('figs/mean_ops_by_constraint.pdf', plot = g3, family = 'Times')
#ggsave('figs/seqs_time.pdf', plot = g4, family = 'Times')
#ggsave('figs/total_time.pdf', plot = g5, family = 'Times')

all <- ggarrange(g1, g2, g3, g4, g5, nrow = 1)
ggsave('figs/main_scatter.pdf', plot = all, family = 'Times', 
       width = 30, height = 6)
