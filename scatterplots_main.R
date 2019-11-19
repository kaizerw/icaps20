source('common.R')

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

our <- read_all_results(filename, 'LMCUT_T3')
sat <- read_all_results(filename, 'SAT')

seqs <- tibble(our = our$seqs, sat = sat$seqs) %>%
  filter(!is.na(our), !is.na(sat))
scatter_plot(seqs$our, seqs$sat, 0, 6000,
             x_label = TeX("$S$ OpSearch"), y_label = TeX("$S$ OpSeq"), name = 'seqs')

planner_memory <- tibble(our = our$planner_memory, sat = sat$planner_memory) %>%
  filter(!is.na(our), !is.na(sat))
scatter_plot(planner_memory$our, planner_memory$sat, 0, 4200,
             x_label = TeX("$M$ OpSearch"), y_label = TeX("$M$ OpSeq"), name = 'memory')

mean_ops_by_constraint <- tibble(our = our$mean_ops_by_constraint, sat = sat$mean_ops_by_constraint) %>%
  filter(!is.na(our), !is.na(sat))
scatter_plot(mean_ops_by_constraint$our, mean_ops_by_constraint$sat, 0, 71,
             x_label = TeX("\\bar{u} OpSearch"),
             y_label = TeX("\\bar{u} OpSeq"),
             name = 'mean_ops_by_constraint')

seqs_time <- tibble(our = our$total_seq_time, sat = sat$total_seq_time) %>%
  filter(!is.na(our), !is.na(sat))
scatter_plot(seqs_time$our, seqs_time$sat, 0, 3000,
             x_label = TeX("$S_t$ OpSearch"), y_label = TeX("$T_t$ OpSeq"), name = 'seqs_time')

total_time <- tibble(our = our$total_solve_time, sat = sat$total_solve_time) %>%
  filter(!is.na(our), !is.na(sat))
scatter_plot(total_time$our, total_time$sat, 0, 3700,
             x_label = TeX("$S_t$ OpSearch"), y_label = TeX("$T_t$ OpSeq"), name = 'total_time')
