source('common.R')

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

our <- read_all_results(filename, 'LMCUT_T3')
sat <- read_all_results(filename, 'SAT')

seqs <- tibble(our = our$seqs, sat = sat$seqs) %>%
  filter(!is.na(our), !is.na(sat))
scatter_plot(seqs$our, seqs$sat, 0, 6000,
             x_label = 'S OpSearch', y_label = 'S SAT', name = 'seqs')

planner_memory <- tibble(our = our$planner_memory, sat = sat$planner_memory) %>%
  filter(!is.na(our), !is.na(sat))
scatter_plot(planner_memory$our, planner_memory$sat, 0, 4200,
             x_label = 'M OpSearch', y_label = 'M SAT', name = 'memory')

mean_ops_by_constraint <- tibble(our = our$mean_ops_by_constraint, sat = sat$mean_ops_by_constraint) %>%
  filter(!is.na(our), !is.na(sat))
scatter_plot(mean_ops_by_constraint$our, mean_ops_by_constraint$sat, 0, 71,
             x_label = '\\overline{u} OpSearch',
             y_label = '\\overline{u} SAT',
             name = 'mean_ops_by_constraint')
