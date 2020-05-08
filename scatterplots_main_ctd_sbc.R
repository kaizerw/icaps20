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

total_time <- tibble(our = our$total_solve_time, sat = sat$total_solve_time) %>%
  filter(!is.na(our), !is.na(sat)) %>% mutate(our = our + 1, sat = sat + 1)
g1 <- scatter_plot(log2(total_time$sat), log2(total_time$our), 
                   y_label = 'OpSearch',
                   subtitle = TeX("Total Time"), fontsize = 50, pointsize=7) + xlim(0, 13) + ylim(0, 13)

planner_memory <- tibble(our = our$planner_memory, sat = sat$planner_memory) %>%
  filter(!is.na(our), !is.na(sat)) %>% mutate(our = our + 1, sat = sat + 1)
g2 <- scatter_plot(log2(planner_memory$sat), log2(planner_memory$our),
                   x_label="OpSeq",
                   subtitle = TeX("Memory Used"), fontsize = 50, pointsize=7) + xlim(6, 13) + ylim(6, 13)

mean_ops_by_constraint <- tibble(our = our$mean_ops_by_constraint, sat = sat$mean_ops_by_constraint) %>%
  filter(!is.na(our), !is.na(sat)) %>% mutate(our = our + 1, sat = sat + 1)
g3 <- scatter_plot(log2(mean_ops_by_constraint$sat), log2(mean_ops_by_constraint$our), 
                   subtitle = TeX("$\\%$ ops in Constraint"), fontsize = 50, pointsize=7) + xlim(0, 7) + ylim(0, 7)

all <- ggarrange(g1, g2, g3, nrow = 1)
ggsave('figs/main_scatter_ctd_sbc.pdf', plot = all, family = 'Times', width = 25, height = 8)
