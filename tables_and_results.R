library(tidyverse)
library(xtable)
library(readxl)
library(ggplot2)

source("common.R")

save_table <-
  function(df, caption, label, file, environment = 'table*') {
    print(
      xtable(
        df,
        digits = 2,
        auto = TRUE,
        caption = caption,
        label = label
      ),
      size = "small",
      table.placement = "htbp",
      include.rownames = FALSE,
      floating.environment = environment,
      file = file
    )
  }

transpose_df <- function(df) {
  df %>%
    gather(key = '', value = value, 2:ncol(df)) %>%
    spread_(key = names(df)[1], value = 'value')
}


###############################################################################

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

sss <- read_results(filename, 'Geral', 0, 12)
sat <- read_results(filename, 'Geral', 14, 12)

save_table(
  sss,
  '\\oursolver{} ignoring zero cost operators',
  'tab:our_ignoring',
  'tabs/our_ignoring.tex'
)
save_table(sat, 'Our SAT implementation', 'tab:sat', 'tabs/sat.tex')

###############################################################################

filename <- 'results/11_08/0B_landmarks_h+_seq.xlsx'

sss <- read_results(filename, 'Geral', 0, 12)

save_table(
  sss,
  '\\oursolver{} with zero cost operators',
  'tab:our_with_zero',
  'tabs/our_with_zero.tex'
)

###############################################################################

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

sss <- read_all_results(filename, 'LMCUT_T3')
sat <- read_all_results(filename, 'SAT')

sss_solved <- filter(sss, solved == 1)
sat_solved <- filter(sat, solved == 1)
solved_by_both <- sss_solved %>%
  inner_join(sat_solved, by = "instance", suffix = c('.SSS', '.SAT')) %>%
  select(-solved.SSS, -solved.SAT, -restarts.SSS, -restarts.SAT)

sss_solved <- select(solved_by_both, instance, contains('.SSS'))
sat_solved <- select(solved_by_both, instance, contains('.SAT'))

sss_solved <- sss_solved %>%
  rename_all( ~ stringr::str_replace_all(., '.SSS', ''))
sat_solved <- sat_solved %>%
  rename_all( ~ stringr::str_replace_all(., '.SAT', ''))

save_table(
  sss_solved,
  '\\oursolver{}: instances solved by both (ignoring zero cost operators)',
  'tab:our_both_ignoring',
  'tabs/our_both_ignoring.tex'
)

save_table(
  sat_solved,
  'SAT instances: solved by both (ignoring zero cost operators)',
  'tab:sat_both',
  'tabs/sat_both.tex'
)



sss_solved <- sss_solved %>%
  summarise(
    method = 'OpSearch',
    seqs = sum(seqs),
    total_seq_time = sum(total_seq_time),
    total_solve_time = sum(total_solve_time),
    planner_memory = mean(planner_memory),
    mean_ops_by_constraint = mean(mean_ops_by_constraint)
  )

sat_solved <- sat_solved %>%
  summarise(
    method = 'SAT',
    seqs = sum(seqs),
    total_seq_time = sum(total_seq_time),
    total_solve_time = sum(total_solve_time),
    planner_memory = mean(planner_memory),
    mean_ops_by_constraint = mean(mean_ops_by_constraint)
  )

summaries <- bind_rows(sss_solved, sat_solved) %>%
  transpose_df()
save_table(
  summaries,
  'Solved by both (ignoring zero cost operators)',
  'tab:summary_both',
  'tabs/summary_both.tex',
  environment = 'table'
)



solved_by_both <- solved_by_both %>%
  filter(planner_memory.SSS < 500000, planner_memory.SAT < 500000)

scatter_plot(
  solved_by_both$seqs.SSS,
  solved_by_both$seqs.SAT,
  0,
  700,
  0,
  700,
  x_label = 'seqs SSS',
  y_label = 'seqs SAT',
  title = 'seqs'
)

scatter_plot(
  solved_by_both$total_seq_time.SSS,
  solved_by_both$total_seq_time.SAT,
  0,
  1000,
  0,
  1000,
  x_label = 'total_seq_time SSS',
  y_label = 'total_seq_time SAT',
  title = 'total_seq_time'
)

scatter_plot(
  solved_by_both$planner_memory.SSS,
  solved_by_both$planner_memory.SAT,
  50000,
  350000,
  50000,
  350000,
  x_label = 'memory SSS',
  y_label = 'memory SAT',
  title = 'memory'
)

scatter_plot(
  solved_by_both$mean_ops_by_constraint.SSS,
  solved_by_both$mean_ops_by_constraint.SAT,
  -0.1,
  0.8,-0.1,
  0.8,
  x_label = 'mean_ops_by_constraint SSS',
  y_label = 'mean_ops_by_constraint SAT',
  title = 'mean_ops_by_constraint'
)

###############################################################################

filename_default <- 'results/11_14/1_emphasis_default_20m.xlsx'
filename_bestbound <- 'results/11_14/1_emphasis_bestbound_20m.xlsx'

default_sss <- read_results(filename_default, 'Geral', 0, 12)
default_sat <- read_results(filename_default, 'Geral', 14, 12)

best_bound_sss <- read_results(filename_bestbound, 'Geral', 0, 12)
best_bound_sat <- read_results(filename_bestbound, 'Geral', 14, 12)

save_table(
  default_sss,
  '\\oursolver{} with balanced emphasis',
  'tab:default_our',
  'tabs/default_our.tex'
)
save_table(default_sat,
           'SAT with balanced emphasis',
           'tab:default_sat',
           'tabs/default_sat.tex')
save_table(
  best_bound_sss,
  '\\oursolver{} with best bound emphasis',
  'tab:best_bound_our',
  'tabs/best_bound_our.tex'
)
save_table(
  best_bound_sat,
  'SAT with best bound emphasis',
  'tab:best_bound_sat',
  'tabs/best_bound_sat.tex'
)

scatter_plot(
  default_sss$seqs,
  best_bound_sss$seqs,
  0,
  70000,
  0,
  70000,
  x_label = 'seqs SSS default emphasis',
  y_label = 'seqs SSS best bound emphasis',
  title = 'SSS_seqs'
)

scatter_plot(
  default_sss$restarts,
  best_bound_sss$restarts,
  0,
  70,
  0,
  70,
  x_label = 'restarts SSS default emphasis',
  y_label = 'restarts SSS best bound emphasis',
  title = 'SSS_restarts'
)

scatter_plot(
  default_sat$seqs,
  best_bound_sat$seqs,
  0,
  30000,
  0,
  30000,
  x_label = 'seqs SAT default emphasis',
  y_label = 'seqs SAT best bound emphasis',
  title = 'SAT_seqs'
)

scatter_plot(
  default_sat$restarts,
  best_bound_sat$restarts,
  0,
  25,
  0,
  25,
  x_label = 'restarts SAT default emphasis',
  y_label = 'restarts SAT best bound emphasis',
  title = 'SAT_restarts'
)

###############################################################################

filename <- 'results/11_14/6_selected_seq_landmarks_h+.xlsx'

blind <- read_results(filename, 'Geral', 0, 7)
lmcut <- read_results(filename, 'Geral', 8, 7)
hstar <- read_results(filename, 'Geral', 16, 7)
sat <- read_results(filename, 'Geral', 24, 7)

save_table(
  blind,
  '\\oursolver{} with blind heuristic',
  'tab:selected_blind',
  'tabs/selected_blind.tex'
)
save_table(
  lmcut,
  '\\oursolver{} with LM-Cut heuristic',
  'tab:selected_lmcut',
  'tabs/selected_lmcut.tex'
)
save_table(
  hstar,
  '\\oursolver{} with \\hstar{} heuristic',
  'tab:selected_hstar',
  'tabs/selected_hstar.tex'
)
save_table(sat,
           'Our SAT implementation',
           'tab:selected_sat',
           'tabs/selected_sat.tex')

blind <- blind %>%
  summarise(
    heuristic = 'blind',
    seqs = sum(seqs),
    seq_time = sum(total_seq_time),
    solve_time = sum(total_solve_time),
    memory = mean(planner_memory),
    cut_size = mean(mean_ops_by_constraint)
  )
lmcut <- lmcut %>%
  summarise(
    heuristic = 'lmcut',
    seqs = sum(seqs),
    seq_time = sum(total_seq_time),
    solve_time = sum(total_solve_time),
    memory = mean(planner_memory),
    cut_size = mean(mean_ops_by_constraint)
  )
hstar <- hstar %>%
  summarise(
    heuristic = 'hstar',
    seqs = sum(seqs),
    seq_time = sum(total_seq_time),
    solve_time = sum(total_solve_time),
    memory = mean(planner_memory),
    cut_size = mean(mean_ops_by_constraint)
  )
sat <- sat %>%
  summarise(
    heuristic = 'sat',
    seqs = sum(seqs),
    seq_time = sum(total_seq_time),
    solve_time = sum(total_solve_time),
    memory = mean(planner_memory),
    cut_size = mean(mean_ops_by_constraint)
  )

summary <- bind_rows(blind, lmcut, hstar, sat) %>%
  transpose_df()
save_table(
  summary,
  'Comparison using different heuristic functions',
  'tab:summary_heuristics',
  'tabs/summary_heuristics.tex',
  environment = 'table'
)

###############################################################################
