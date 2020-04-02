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

#filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx' # results using CPLEX 12.8
filename <- 'results/cplex1210/1_opseq_opsearch.xlsx'    # results using CPLEX 12.10

our <- read_all_results(filename, 'OpSearch1', rows_to_keep = rows_to_keep) %>% filter(solved == 1)
sat <- read_all_results(filename, 'OpSeq1', rows_to_keep = rows_to_keep)    %>% filter(solved == 1)

our <- our %>% mutate(perc = total_seq_time / total_solve_time * 100)
sat <- sat %>% mutate(perc = total_seq_time / total_solve_time * 100)

both <- our %>%
  inner_join(sat, by = "instance", suffix = c('.OUR', '.SAT')) %>%
  select(-solved.OUR, -solved.SAT)

our <- select(both, instance, contains('.OUR')) %>% 
  rename_all( ~ str_replace_all(., '.OUR', '')) %>%
  make_summary('OpSearch')
sat <- select(both, instance, contains('.SAT')) %>%
  rename_all( ~ str_replace_all(., '.SAT', '')) %>%
  make_summary('OpSeq')

new_names <- c("${\\scriptstyle S}$" = "seqs",
               "${\\scriptstyle R}$" = "restarts",
               "${\\scriptstyle \\bar{S_t}}$" = "total_seq_time",
               "${\\scriptstyle \\bar{T_t}}$" = "total_solve_time",
               "${\\scriptstyle \\bar{p}}$" = "perc",
               "${\\scriptstyle \\bar{M}}$" = "planner_memory",
               "${\\scriptstyle \\bar{u}}$" = "mean_ops_by_constraint")

df <- bind_rows(our, sat) %>%
  mutate(perc = total_seq_time / total_solve_time * 100) %>%
  rename(!!new_names) %>%
  select(-"${\\scriptstyle \\bar{S_t}}$") %>%
  rename(" "="method")

save_table(
  df,
  'Summary for the 50 instances solved by both methods.',
  'summary_both',
  environment = 'table',
  only.contents = T,
  digits = c(0, 0, 0, 0, 0, 0, 0, 1)
)
