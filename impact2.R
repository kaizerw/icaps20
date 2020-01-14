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
    'mean_ops_by_constraint'
  )
}

file_blind <- 'results/11_29/1_opsearch_blind.xlsx'
file_lmcut <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'
file_oc <- 'results/11_29/2_opsearch_oc_seq_landmarks.xlsx'

blind_summary <- read_results(file_blind, 'Geral', 0, 13, rows_to_keep = rows_to_keep)
lmcut_summary <- read_results(file_lmcut, 'Geral', 0, 13, rows_to_keep = rows_to_keep)
oc_summary <- read_results(file_oc, 'Geral', 0, 13, rows_to_keep = rows_to_keep)

blind_summary <- blind_summary %>% mutate(perc = total_seq_time / total_solve_time * 100)
lmcut_summary <- lmcut_summary %>% mutate(perc = total_seq_time / total_solve_time * 100)
oc_summary <- oc_summary %>% mutate(perc = total_seq_time / total_solve_time * 100)

coverages <- tribble(~heuristic, ~coverage, 
                     'blind', blind_summary[[12, 'solved']], 
                     'lmcut', lmcut_summary[[12, 'solved']],
                     'oc', oc_summary[[12, 'solved']])

blind_all <- read_all_results(file_blind, 'blind', rows_to_keep = rows_to_keep)
lmcut_all <- read_all_results(file_lmcut, 'LMCUT_T3', rows_to_keep = rows_to_keep)
oc_all <- read_all_results(file_oc, 'oc_seq_landmarks', rows_to_keep = rows_to_keep)

blind_all <- blind_all %>% mutate(perc = total_seq_time / total_solve_time * 100)
lmcut_all <- lmcut_all %>% mutate(perc = total_seq_time / total_solve_time * 100)
oc_all <- oc_all %>% mutate(perc = total_seq_time / total_solve_time * 100)

solved_by_blind <- blind_all %>% filter(solved == 1)
solved_by_lmcut <- lmcut_all %>% filter(solved == 1)
solved_by_oc <- oc_all %>% filter(solved == 1)

solved_by_all <- solved_by_blind$instance %>% 
  intersect(solved_by_lmcut$instance) %>%
  intersect(solved_by_oc$instance)

solved_by_blind <- solved_by_blind %>% filter(instance %in% solved_by_all)
solved_by_lmcut <- solved_by_lmcut %>% filter(instance %in% solved_by_all)
solved_by_oc <- solved_by_oc %>% filter(instance %in% solved_by_all)

solved_by_blind <- make_summary(solved_by_blind, 'blind') %>% rename(heuristic = method)
solved_by_lmcut <- make_summary(solved_by_lmcut, 'lmcut') %>% rename(heuristic = method)
solved_by_oc <- make_summary(solved_by_oc, 'oc') %>% rename(heuristic = method)

new_names <- c("${\\scriptstyle C}$" = "coverage",
               "${\\scriptstyle S}$" = "seqs",
               "${\\scriptstyle R}$" = "restarts",
               "${\\scriptstyle \\bar{S_t}}$" = "total_seq_time",
               "${\\scriptstyle \\bar{T_t}}$" = "total_solve_time",
               "${\\scriptstyle \\bar{p}}$" = "perc",
               "${\\scriptstyle \\bar{M}}$" = "planner_memory",
               "${\\scriptstyle \\bar{u}}$" = "mean_ops_by_constraint")

summaries <- bind_rows(solved_by_blind, solved_by_lmcut, solved_by_oc) %>%
  bind_cols(coverages) %>% 
  select(-heuristic1) %>%
  rename(!!!new_names) %>%
  select(-"${\\scriptstyle \\bar{S_t}}$") %>%
  select("heuristic", "${\\scriptstyle C}$", "${\\scriptstyle S}$", "${\\scriptstyle R}$", "${\\scriptstyle \\bar{T_t}}$", 
         "${\\scriptstyle \\bar{M}}$", "${\\scriptstyle \\bar{u}}$", "${\\scriptstyle \\bar{p}}$") %>%
  rename(" "="heuristic")

save_table(
  summaries,
  'Comparison using different heuristic functions',
  'summary_heuristics2',
  environment = 'table',
  only.contents = T,
  digits = c(0, 0, 0, 0, 0, 0, 0, 0, 1)
)
