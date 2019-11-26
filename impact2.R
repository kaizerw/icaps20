source('common.R')

file_blind <- 'results/11_29/1_opsearch_blind.xlsx'
file_lmcut <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'
file_oc <- 'results/11_29/2_opsearch_oc_seq_landmarks.xlsx'

blind_summary <- read_results(file_blind, 'Geral', 0, 13)
lmcut_summary <- read_results(file_lmcut, 'Geral', 0, 13)
oc_summary <- read_results(file_oc, 'Geral', 0, 13)

coverages <- tribble(~heuristic, ~coverage, 
                     'blind', blind_summary[[12, 'solved']], 
                     'lmcut', lmcut_summary[[12, 'solved']],
                     'oc', oc_summary[[12, 'solved']])

blind_all <- read_all_results(file_blind, 'blind')
lmcut_all <- read_all_results(file_lmcut, 'LMCUT_T3')
oc_all <- read_all_results(file_oc, 'oc_seq_landmarks')

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

new_names <- c("$C$" = "coverage",
               "$S$" = "seqs",
               "$\\bar{S_t}$" = "total_seq_time",
               "$\\bar{T_t}$" = "total_solve_time",
               "$\\bar{M}$" = "planner_memory",
               "$\\bar{u}$" = "mean_ops_by_constraint")

summaries <- bind_rows(solved_by_blind, solved_by_lmcut, solved_by_oc) %>%
  bind_cols(coverages) %>% 
  select(-heuristic1) %>%
  rename(!!!new_names)

save_table(
  summaries,
  'Comparison using different heuristic functions',
  'summary_heuristics2',
  environment = 'table'
)
