source('common.R')

filename <- 'results/11_14/6_selected_seq_landmarks_h+.xlsx'

blind_all <- read_all_results_heuristics(filename, 'T3_BLIND')
lmcut_all <- read_all_results_heuristics(filename, 'T3_LMCUT')
hstar_all <- read_all_results_heuristics(filename, 'T3_HSTAR')
#sat_all <- read_all_results_heuristics(filename, 'SAT')

solved_by_blind <- blind_all %>% filter(solved == 1)
solved_by_lmcut <- lmcut_all %>% filter(solved == 1)
solved_by_hstar <- hstar_all %>% filter(solved == 1)
#solved_by_sat <- sat_all %>% filter(solved == 1)

instances_solved_by_all <- solved_by_blind$instance %>% 
  intersect(solved_by_lmcut$instance) %>%
  intersect(solved_by_hstar$instance)
#%>% intersect(solved_by_sat$instance)

solved_by_blind <- solved_by_blind %>% 
  filter(instance %in% instances_solved_by_all)
solved_by_lmcut <- solved_by_lmcut %>% 
  filter(instance %in% instances_solved_by_all)
solved_by_hstar <- solved_by_hstar %>% 
  filter(instance %in% instances_solved_by_all)
#solved_by_sat <- solved_by_sat %>% 
#  filter(instance %in% instances_solved_by_all)

solved_by_blind <- solved_by_blind %>%
  summarise(
    heuristic = 'blind',
    seqs = sum(seqs),
    seq_time = sum(total_seq_time),
    solve_time = sum(total_solve_time),
    memory = mean(planner_memory),
    cut_size = mean(mean_ops_by_constraint)
  )
solved_by_lmcut <- solved_by_lmcut %>%
  summarise(
    heuristic = 'lmcut',
    seqs = sum(seqs),
    seq_time = sum(total_seq_time),
    solve_time = sum(total_solve_time),
    memory = mean(planner_memory),
    cut_size = mean(mean_ops_by_constraint)
  )
solved_by_hstar <- solved_by_hstar %>%
  summarise(
    heuristic = 'hstar',
    seqs = sum(seqs),
    seq_time = sum(total_seq_time),
    solve_time = sum(total_solve_time),
    memory = mean(planner_memory),
    cut_size = mean(mean_ops_by_constraint)
  )
#solved_by_sat <- solved_by_sat %>%
#  summarise(
#    heuristic = 'sat',
#    seqs = sum(seqs),
#    seq_time = sum(total_seq_time),
#    solve_time = sum(total_solve_time),
#    memory = mean(planner_memory),
#    cut_size = mean(mean_ops_by_constraint)
#  )

blind_summary <- read_results(filename, 'Geral', 0, 7)
lmcut_summary <- read_results(filename, 'Geral', 8, 7)
hstar_summary <- read_results(filename, 'Geral', 16, 7)
#sat_summary <- read_results(filename, 'Geral', 24, 7)

coverages <- tribble(~heuristic, ~coverage, 
                     'blind', blind_summary[[6, 'solved']], 
                     'lmcut', lmcut_summary[[6, 'solved']],
                     'hstar', hstar_summary[[6, 'solved']])

#coverages <- tribble(~heuristic, ~coverage, 
#                    'blind', blind_summary[[6, 'solved']], 
#                    'lmcut', lmcut_summary[[6, 'solved']],
#                    'hstar', hstar_summary[[6, 'solved']],
#                    'sat', sat_summary[[6, 'solved']])

#summaries <- bind_rows(solved_by_blind, solved_by_lmcut, solved_by_hstar, solved_by_sat) %>%
summaries <- bind_rows(solved_by_blind, solved_by_lmcut, solved_by_hstar) %>%
  bind_cols(coverages) %>% 
  select(-heuristic1) %>%
  transpose_df()
save_table(
  summaries,
  'Comparison using different heuristic functions',
  'summary_heuristics',
  environment = 'table'
)
