# THIS FILE IS SIMILAR TO IMPACT3.R; I HAVE ONLY ADDED SAT IN THE TABLES

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


file <- 'results/12_17/4_blind_lmcut_oc_hstar_sat.xlsx'

summary_blind <- read_results(file, 'Geral', 0, 37, rows_to_keep = rows_to_keep)
summary_lmcut <- read_results(file, 'Geral', 38, 37, rows_to_keep = rows_to_keep)
summary_oc <- read_results(file, 'Geral', 76, 37, rows_to_keep = rows_to_keep)
summary_hstar <- read_results(file, 'Geral', 114, 37, rows_to_keep = rows_to_keep)
summary_sat <- read_results(file, 'Geral', 152, 37, rows_to_keep = rows_to_keep)

summary_blind <- summary_blind %>% mutate(perc = total_seq_time / total_solve_time * 100)
summary_lmcut <- summary_lmcut %>% mutate(perc = total_seq_time / total_solve_time * 100)
summary_oc <- summary_oc %>% mutate(perc = total_seq_time / total_solve_time * 100)
summary_hstar <- summary_hstar %>% mutate(perc = total_seq_time / total_solve_time * 100)
summary_sat <- summary_sat %>% mutate(perc = total_seq_time / total_solve_time * 100)

coverages <- tribble(~heuristic, ~coverage, 
                     'blind', summary_blind[[36, 'solved']], 
                     'lmcut', summary_lmcut[[36, 'solved']],
                     'oc', summary_oc[[36, 'solved']],
                     'hstar', summary_hstar[[36, 'solved']],
                     'sat', summary_sat[[36, 'solved']])

all_blind <- read_all_results_heuristics2(file, 'blind', rows_to_keep = rows_to_keep)
all_lmcut <- read_all_results_heuristics2(file, 'lmcut', rows_to_keep = rows_to_keep)
all_oc <- read_all_results_heuristics2(file, 'oc_seq_landmarks', rows_to_keep = rows_to_keep)
all_hstar <- read_all_results_heuristics2(file, 'hstar', rows_to_keep = rows_to_keep)
all_sat <- read_all_results_heuristics2(file, 'SAT', rows_to_keep = rows_to_keep)

all_blind <- all_blind %>% mutate(perc = total_seq_time / total_solve_time * 100)
all_lmcut <- all_lmcut %>% mutate(perc = total_seq_time / total_solve_time * 100)
all_oc <- all_oc %>% mutate(perc = total_seq_time / total_solve_time * 100)
all_hstar <- all_hstar %>% mutate(perc = total_seq_time / total_solve_time * 100)
all_sat <- all_sat %>% mutate(perc = total_seq_time / total_solve_time * 100)

solved_by_blind <- all_blind %>% filter(solved == 1)
solved_by_lmcut <- all_lmcut %>% filter(solved == 1)
solved_by_oc <- all_oc %>% filter(solved == 1)
solved_by_hstar <- all_hstar %>% filter(solved == 1)
solved_by_sat <- all_sat %>% filter(solved == 1)

solved_by_all <- solved_by_blind$instance %>% 
  intersect(solved_by_lmcut$instance) %>%
  intersect(solved_by_oc$instance) %>%
  intersect(solved_by_hstar$instance) %>%
  intersect(solved_by_sat$instance)

solved_by_blind <- solved_by_blind %>% filter(instance %in% solved_by_all)
solved_by_lmcut <- solved_by_lmcut %>% filter(instance %in% solved_by_all)
solved_by_oc <- solved_by_oc %>% filter(instance %in% solved_by_all)
solved_by_hstar <- solved_by_hstar %>% filter(instance %in% solved_by_all)
solved_by_sat <- solved_by_sat %>% filter(instance %in% solved_by_all)

solved_by_blind <- make_summary(solved_by_blind, 'blind') %>% rename(heuristic = method)
solved_by_lmcut <- make_summary(solved_by_lmcut, 'lmcut') %>% rename(heuristic = method)
solved_by_oc <- make_summary(solved_by_oc, 'oc') %>% rename(heuristic = method)
solved_by_hstar <- make_summary(solved_by_hstar, '\\hstar{}') %>% rename(heuristic = method)
solved_by_sat <- make_summary(solved_by_sat, 'sat') %>% rename(heuristic = method)

new_names <- c("${\\scriptstyle C}$" = "coverage",
               "${\\scriptstyle S}$" = "seqs",
               "${\\scriptstyle R}$" = "restarts",
               "${\\scriptstyle \\bar{S_t}}$" = "total_seq_time",
               "${\\scriptstyle \\bar{T_t}}$" = "total_solve_time",
               "${\\scriptstyle \\bar{p}}$" = "perc",
               "${\\scriptstyle \\bar{M}}$" = "planner_memory",
               "${\\scriptstyle \\bar{u}}$" = "mean_ops_by_constraint")

summaries <- bind_rows(solved_by_blind, solved_by_lmcut, solved_by_oc, solved_by_hstar, solved_by_sat) %>%
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
  'summary_heuristics5',
  environment = 'table',
  only.contents = T,
  digits = c(0, 0, 0, 0, 0, 0, 0, 0, 1)
)
