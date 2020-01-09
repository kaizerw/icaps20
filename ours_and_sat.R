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

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

our <- read_results(filename, 'Geral', 0, 12, rows_to_keep = rows_to_keep)
sat <- read_results(filename, 'Geral', 14, 12, rows_to_keep = rows_to_keep)

our <- our %>% mutate(perc = total_seq_time / total_solve_time * 100)
sat <- sat %>% mutate(perc = total_seq_time / total_solve_time * 100)

our <- our %>% mutate(domain = lapply(domain, FUN=texttt))
sat <- sat %>% mutate(domain = lapply(domain, FUN=texttt))

new_names <- c("${\\scriptstyle C}$" = "solved",
               "${\\scriptstyle S}$" = "seqs",
               "${\\scriptstyle R}$" = "restarts",
               "${\\scriptstyle \\bar{S_t}}$" = "total_seq_time",
               "${\\scriptstyle \\bar{T_t}}$" = "total_solve_time",
               "${\\scriptstyle \\bar{p}}$" = "perc",
               "${\\scriptstyle \\bar{M}}$" = "planner_memory",
               "${\\scriptstyle \\bar{u}}$" = "mean_ops_by_constraint",
               "${\\scriptstyle bb}$" = "best_bound")

our <- our %>% rename(!!new_names) %>% select(-"${\\scriptstyle \\bar{S_t}}$")
sat <- sat %>% rename(!!new_names) %>% select(-"${\\scriptstyle \\bar{S_t}}$")

save_table(sat, 'OpSeq.',         'sat', environment = 'table', only.contents = T, 
           hline.after = c(-1, 0, nrow(sat) - 1, nrow(sat)), digits = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1))
save_table(our, '\\oursolver{}.', 'our', environment = 'table', only.contents = T, 
           hline.after = c(-1, 0, nrow(our) - 1, nrow(our)), digits = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1))

