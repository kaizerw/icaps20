source('common.R')

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

our <- read_results(filename, 'Geral', 0, 12)
sat <- read_results(filename, 'Geral', 14, 12)

new_names <- c("$C$" = "solved",
               "$S$" = "seqs",
               "$R$" = "restarts",
               "$\\bar{S_t}$" = "total_seq_time",
               "$\\bar{T_t}$" = "total_solve_time",
               "$\\bar{M}$" = "planner_memory",
               "$\\bar{u}$" = "mean_ops_by_constraint")

our <- our %>% rename(!!new_names)
sat <- sat %>% rename(!!new_names)

save_table(sat, 'OpSeq',         'sat', environment = 'table', only.contents = T, hline.after = c(-1, 0, nrow(sat) - 1, nrow(sat)))
our <- select(our, -domain)
save_table(our, '\\oursolver{}', 'our', environment = 'table', only.contents = T, hline.after = c(-1, 0, nrow(our) - 1, nrow(our)))
