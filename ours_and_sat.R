source('common.R')

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

our <- read_results(filename, 'Geral', 0, 12)
sat <- read_results(filename, 'Geral', 14, 12)

new_names <- c("$C$" = "solved",
               "$S$" = "seqs",
               "$R$" = "restarts",
               "$\\overline{S_t}$" = "total_seq_time",
               "$\\overline{T_t}$" = "total_solve_time",
               "$\\overline{M}$" = "planner_memory",
               "$\\overline{u}$" = "mean_ops_by_constraint")

our <- our %>% rename(!!new_names)
sat <- sat %>% rename(!!new_names)

save_table(our, '\\oursolver{}', 'our', environment = 'table')
save_table(sat, 'SAT approach',  'sat', environment = 'table')

