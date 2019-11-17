source('common.R')

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

our <- read_results(filename, 'Geral', 0, 12)
sat <- read_results(filename, 'Geral', 14, 12)

save_table(our, '\\oursolver{}', 'our')
save_table(sat, 'SAT approach',  'sat')
