library(tidyverse)
library(xtable)
library(readxl)
library(ggplot2)

source("common.R")

save_table <- function(df, caption, label, file) {
  print(xtable(df, digits = 3, auto = TRUE, 
               caption = caption,
               label = label), 
        include.rownames = FALSE,
        floating.environment = "table*",
        file = file)
}


###############################################################################

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

sss <- read_results(filename, 'Geral', 0, 12)
sat <- read_results(filename, 'Geral', 14, 12)

save_table(sss, '\\oursolver{} ignoring zero cost operators', 
           'tab:our_ignoring', 'tables/our_ignoring.tex')
save_table(sat, 'Our SAT implementation', 'tab:sat', 'tables/sat.tex')

###############################################################################

filename <- 'results/11_08/0B_landmarks_h+_seq.xlsx'

sss <- read_results(filename, 'Geral', 0, 12)

save_table(sss, '\\oursolver{} with zero cost operators', 
           'tab:our_with_zero', 'tables/our_with_zero.tex')

###############################################################################


sss <- read_all_results(filename, 'LMCUT_T3')
sat <- read_all_results(filename, 'SAT')

sss_solved <- filter(sss, solved == 1)
sat_solved <- filter(sat, solved == 1)
solved_by_both <- sss_solved %>% 
  inner_join(sat_solved, by = "instance", suffix = c('.SSS', '.SAT')) %>%
  select(-solved.SSS, -solved.SAT, -restarts.SSS, -restarts.SAT)

save_table(select(solved_by_both, instance, contains('.SSS')), 
           '\\oursolver{}: instances solved by both (ignoring zero cost operators)', 
           'tab:our_both_ignoring', 'tables/our_both_ignoring.tex')

save_table(select(solved_by_both, instance, contains('.SAT')), 
           'SAT instances: solved by both (ignoring zero cost operators)', 
           'tab:sat_both', 'tables/sat_both.tex')



scatter_plot(solved_by_both$seqs.SSS, 
             solved_by_both$seqs.SAT, 
             x_label = 'seqs SSS', 
             y_label = 'seqs SAT', 
             title = 'seqs')

scatter_plot(solved_by_both$total_seq_time.SSS, 
             solved_by_both$total_seq_time.SAT, 
             x_label = 'total_seq_time SSS', 
             y_label = 'total_seq_time SAT', 
             title = 'total_seq_time')

scatter_plot(solved_by_both$planner_memory.SSS, 
             solved_by_both$planner_memory.SAT, 
             x_label = 'memory SSS', 
             y_label = 'memory SAT', 
             title = 'memory')

scatter_plot(solved_by_both$mean_ops_by_constraint.SSS, 
             solved_by_both$mean_ops_by_constraint.SAT, 
             x_label = 'mean_ops_by_constraint SSS', 
             y_label = 'mean_ops_by_constraint SAT', 
             title = 'mean_ops_by_constraint')
