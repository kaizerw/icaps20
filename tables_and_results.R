library(tidyverse)
library(xtable)
library(readxl)
library(ggplot2)

source("common.R")

###############################################################################

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

###############################################################################

sss <- read_results(filename, 'Geral', 0, 12)
sat <- read_results(filename, 'Geral', 14, 12)

print(xtable(sss, digits = 3, auto = TRUE, caption = '\\oursolver{} with ignore zero costs trick'), include.rownames = FALSE)
print(xtable(sat, digits = 3, auto = TRUE, caption = 'Our SAT implementation'), include.rownames = FALSE)

###############################################################################

sss <- read_all_results(filename, 'LMCUT_T3')
sat <- read_all_results(filename, 'SAT')

sss_solved <- filter(sss, solved == 1)
sat_solved <- filter(sat, solved == 1)
solved_by_both <- sss_solved %>% 
  inner_join(sat_solved, by = "instance", suffix = c('.SSS', '.SAT')) %>%
  select(-solved.SSS, -solved.SAT)

print(xtable(solved_by_both, digits = 3, auto = TRUE, caption = 'Instances solved by both (with ignore zero costs trick)'), include.rownames = FALSE)

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
