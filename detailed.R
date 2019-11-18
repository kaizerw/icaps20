source('common.R')

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'

our <- read_all_results(filename, 'LMCUT_T3') %>% filter(solved == 1)
sat <- read_all_results(filename, 'SAT')      %>% filter(solved == 1)

both <- our %>%
  inner_join(sat, by = "instance", suffix = c('.OUR', '.SAT')) %>%
  select(-solved.OUR, -solved.SAT, -restarts.OUR, -restarts.SAT)

our <- select(both, instance, contains('.OUR')) %>% 
  rename_all( ~ str_replace_all(., '.OUR', '')) %>%
  make_summary('OpSearch')
sat <- select(both, instance, contains('.SAT')) %>%
  rename_all( ~ str_replace_all(., '.SAT', '')) %>%
  make_summary('SAT')

save_table(
  bind_rows(our, sat) %>% transpose_df(),
  'Solved by both',
  'summary_both',
  environment = 'table'
)
