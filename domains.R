source('common.R')

filename <- 'results/11_08/cat_by_cost.xlsx'
cat_by_cost <- read_excel(filename, range = 'A1:G12')

filename <- 'results/11_14/3_initial_relaxation_values_1m.xlsx'
h_init <- read_excel(filename, range = 'A1:O12') %>% 
  select(instance, initial_lp_solution) %>%
  mutate(instance = str_replace(instance, 'T3.*/', '')) %>%
  mutate(instance = str_replace(instance, '-opt11-strips', '')) %>%
  rename(domain = instance)

filename <- 'results/11_14/7_lp_size.xlsx'
lp_size <- read_excel(filename, range = 'A1:D12') %>%
  select(-solved) %>%
  mutate(instance = str_replace(instance, 'T3.*/', '')) %>%
  mutate(instance = str_replace(instance, '-opt11-strips', '')) %>%
  rename(domain = instance)

filename <- 'results/11_14/0_ignore_zero_cost_ops.xlsx'
others <- read_excel(filename, range = 'A1:AV12') %>%
  select(instance, optimal_plan_cost, ops, variables) %>%
  mutate(instance = str_replace(instance, 'LMCUT.*/', '')) %>%
  mutate(instance = str_replace(instance, '-opt11-strips', '')) %>%
  rename(domain = instance)

new_names <- c("${\\scriptstyle zco}$" = "any_zero_cost_ops",
               "${\\scriptstyle \\overline{c_{min}}}$" = "min_op_cost", 
               "${\\scriptstyle \\overline{c_{max}}}$" = "max_op_cost",
               "${\\scriptstyle c_{min}}$" = "min_cost", 
               "${\\scriptstyle c_{max}}$" = "max_cost",
               "${\\scriptstyle \\overline{|\\mathcal{V}|}}$" = "variables",
               "${\\scriptstyle \\overline{|O|}}$" = "ops",
               "${\\scriptstyle \\overline{lb}}$" = "optimal_plan_cost",
               "${\\scriptstyle \\overline{z_0}}$" = "initial_lp_solution",
               "${\\scriptstyle \\overline{r_0}}$" = "initial_lp_rows",
               "${\\scriptstyle \\overline{c_0}}$" = "initial_lp_cols")

all <- bind_cols(cat_by_cost, h_init, lp_size, others) %>%
  select(-domain1, -domain2, -domain3, -all_unit_cost_ops) %>%
  mutate(any_zero_cost_ops = ifelse(any_zero_cost_ops == 20, '$\\checkmark$', '$-$')) %>%
  mutate(min_op_cost = as.integer(min_op_cost), max_op_cost = as.integer(max_op_cost),
         min_cost = as.integer(min_cost), max_cost = as.integer(max_cost)) %>%
  rename(!!new_names) %>%
  select(domain, "${\\scriptstyle \\overline{|\\mathcal{V}|}}$", "${\\scriptstyle \\overline{|O|}}$", "${\\scriptstyle zco}$", 
         "${\\scriptstyle \\overline{c_{min}}}$", "${\\scriptstyle \\overline{c_{max}}}$", "${\\scriptstyle c_{min}}$", "${\\scriptstyle c_{max}}$",
         "${\\scriptstyle \\overline{lb}}$", "${\\scriptstyle \\overline{z_0}}$", "${\\scriptstyle \\overline{r_0}}$", 
         "${\\scriptstyle \\overline{c_0}}$") %>%
  arrange(domain) %>% 
  mutate(domain = lapply(domain, FUN=texttt))

save_table(all, 'Information by domain.', 'domains',
           only.contents = T,
           digits = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 2, 2, 1, 1))
