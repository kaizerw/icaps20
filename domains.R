source('common.R')

filename <- 'results/11_08/cat_by_cost.xlsx'
cat_by_cost <- read_excel(filename, range = 'A1:E12')

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

new_names <- c("$zco$" = "any_zero_cost_ops",
               "$\\overline{c_{min}}$" = "min_op_cost", 
               "$\\overline{c_{max}}$" = "max_op_cost",
               "$\\overline{|V|}$" = "variables",
               "$\\overline{|O|}$" = "ops",
               "$\\overline{C^*}$" = "optimal_plan_cost",
               "$\\overline{z_0}$" = "initial_lp_solution",
               "$\\overline{r_0}$" = "initial_lp_rows",
               "$\\overline{c_0}$" = "initial_lp_cols")

all <- bind_cols(cat_by_cost, h_init, lp_size, others) %>%
  select(-domain1, -domain2, -domain3, -all_unit_cost_ops) %>%
  mutate(any_zero_cost_ops = ifelse(any_zero_cost_ops == 20, 'YES', 'NO')) %>%
  mutate(min_op_cost = as.integer(min_op_cost), max_op_cost = as.integer(max_op_cost)) %>%
  rename(!!new_names) %>%
  select(domain, "$\\overline{|V|}$", "$\\overline{|O|}$", "$zco$", "$\\overline{c_{min}}$", "$\\overline{c_{max}}$", 
         "$\\overline{C^*}$", "$\\overline{z_0}$", "$\\overline{r_0}$", "$\\overline{c_0}$") %>%
  arrange(domain)

all1 <- all %>% select(domain, "$\\overline{|V|}$", "$\\overline{|O|}$", "$zco$", "$\\overline{c_{min}}$", "$\\overline{c_{max}}$")
all2 <- all %>% select("$\\overline{C^*}$", "$\\overline{z_0}$", "$\\overline{r_0}$", "$\\overline{c_0}$")

save_table(all1, '', 'domains1', environment = 'table', only.contents = T)
save_table(all2, '', 'domains2', environment = 'table', only.contents = T)
