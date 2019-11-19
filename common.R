library(tidyverse)
library(xtable)
library(readxl)
library(ggplot2)
library(latex2exp)
library(egg)

###############################################################################
## common functions
###############################################################################

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

read_results <-
  function(filename,
           sheet,
           n_rows_to_skip,
           n_rows_to_read) {
    read_excel(filename,
               sheet = sheet,
               skip = n_rows_to_skip,
               n_max = n_rows_to_read) %>%
      as_tibble() %>%
      select(rows_to_keep()) %>%
      rename(domain = instance, total_seq_time = total_astar_time) %>%
      mutate(domain = str_replace(domain, '-opt11-strips', '')) %>%
      mutate(domain = str_replace(domain, 'SAT/', '')) %>%
      mutate(domain = str_replace(domain, 'LMCUT_T3/', '')) %>%
      mutate(domain = str_replace(domain, 'T3_BLIND/', '')) %>%
      mutate(domain = str_replace(domain, 'T3_LMCUT/', '')) %>%
      mutate(domain = str_replace(domain, 'T3_HSTAR/', '')) %>%
      mutate(domain = str_replace(domain, '-selected', '')) %>%
      mutate(domain = str_replace(domain, 'Summary', 'Total')) %>%
      mutate(planner_memory = planner_memory / 1e3) %>%
      mutate(mean_ops_by_constraint = mean_ops_by_constraint * 100) %>%
      mutate(total_solve_time = as.integer(total_solve_time), 
             planner_memory = as.integer(planner_memory), 
             mean_ops_by_constraint = as.integer(mean_ops_by_constraint))
  }

read_all_results <- function(filename, sheet) {
  n_rows_to_read <- 22
  dfs <- list()
  for (i in seq(0, 10)) {
    n_rows_to_skip <- i * n_rows_to_read + i
    dfs[[i + 1]] <- read_results(filename, sheet, n_rows_to_skip,
                                 n_rows_to_read)
  }
  bind_rows(dfs) %>%
    rename(instance = domain) %>%
    filter(instance != 'Total')
}

read_all_results_heuristics <- function(filename, sheet) {
  dfs <- list()
  
  dfs[[1]] <- read_results(filename, sheet, 0, 4)
  dfs[[2]] <- read_results(filename, sheet, 5, 10)
  dfs[[3]] <- read_results(filename, sheet, 16, 5)
  dfs[[4]] <- read_results(filename, sheet, 22, 6)
  dfs[[5]] <- read_results(filename, sheet, 29, 10)
  
  bind_rows(dfs) %>%
    rename(instance = domain) %>%
    filter(instance != 'Total') %>%
    mutate(instance = str_replace(instance, 'T3_BLIND/', '')) %>%
    mutate(instance = str_replace(instance, 'T3_LMCUT/', '')) %>%
    mutate(instance = str_replace(instance, 'T3_HSTAR/', '')) %>%
    mutate(instance = str_replace(instance, 'SAT/', ''))
}

scatter_plot <-
  function(x,
           y,
           f_min,
           f_max,
           x_label = '',
           y_label = '',
           subtitle = '') {
    df <- tibble(x = x, y = y)
    ggplot(df, aes(x, y)) +
      xlim(f_min, f_max) + ylim(f_min, f_max) +
      xlab(x_label) + ylab(y_label) +
      geom_abline(intercept = 0, slope = 1) +
      theme_minimal() +
      labs(subtitle = subtitle) +
      theme(
        text = element_text(size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1),
        plot.subtitle = element_text(hjust = 0.5)
      ) +
      geom_point(
        size = 5,
        shape = 1,
        fill = "white",
        alpha = 1
      ) +
      geom_point(
        size = 5,
        shape = 16,
        fill = "black",
        alpha = 0.3
      ) +
      coord_fixed()
  }

save_table <-
  function(df, caption, name, environment = 'table*', only.contents = F, size = NULL, hline.after = c(-1, 0, nrow(df))) {
    print(
      xtable(
        df,
        digits = 2,
        auto = TRUE,
        caption = caption,
        label = str_interp("tab:${name}")
      ),
      size = size,
      table.placement = "htbp",
      include.rownames = FALSE,
      hline.after = hline.after,
      only.contents = only.contents,
      floating.environment = environment,
      sanitize.text.function = function(x) {x},
      file = str_interp("tabs/${name}.tex")
    )
  }

make_summary <- function(df, method) {
  df <- df %>%
    summarise(
      method = method,
      seqs = sum(seqs),
      total_seq_time = mean(total_seq_time),
      total_solve_time = mean(total_solve_time),
      planner_memory = mean(planner_memory),
      mean_ops_by_constraint = mean(mean_ops_by_constraint)
    )
}