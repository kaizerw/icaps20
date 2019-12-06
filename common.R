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
  function(filename, sheet, n_rows_to_skip, n_rows_to_read) {
    read_excel(filename, sheet = sheet, skip = n_rows_to_skip, n_max = n_rows_to_read) %>%
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
      mutate(domain = str_replace(domain, 'blind/', '')) %>%
      mutate(domain = str_replace(domain, 'oc_seq_landmarks/', '')) %>%
      mutate(domain = str_replace(domain, 'lmcut/', '')) %>%
      mutate(domain = str_replace(domain, 'hstar/', '')) %>%
      mutate(planner_memory = planner_memory / 1e3) %>%
      mutate(mean_ops_by_constraint = mean_ops_by_constraint * 100) %>%
      mutate(planner_memory = as.integer(planner_memory), 
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

read_all_results_heuristics2 <- function(filename, sheet) {
  dfs <- list()
  
  indices <- c(1, 16, 20, 28, 35, 40, 48, 56, 71, 86, 91, 149, 182, 186, 193, 204, 212, 220, 226, 230, 
               235, 280, 288, 295, 304, 310, 320, 328, 340, 346, 351, 357, 368, 373, 378, 388)
  
  for (i in seq(1, length(indices) - 1)) {
    n_rows_to_read <- indices[i + 1] - indices[i] - 1
    dfs[[i + 1]] <- read_results(filename, sheet, indices[i] - 1, n_rows_to_read)
  }

  bind_rows(dfs) %>%
    rename(instance = domain) %>%
    filter(instance != 'Total') %>%
    mutate(instance = str_replace(instance, 'blind/', '')) %>%
    mutate(instance = str_replace(instance, 'lmcut/', '')) %>%
    mutate(instance = str_replace(instance, 'oc_seq_landmarks/', '')) %>%
    mutate(instance = str_replace(instance, 'hstar/', '')) %>%
    mutate(instance = str_replace(instance, '-hstar', ''))
}

scatter_plot <-
  function(x, y, x_label = '', y_label = '', subtitle = '', fontsize = 25) {
    df <- tibble(x, y)
    ggplot(df, aes(x, y)) +
      xlab(x_label) + ylab(y_label) +
      geom_abline(intercept = 0, slope = 1) +
      theme_minimal() +
      labs(subtitle = subtitle) +
      theme(
        text = element_text(size = fontsize, color = 'black'),
        axis.text = element_text(color = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1),
        plot.subtitle = element_text(hjust = 0.5)
      ) +
      geom_point(size = 5, shape = 1, fill = "white", alpha = 1) +
      geom_point(size = 5, shape = 16, fill = "black", alpha = 0.3) +
      coord_fixed()
  }

save_table <-
  function(df, caption, name, environment = 'table*', only.contents = F, size = NULL, 
           hline.after = c(-1, 0, nrow(df)), digits = 2) {
    print(
      xtable(df, digits = digits, auto = TRUE, caption = caption, label = str_interp("tab:${name}")),
      size = size,
      table.placement = "htbp",
      include.rownames = FALSE,
      hline.after = hline.after,
      only.contents = only.contents,
      floating.environment = environment,
      sanitize.text.function = function(x) {x},
      file = str_interp("tabs/${name}.tex"),
      fileEncoding="UTF-8"
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

texttt <- function(x) {
  if (str_detect(x, 'Total')) {
    return(x);
  }
  str_interp("\\texttt{${x}}")
}