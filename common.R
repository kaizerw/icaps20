###############################################################################
## common functions
###############################################################################

rows_to_keep <- function(){
    c('instance', 'solved', 'seqs', 'restarts',
      'total_astar_time', 'total_solve_time',
      'planner_memory', 'mean_ops_by_constraint')
}

read_results <- function(filename, sheet, n_rows_to_skip, n_rows_to_read) {
    read_excel(filename, sheet = sheet, skip = n_rows_to_skip, 
               n_max = n_rows_to_read) %>%
        as_tibble() %>%
        select(rows_to_keep()) %>%
        rename(domain = instance, total_seq_time = total_astar_time) %>%
        mutate(domain = str_replace(domain, '-opt11-strips', '')) %>%
        mutate(domain = str_replace(domain, 'SAT/', '')) %>%
        mutate(domain = str_replace(domain, 'LMCUT_T3/', '')) %>%
        mutate(domain = str_replace(domain, 'Summary', 'Total'))
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

scatter_plot <- function(x, y, x_label = 'X', y_label = 'Y', title = 'Plot') {
    x_min <- min(x) - 1
    x_max <- max(x) + 1
    y_min <- min(y) - 1
    y_max <- max(y) + 1
    df <- tibble(x = x, y = y)
    ggplot(df, aes(x, y)) +
        xlim(x_min, x_max) + ylim(y_min, y_max) +
        xlab(x_label) + ylab(y_label) +
        ggtitle(title) +
        geom_abline(intercept = 0, slope = 1) +
        theme_minimal() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(colour = "black", size = 1)) +
        geom_point(size = 1, shape = 1, fill = "white", alpha = 1) +
        geom_point(size = 1, shape = 16, fill = "black", alpha = 0.3) +
        coord_fixed()
    ggsave(paste('figs/', title, '.pdf', sep = ""), family = 'Times')    
}

