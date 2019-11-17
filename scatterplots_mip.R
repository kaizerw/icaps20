source('common.R')

filename_df <- 'results/11_14/1_emphasis_default_20m.xlsx'
filename_bb <- 'results/11_14/1_emphasis_bestbound_20m.xlsx'

our_df <- read_all_results(filename_df, 'LMCUT_T3')
our_bb <- read_all_results(filename_bb, 'LMCUT_T3')
sat_df <- read_all_results(filename_df, 'SAT')
sat_bb <- read_all_results(filename_bb, 'SAT')

seqs <- tibble(df = our_df$seqs, bb = our_bb$seqs) %>%
  filter(!is.na(df), !is.na(bb))
scatter_plot(seqs$df, seqs$bb, 0, 6200,
             x_label = 'seqs OpSearch default emphasis',
             y_label = 'seqs OpSearch best bound emphasis',
             name = 'opsearch_seqs')

restarts <- tibble(df = our_df$restarts, bb = our_bb$restarts) %>%
  filter(!is.na(df), !is.na(bb))
scatter_plot(restarts$df, restarts$bb, 0, 10,
             x_label = 'restarts OpSearch default emphasis',
             y_label = 'restarts OpSearch best bound emphasis',
             name = 'opsearch_restarts')

seqs <- tibble(df = sat_df$seqs, bb = sat_bb$seqs) %>%
  filter(!is.na(df), !is.na(bb))
scatter_plot(seqs$df, seqs$bb, 0, 3500,
             x_label = 'seqs SAT default emphasis',
             y_label = 'seqs SAT best bound emphasis',
             name = 'sat_seqs')

restarts <- tibble(df = sat_df$restarts, bb = sat_bb$restarts) %>%
  filter(!is.na(df), !is.na(bb))
scatter_plot(restarts$df, restarts$bb, 0, 5,
             x_label = 'restarts SAT default emphasis',
             y_label = 'restarts SAT best bound emphasis',
             name = 'sat_restarts')
