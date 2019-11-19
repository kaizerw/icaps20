source('common.R')

filename_df <- 'results/11_14/1_emphasis_default_20m.xlsx'
filename_bb <- 'results/11_14/1_emphasis_bestbound_20m.xlsx'

our_df <- read_all_results(filename_df, 'LMCUT_T3')
our_bb <- read_all_results(filename_bb, 'LMCUT_T3')
sat_df <- read_all_results(filename_df, 'SAT')
sat_bb <- read_all_results(filename_bb, 'SAT')

seqs <- tibble(df = our_df$seqs, bb = our_bb$seqs) %>%
  filter(!is.na(df), !is.na(bb))
g1 <- scatter_plot(seqs$df, seqs$bb, 0, 6200,
             x_label = "default",
             y_label = "best bound",
             subtitle = 'OpSearch')

seqs <- tibble(df = sat_df$seqs, bb = sat_bb$seqs) %>%
  filter(!is.na(df), !is.na(bb))
g2 <- scatter_plot(seqs$df, seqs$bb, 0, 3500,
                   subtitle = 'OpSeq')

#ggsave('figs/opsearch_seqs.pdf', plot = g1, family = 'Times')
#ggsave('figs/sat_seqs.pdf', plot = g2, family = 'Times')

all <- ggarrange(g1, g2, nrow = 1)
ggsave('figs/mip_scatter.pdf', plot = all, family = 'Times', 
       width = 8, height = 4)
