source('common.R')

filename_df <- 'results/11_14/1_emphasis_default_20m.xlsx'
filename_bb <- 'results/11_14/1_emphasis_bestbound_20m.xlsx'

our_df <- read_all_results(filename_df, 'LMCUT_T3')
our_bb <- read_all_results(filename_bb, 'LMCUT_T3')
sat_df <- read_all_results(filename_df, 'SAT')
sat_bb <- read_all_results(filename_bb, 'SAT')

seqs_our <- tibble(df = our_df$seqs, bb = our_bb$seqs, type = 'OpSearch') %>%
  filter(!is.na(df), !is.na(bb))
seqs_sat <- tibble(df = sat_df$seqs, bb = sat_bb$seqs, type = 'OpSeq') %>%
  filter(!is.na(df), !is.na(bb))
all <- bind_rows(seqs_our, seqs_sat)

all <- ggplot(all, aes(df, bb)) +
  xlim(0, 6200) + ylim(0, 6200) +
  xlab('default') + ylab('best bound') +
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal() +
  theme(
    text = element_text(size = 25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size = 1),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_point(size = 5, shape = 1, fill = "white", alpha = 1) +
  geom_point(size = 5, shape = 16, fill = "black", alpha = 0.3) +
  facet_wrap(~type, scales = "fixed") +
  coord_fixed()
ggsave('figs/mip_scatter1.pdf', plot = all, family = 'Times', 
       width = 20, height = 10)


g1 <- scatter_plot(seqs_our$df, seqs_our$bb, 0, 6200,
             x_label = "default",
             y_label = "best bound",
             subtitle = 'OpSearch')
g2 <- scatter_plot(seqs_sat$df, seqs_sat$bb, 0, 3500,
                   subtitle = 'OpSeq')

all <- ggarrange(g1, g2, nrow = 1)
ggsave('figs/mip_scatter2.pdf', plot = all, family = 'Times', 
       width = 20, height = 10)
