#############################################
## Figures

library(ggplot2)
library(dplyr)

thetheme <- theme_bw()+
  theme(axis.title = element_text(family = 'Century Gothic', 
                                  size = 16,
                                  color = 'black'),
        axis.text = element_text(family = 'Century Gothic',
                                 size = 14,
                                 color = 'black'),
        legend.text = element_text(family = 'Century Gothic',
                                   size = 14,
                                   color = 'black'),
        legend.title = element_text(family = 'Century Gothic',
                                    size = 16,
                                    color = 'black'),
        title = element_text(family = 'Century Gothic', 
                             size = 16,
                             color = 'black', 
                             hjust = 0.5),
        axis.line = element_line(color = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
  )

####################
## Load data
ddf <- read.csv('../data/all_data.csv')  # main data
base_dt <- read.csv('../data/baseline_gbi_per_year.csv')
word_sim <- read.csv('../data/abstr_100w_sim.csv') # similarity of top 100 words per journal
stat_data <- read.csv('../data/data_for_stats.csv')

## Cut in date categories for plots
ddf$date_group <- cut(ddf$date, 
                      breaks = c(1999, 2005, 2010, 2015, 2021), 
                      labels=c("2000-2005","2005-2010","2010-2015", "2015-2020"))
ddf$date_group <- factor(ddf$date_group, levels = c("2000-2005","2005-2010","2010-2015", "2015-2020"))

## Cut in gender categories for plots
levs <- seq(0, 1, 0.2)
ddf$g_cat <- cut(ddf$g_idx, include.lowest = T,
                 breaks= levs, 
                 labels=c("HF","LF","B", "LM", "HM"))

## Animal behaviour journals subset for case study
anbeh <- ddf[ddf$journal %in% c('animal behaviour',
                                'behavioural ecology',
                                'ethology',
                                "behavioral ecology and sociobiology"
),]

##################################################
## S1 Fig. A. Number of citations 

cit_hist <- ggplot(ddf[ddf$cit < 416,], aes(x = cit, fill = date_group, color = date_group))+
  geom_histogram(aes(y=..density..), color = 'black', fill = 'white', bins = 40, linewidth = 1)+
  geom_density( alpha = 0.3, linewidth = 1)+
  labs(x = 'Citations', y ='Density', color = 'Date', fill = 'Date') +
  scale_color_manual(values = c('#6182ad','#7f628a','#91322c', '#f2a155'))+
  scale_fill_manual(values = c('#6182ad','#7f628a','#91322c', '#f2a155'))+
  thetheme+
  theme(legend.position = c(0.8,0.7),
        legend.background = element_rect(color= 'black'))

##################################################
## S1 Fig. B. Number of references 

ref_hist <- ggplot(ddf[ddf$Nrefs < 112,], aes(x = Nrefs, fill = date_group, color = date_group))+
  geom_histogram(aes(y=..density..), color = 'black', fill = 'white', bins = 30, linewidth = 1)+
  geom_density( alpha = 0.3, linewidth = 1)+
  labs(x = 'Number of references', y = 'Density', color = 'Date', fill = 'Date') +
  scale_color_manual(values = c('#6182ad','#7f628a','#91322c', '#f2a155'))+
  scale_fill_manual(values = c('#6182ad','#7f628a','#91322c', '#f2a155'))+
  thetheme+
  theme(legend.position = c(0.8,0.7),
        legend.background = element_rect(color= 'black'))

##################################################
## S1 Fig. C. Number of authors 

N_hist <- ggplot(ddf[ddf$N < 20,], aes(x = N, fill = date_group, color = date_group))+
  geom_histogram(aes(y= ..density..), color = 'black', fill = 'white', bins = 15, linewidth = 1)+
  geom_density( alpha = 0.3, linewidth = 1, adjust = 3)+
  labs(x = 'Number of authors', y = 'Density', color = 'Date', fill = 'Date') +
  scale_color_manual(values = c('#6182ad','#7f628a','#91322c', '#f2a155'))+
  scale_fill_manual(values = c('#6182ad','#7f628a','#91322c', '#f2a155'))+
  thetheme+
  theme(legend.position = c(0.8,0.7),
        legend.background = element_rect(color= 'black'))

pS1 <- cowplot::plot_grid(cit_hist +
                           scale_y_continuous(
                              labels = function(x) ifelse(x < 0.005, sprintf("%.f", x), sprintf("%.2f", x))), 
                             ref_hist + 
                               labs(y= '') +
                               theme(legend.position = 'none',
                                     axis.text.y = element_blank()), 
                             N_hist + 
                               labs(y= '') +
                               theme(legend.position = 'none',
                                     axis.text.y = element_blank()),
                             labels = c('(a)', '(b)  ', '(c)  '),
                          label_x = -0.031,
                          label_fontface = 'plain',
                           nrow = 1)

ggsave(pS1, filename = '../output/FigS1.tiff', dpi = 300, width = 14, height = 4)

##################################################
## S3 Fig. Word similarity among journal abstracts

word_sim <- word_sim[order(word_sim$perc, decreasing = T),]
word_sim$jj <- factor(word_sim$jj, levels = word_sim$jj)

pS3 <- ggplot(word_sim[word_sim$jj!= 'animal behaviour',], 
              aes(x = perc, y = jj))+
  geom_col(color = 'black')+
  labs(y = '', x = 'Proportion')+
  thetheme +
  scale_x_continuous(
    labels = function(x) ifelse(x < 0.005, sprintf("%.f", x), sprintf("%.2f", x)))

ggsave(pS3, filename = '../output/FigS3.tiff', dpi = 300, width = 14, height = 12)


##################################################
## Figure 1 - timeseries

tpall <- ddf[ddf$N > 1,] %>% group_by(date) %>% 
  summarise(mgidx = mean(fr),
            sdidx = sd(fr)/sqrt(n()),
            fa = sum(g_f == 'female')/n(),
            la = sum(g_l == 'female')/n()
  )
tpforsd <- tpall
tpall <- reshape2::melt(tpall[,colnames(tpall) != 'sdidx'], id = 'date')

base_dt$X <- 'base'
base_dt <- base_dt[,c('date','X', 'gidx')]
colnames(base_dt) <- colnames(tpall)
tpall <- rbind(base_dt, tpall)

p1A <- ggplot(tpall, aes(x = date, y = value, fill = as.factor(variable),
                          linetype = as.factor(variable),
                          shape = as.factor(variable),
                          color = variable))+
  geom_path(linewidth = 0.8) +
  geom_point(color = 'black', size = 2.2)+
  geom_errorbar(data = tpforsd,
                aes(x= date, y = mgidx,
                    ymin = mgidx - sdidx, ymax = mgidx + sdidx), color = 'black', width = 0.5, inherit.aes = F)+
  scale_fill_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                    labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                    values = c('deepskyblue3','orchid3','black', 'grey'),
  )+
  scale_color_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                     labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                     values = c('deepskyblue3','orchid3','black', 'grey50'),
  )+
  scale_linetype_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                        labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                        values = c('solid', 'solid', 'solid', 'dashed'),
  )+
  scale_shape_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                     labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                     values = c(21, 21, 23, 24),
  )+
  labs(x = 'Year', y = 'Female ratio', fill = 'Group:', 
       color = 'Group:', shape = 'Group:', linetype = 'Group:', ) +
  thetheme+
  scale_y_continuous(limits = c(0.2,0.4),
                     labels = c(0.2, 0.25, 0.3, 0.35, 0.4))+
  theme(legend.position = 'top',
        legend.background = element_rect(color= 'black'),
        panel.grid.major = element_blank())


## Full axis for supplementary
pS4 <- ggplot(tpall, aes(x = date, y = value, fill = as.factor(variable),
                         linetype = as.factor(variable),
                         shape = as.factor(variable),
                         color = variable))+
  geom_path(linewidth = 0.8) +
  geom_point(color = 'black', size = 2.2)+
  geom_errorbar(data = tpforsd,
                aes(x= date, y = mgidx,
                    ymin = mgidx - sdidx, ymax = mgidx + sdidx), color = 'black', width = 0.5, inherit.aes = F)+
  scale_fill_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                    labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                    values = c('deepskyblue3','orchid3','black', 'grey'),
  )+
  scale_color_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                     labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                     values = c('deepskyblue3','orchid3','black', 'grey50'),
  )+
  scale_linetype_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                        labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                        values = c('solid', 'solid', 'solid', 'dashed'),
  )+
  scale_shape_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                     labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                     values = c(21, 21, 23, 24),
  )+
  labs(x = 'Year', y = 'Female ratio', fill = 'Group:', 
       color = 'Group:', shape = 'Group:', linetype = 'Group:', ) +
  thetheme+
 # ylim(c(0,1))+
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
    theme(legend.position = 'top',
        legend.background = element_rect(color= 'black'),
        panel.grid.major = element_blank()) +
  scale_y_continuous(limits = c(0,1),
                     labels = c(0, 0.25, 0.5, 0.75, 1))


ggsave(pS4, filename = '../output/FigS4.tiff', dpi = 300,
       width = 7, height = 4)

## B. Deviation from baseline
tpall <- ddf[ddf$N > 1,] %>% group_by(date) %>% 
  summarise(mgidx = mean(fr),
            sdidx = sd(fr),
            fa = sum(g_f == 'female')/n(),
            la = sum(g_l == 'female')/n()
  )

tpall <- left_join(tpall, base_dt)
tpall$fa_s <- (tpall$fa - tpall$value)/tpall$value 
tpall$la_s <-  (tpall$la - tpall$value)/tpall$value 
tpall$mgidx_s <-  (tpall$mgidx - tpall$value)/tpall$value 
tpall <- reshape2::melt(tpall[,c('date', 'fa_s', 'la_s', 'mgidx_s')], id = 'date')

tpall$date_group <- cut(tpall$date,
                        breaks = c(1999, 2005, 2010, 2015, 2021),
                        labels=c("2000-2005","2005-2010","2010-2015", "2015-2020"))

tpall <- tpall %>% group_by(date_group, variable) %>% 
  summarise(md = mean(value),
            sd = sd(value))

p1B <- ggplot(tpall, aes(x = date_group, y = md, shape = variable, 
                            fill = as.factor(variable)))+
  geom_hline(yintercept = 0, linetype = 'dashed', color ='black') +
  geom_errorbar(aes(ymin = md - sd, ymax = md + sd), color = 'black', width = 0.2,  position=position_dodge(width=0.5))+
  geom_point(color = 'black', size = 3, position=position_dodge(width=0.5))+
  
  scale_fill_manual(
    breaks = c('fa_s', 'la_s', 'mgidx_s'),
    labels = c('First author', 'Last author', 'All authors'),
    values = c('deepskyblue3','orchid3','black', 'grey50'),
  )+
  scale_shape_manual(breaks = c('fa_s', 'la_s', 'mgidx_s'),
                     labels = c('First author', 'Last author', 'All authors'),
                     values = c(21, 21, 23, 24),
  )+
  scale_y_continuous(labels = c(-0.15, -0.1 , -0.05, 0))+
  labs(x = 'Year', y = 'Deviation from baseline', #title = 'Female ratio per group',
       fill = '', color = '', shape = '', ) +
  thetheme+
  theme(legend.position = 'top',
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(color= 'black')
  )

## C. Female ratio categories
tp <- ddf[ddf$N >1,]

p1C <- ggplot(tp[!is.na(tp$g_cat) ,],
              aes(x = date, y = after_stat(count),
                  fill= factor(g_cat, levels = c('HF', 'LF', 'B', 'LM', 'HM')),
                                          ))+
  geom_bar(position = 'fill', color= 'black') + 
  scale_fill_manual(breaks = rev(c('HM', 'LM', 'B', 'LF', 'HF')),
                    values =  rev(c( '#91322c', '#f2a155', '#7f628a','#6182ad', 'turquoise4')),
                    name = 'Female ratio:',
                    labels= rev(c('Very low','Low', 'Balanced','High', 'Very high')))+ 
  xlab('Year') +
  ylab('Proportion of articles') +
  theme_classic() +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),
                     labels = c(0, 0.25, 0.5, 0.75, 1))+
  thetheme+
  theme(legend.background = element_rect(color = 'black'),
        legend.position = 'top',
        panel.border  = element_rect(color = 'black', linewidth = 1))

## D. Single authors
tpsingl <- ddf[ddf$N < 2,] 

p1D <- ggplot(tpsingl, aes(x = date_group, y= ..count.., fill = g_f))+
  geom_bar(position = 'fill', color = 'black') +
  scale_fill_manual(breaks = c('male', 'female'),
                    values =  c('grey30','olivedrab'), 
                    name = 'Single Author:',
                    labels= c('Male', 'Female'))+ 
  xlab('Year') +
  ylab('Proportion of articles') +
  thetheme+
  theme(
    legend.position = 'top',
    panel.border  = element_rect(color = 'black', linewidth = 1))+
  scale_y_continuous(limits = c(0,1),
                     labels = c(0, 0.25, 0.5, 0.75, 1))



FIG1 <- aplot::plot_list(p1A, p1B, p1C, p1D, 
                          widths =  c(0.65,0.35), 
                          labels =  c('(a)', '(b)','(c)','(d)')
                       )

ggsave(FIG1, filename = '../output/Fig1.tiff',
       dpi = 400, width = 15, height = 11)

##################################################
# Figure 2

tp <- ddf[ddf$N > 1,]
tpall <- tp %>% group_by(date_group, g_l, g_f) %>% 
  summarise(Npair = n()) %>% 
  group_by(date_group, g_l) %>%
  mutate(Ns = Npair/sum(Npair))

p2A <- ggplot(tpall[tpall$date_group %in% c('2000-2005', '2015-2020'),], 
                aes(x = as.factor(g_l), y = Ns, label = round(Ns,2), fill= g_f))+
  geom_col(position = 'fill', color = 'black') +
  geom_text( aes(label = round(Ns, 2)), color = 'white',
             family ='Century Gothic',
             position = position_fill(vjust = 0.5), size = 5
  )+
  facet_grid(~ date_group)+
  scale_fill_manual(breaks = c('female', 'male'),
                    values =  c( 'olivedrab','grey20'),
                    labels = c('female', 'male'))+
  
  labs(y = 'Proportion of articles', 
       x = 'Last author', 
       fill = 'First author:') +
  scale_y_continuous(labels = c(0, 0.25, 0.5,0.75, 1))+
  thetheme +
  theme(legend.background = element_rect(color = 'black'),
        legend.position = 'top',
        strip.background = element_rect(fill ='white'),
        strip.text = element_text(size = 16, family = 'Century Gothic'),
        panel.border  = element_rect(color = 'black', linewidth = 1))


## B. Last authors
tp <- ddf[!is.na(ddf$g_l) & ddf$N > 1,]

p2B <- ggplot(tp, aes(x = g_l, y = fr, fill = g_l))+
  geom_boxplot(color = 'black') +
  scale_fill_manual(breaks = c('female', 'male'),
                    values =  c('olivedrab','grey30')) +
  labs(y = 'Female ratio', 
       x = 'Last author', 
       title = '') +
  ggsignif::geom_signif(comparisons = list(c('female', 'male')),
                        map_signif_level = TRUE)+
  thetheme +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     limits = c(0, 1.099),
                     labels = c(0, 0.25, 0.5,0.75, 1))+
  theme(legend.background = element_rect(color = 'black'),
        legend.position = 'none',
        panel.border  = element_rect(color = 'black', linewidth = 1))

## C. Diversity index

p2C <- ggplot(tp, aes(x = g_f, y = div_idx, fill = g_f))+
  geom_boxplot(color = 'black') +
  scale_fill_manual(breaks = c('female', 'male'),
                    values =  c('olivedrab','grey30')) +
  labs(y = 'Diversity Index (DI)', 
       x = 'First author', 
       title = '') +
  ggsignif::geom_signif(comparisons = list(c('female', 'male')),
                        map_signif_level = TRUE)+
  thetheme +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     limits = c(0, 1.099),
                     labels = c(0, 0.25, 0.5,0.75, 1))+
  theme(legend.background = element_rect(color = 'black'),
        legend.position = 'none',
        panel.border  = element_rect(color = 'black', linewidth = 1))

FIG2 <-  cowplot::plot_grid(p2A, p2B, p2C, 
                            nrow = 1, labels = c('(a)', '(b)', '(c)'),
                            label_fontface = 'plain'
)

ggsave(FIG2, file ='../output/Fig2.tiff', dpi = 300, width = 14, height= 6)


#######################################
## Figure 3

nosingl <- stat_data[stat_data$N > 1, ]
tp1 <- nosingl[nosingl$N <= 4,]
tp2 <- nosingl[nosingl$N > 4,]

quant.sum <- reshape2::melt(xtabs(~ g_f + cit_q, data = tp1))
quant.sum$proport <- quant.sum$value/reshape2::melt(xtabs(~ g_f, data = tp1))$value
quant.sum2 <- reshape2::melt(xtabs(~ g_f + cit_q, data = tp2))
quant.sum2$proport <- quant.sum2$value/reshape2::melt(xtabs(~ g_f, data = tp2))$value
quant.sum2$N <- '>4'
quant.sum$N <- '<=4'
tpall <- rbind(quant.sum, quant.sum2)
tadd <- tpall[tpall$cit_q == 5,]
tadd$cit_q <- 6
tpall <- rbind(tpall, tadd )

p3A <- ggplot(tpall, aes(cit_q, proport,
                           linetype = N, colour=g_f)) +
  geom_step(linewidth= 1.5, ) + 
  labs(x = 'Citation performance',
       y = 'Proportion of articles',
       color = 'First author:',
       linetype = 'Authors N.:') +
  scale_x_continuous(breaks = c(1, 2, 3,4,5))+
  scale_y_continuous(labels = c(0.1, 0.15, 0.2, 0.25, 0.3),
                     limits =c(NA, 0.3))+
  scale_color_manual(breaks = c('female', 'male'),
                     values =  c( 'olivedrab','grey20'))+
  scale_linetype_manual(breaks = c('<=4', '>4'),
                        values =  c( 'solid','dashed'))+
  thetheme +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(hjust = -3),
        legend.position = c(0.2, 0.2),
        legend.background = element_rect(color= 'black'))


quant.sum <- reshape2::melt(xtabs(~ g_l + cit_q, data = tp1))
quant.sum$proport <- quant.sum$value/reshape2::melt(xtabs(~ g_l, data = tp1))$value
quant.sum2 <- reshape2::melt(xtabs(~ g_l + cit_q, data = tp2))
quant.sum2$proport <- quant.sum2$value/reshape2::melt(xtabs(~ g_l, data = tp2))$value
quant.sum2$N <- '>4'
quant.sum$N <- '<=4'
tpall <- rbind(quant.sum, quant.sum2)
tadd <- tpall[tpall$cit_q == 5,]
tadd$cit_q <- 6
tpall <- rbind(tpall, tadd )

p3B <- ggplot(tpall, aes(cit_q, proport,
                                linetype = N, colour=g_l)) +
  geom_step(linewidth = 1.5, ) + 
  labs(x = 'Citation performance',
       y = '',
       color = 'Last author:',
       linetype = 'Number of\nauthors:') +
  scale_x_continuous(breaks = c(1, 2, 3,4,5)
  )+
  scale_y_continuous(labels = c(0.1, 0.15, 0.2, 0.25, 0.3),
                     limits =c(NA, 0.3))+
  scale_color_manual(breaks = c('female', 'male'),
                     values =  c( 'olivedrab','grey15'))+
  scale_linetype_manual(breaks = c('<=4', '>4'),
                        values =  c( 'solid','dashed'),
                        guide = 'none')+
  thetheme +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(hjust = -3),
        legend.position = c(0.2, 0.2),
        legend.background = element_rect(color= 'black'))

## Female ratio Categories
quant.sum <- reshape2::melt(xtabs(~ g_cat + cit_q, data = tp1))
quant.sum$proport <- quant.sum$value/reshape2::melt(xtabs(~ g_cat, data = tp1))$value
quant.sum2 <- reshape2::melt(xtabs(~ g_cat + cit_q, data = tp2))
quant.sum2$proport <- quant.sum2$value/reshape2::melt(xtabs(~ g_cat, data = tp2))$value
quant.sum2$N <- '>4'
quant.sum$N <- '<=4'
tpall <- rbind(quant.sum, quant.sum2)
tadd <- tpall[tpall$cit_q == 5,]
tadd$cit_q <- 6
tpall <- rbind(tpall, tadd )
tpall <- tpall[tpall$g_cat %in% c('HM', 'B', 'HF'),]

p3C <- ggplot(tpall, aes(cit_q, proport,
                               linetype = N, colour=g_cat)) +
  geom_step(linewidth = 1.5) + 
  labs(x = 'Citation performance',
       y = '',
       color = 'Last author:',
       linetype = 'Number of\nauthors:') +
  scale_x_continuous(breaks = c(1, 2, 3,4,5))+
  scale_y_continuous(labels = c(0.1, 0.15, 0.2, 0.25, 0.3),
                     breaks = c(0.1, 0.15, 0.2, 0.25, 0.3),
                     limits =c(NA, 0.3))+
  scale_color_manual(breaks = c('HM', 'B',  'HF'),
                    values =  c( 'firebrick', '#7f628a', 'turquoise3'),
                    name = 'Female ratio:',
                     labels= c('Very low', 'Balanced', 'Very high'))+ 
  
  scale_linetype_manual(breaks = c('<=4', '>4'),
                        values =  c( 'solid','dashed'),
                        guide = 'none')+
  thetheme +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(hjust = -3),
        legend.position = c(0.25, 0.2),
        legend.background = element_rect(color= 'black'))


FIG3 <- cowplot::plot_grid(p3A, 
                             p3B+theme(axis.text.y = element_blank()), 
                             p3C+theme(axis.text.y = element_blank()), nrow = 1, 
                             labels = c('(a)', '(b)', '(c)'),
                           label_x = -0.02,
                           label_fontface = 'plain'
                      )
ggsave(FIG3, file = '../output/Fig3.tiff',dpi = 400, width = 18, height = 6)


##################################################
## Figure 4

tpall <- anbeh[anbeh$N > 1,] %>% group_by(date) %>% 
  summarise(mgidx = mean(fr),
            sdidx = sd(fr)/sqrt(n()),
            fa = sum(g_f == 'female')/n(),
            la = sum(g_l == 'female')/n()
  )
tpforsd <- tpall
tpall <- reshape2::melt(tpall[,colnames(tpall) != 'sdidx'], id = 'date')

p4A <- ggplot(tpall, aes(x = date, y = value, fill = as.factor(variable),
                         linetype = as.factor(variable),
                         shape = as.factor(variable),
                         color = variable))+
  geom_path(size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', color ='black') +
  
  geom_point(color = 'black', size = 2.2)+
  geom_errorbar(data = tpforsd,
                aes(x= date, y = mgidx,
                    ymin = mgidx - sdidx, ymax = mgidx + sdidx), color = 'black', width = 0.5, inherit.aes = F)+
  scale_fill_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                    labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                    values = c('deepskyblue4','orchid4','black', 'grey'),
  )+
  scale_color_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                     labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                     values = c('deepskyblue4','orchid4','black', 'grey'),
  )+
  scale_linetype_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                        labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                        values = c('solid', 'solid', 'solid', 'dashed'),
  )+
  scale_shape_manual(breaks = c('fa', 'la', 'mgidx', 'base'),
                     labels = c('First author', 'Last author', 'All authors', 'Baseline'),
                     values = c(21, 21, 23, 24),
  )+
  scale_y_continuous(labels = c(0.3, 0.35, 0.4, 0.45, 0.5))+
  labs(x = 'Year', y = 'Female ratio', fill = 'Group:', 
       color = 'Group:', shape = 'Group:',
       linetype = 'Group:', ) +
  thetheme+
  theme(legend.position = 'top',
        legend.background = element_rect(color= 'black'),
        panel.grid.major = element_blank())

tpall <- anbeh[anbeh$N > 1,]  %>% group_by(date_group, g_l, g_f) %>% 
  summarise(Npair = n()) %>% 
  group_by(date_group, g_l) %>%
  mutate(Ns = Npair/sum(Npair))

p4B <- ggplot(tpall[tpall$date_group %in% c('2000-2005', '2015-2020'),], 
                aes(x = as.factor(g_l), y = Ns, label = round(Ns,2), fill= g_f))+
  geom_col(position = 'fill', color = 'black') +
  geom_text( aes(label = round(Ns, 2)), color = 'white',
             family ='Century Gothic',
             position = position_fill(vjust = 0.5), size = 5
  )+
  facet_grid(~ date_group)+
  scale_fill_manual(breaks = c('female', 'male'),
                    values =  c( 'olivedrab','grey15'))+
  scale_y_continuous(labels = c(0, 0.25, 0.5, 0.75, 1))+
  labs(y = 'Proportion of articles', 
       x = 'Last author', 
       color = 'First author:',
       fill = 'First author:') +
  thetheme +
  theme(legend.background = element_rect(color = 'black'),
        legend.position = 'top',
        strip.background = element_rect(fill ='white'),
        strip.text = element_text(size = 16, family = 'Century Gothic'),
        panel.border  = element_rect(color = 'black', linewidth = 1))


## C. Journal types

jt <- read.csv('../data/journal_types.csv')
jt$journal <- tolower(jt$journal)
jf <- left_join(ddf, jt)

tp <- jf %>% group_by(type) %>% summarise(fa = mean(ifelse(g_f == 'female', 1,0)),
                                                           la = mean(ifelse(g_l == 'female', 1,0), na.rm = T),
                                                           mfr = mean(fr),
                                                           sdfr = sd(fr)/sqrt(n())
)
tp2 <- reshape2::melt(tp)
tp2 <- tp2[order(tp2$value, decreasing = T),]
tp2$type <- factor(tp2$type, levels = unique(tp2$type))

p4C <- ggplot(tp2[!(tp2$variable %in% c('sdd', 'sdfr')),], 
       aes(x = type, y = value, fill = variable))+
  geom_point(shape = 23, size = 3)+
  geom_errorbar(data= tp, 
                inherit.aes = F, 
                aes(x = type, ymin = mfr-sdfr , ymax = mfr + sdfr), width = 0.1)+
  thetheme +
  labs(y = 'Female ratio', 
       x = 'Journal category', 
       fill = 'First author:') +
  scale_y_continuous(limits = c(0.2, 0.45),
                     breaks = seq(0.2, 0.45, 0.05),
                     labels = c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45))+
  theme(legend.position = c(0.72,0.85),
        panel.grid.major = element_blank(),
        legend.background  = element_rect(color= 'black'))+
  scale_fill_manual(breaks = c('fa', 'la', 'mfr'),
                    labels = c('First author', 'Last author', 'All authors'),
                    values = c('deepskyblue4','orchid4','black'),
  )+
  scale_x_discrete(labels = c('BEH', 'H.I.', 'METH', 'REV', 'Others' ),
                   breaks = c('BEH', 'GH', 'METH', 'REV', 'OTH'))


FIG4 <- cowplot::plot_grid(p4A, 
                           p4B,
                           p4C,
                           nrow = 1,
                           labels = c('(a)', '(b)', '(c)'),
                           rel_widths = c(1,0.7,0.6),
                           label_x= -0.015,
                           label_fontface = 'plain'
                           )
ggsave(FIG4, file = '../output/Fig4.tiff', dpi =300, width = 15, height = 5 )


###################################
## Supplementary S2

ps2A <- ggplot(ddf, aes(x = fr)) +
  geom_density(aes(x = div_idx), fill ='orchid4', alpha = 0.7)+
  geom_density(alpha = 0.2, color = 'black', linewidth = 1.6) +
  geom_density(alpha = 0.2, color = 'yellow', linewidth = 0.9) +
  labs(x = 'Metric')+
  scale_x_continuous(labels = c(0, 0.25, 0.5, 0.75, 1))+
  thetheme +
  theme(axis.text.y = element_text(size = 12) )

ps2B <- ggplot(ddf[ddf$N > 4,], aes( x = fr)) +
  geom_density(aes(x = div_idx), fill ='orchid4', alpha = 0.7)+
  geom_density(alpha = 0.2, color = 'black', linewidth = 1.6) +
  geom_density(alpha = 0.2, color = 'yellow', linewidth = 0.9) +  
  labs(x = 'Metric', y = ' ')+
  scale_x_continuous(labels = c(0, 0.25, 0.5, 0.75, 1))+
  scale_y_continuous(labels = c(0, 0.5, 1, 1.5, 2, 2.5))+
  thetheme +
  theme(axis.text.y = element_text(size = 12) )


S2FIG <- cowplot::plot_grid(ps2A, 
                            ps2B, 
                            nrow = 1, 
                            labels = c('(a)' ,'(b)'),
                            label_x = -0.02, 
                            label_fontface = 'plain'
)
ggsave(S2FIG, filename = '../output/FigS2.tiff', dpi= 300, width = 10, height = 4)
