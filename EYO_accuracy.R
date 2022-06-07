# Determine how accurate EYO is for GENFI symptomatic subjects used in my analysis (ie those with imaging)

library(tidyverse)
library(ggpubr)
library(readxl)

genfi <- read_excel('C:/Users/Admin/Documents/McGill/PhD_Project/genfi_table_mar25_2020.xlsx')

#find baseline data - first visit for each subject
genfi_baseline <- genfi %>%
  mutate(Visit = case_when(Visit == 'V01' ~ 1,
                           Visit == 'V02' ~ 2,
                           Visit == 'V03' ~ 3,
                           Visit == 'V11' ~ 11,
                           Visit == 'V12' ~ 12
  )) %>% 
  group_by(`Blinded Code`) %>% 
  filter(Visit == min(Visit)) %>% 
  ungroup()

#remove subjects with no imaging
included_subjects_df <- read.table("C:/Users/Admin/Documents/McGill/PhD_Project/included_subjects.txt")
included_subjects <- as.character(included_subjects_df$V1)
genfi_bl_included <- genfi_baseline %>% filter(`Blinded Code` %in% included_subjects)

#get symptomatic subjects only
symptomatic <- subset(genfi_bl_included, genfi_bl_included$`Genetic status 2` == 'pos - symp')
n_all <- nrow(symptomatic)

#get relevant columns
symp_eyo <- symptomatic[ , c("Blinded Code" ,"Genetic Group", "Age at visit", "EYO", "Age at Onset")]

#compare age of onset to estimated onset from EYO
symp_eyo$est_age_onset <- symp_eyo$`Age at visit` - symp_eyo$EYO
symp_eyo$est_age_onset <- floor(symp_eyo$est_age_onset)
symp_eyo$diff <- symp_eyo$`Age at Onset` - symp_eyo$est_age_onset

#overall numbers
mean(abs(symp_eyo$diff))
min(abs(symp_eyo$diff))
max(abs(symp_eyo$diff))

#hisotgram
hist_all <- ggplot(symp_eyo, aes(diff)) + 
  geom_histogram(binwidth = 1, fill = 'Dark Blue') +
  labs(x = "Observed - Estimated Age of Onset", y = "Number of Subjects") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-20, 20, by = 5))

#scatterplot
scatter_all <- ggplot(symp_eyo, aes(est_age_onset, `Age at Onset`)) + 
  geom_point(aes(colour = `Genetic Group`)) +
  labs(x = "Estimated Age at Onset", y = "Observed Age at Onset") +
  stat_smooth(method = "lm", size = 1.3) +
  stat_cor(label.y = 85) +
  theme_classic() +
  theme(legend.title = element_blank())

cor(symp_eyo$`Age at Onset`, symp_eyo$est_age_onset)

#combined plot
combined_all <- annotate_figure(ggarrange(hist_all, scatter_all), 
                                top = text_grob(sprintf("Accuracy of Estimated Years to Onset: All Subjects (n = %s)", n_all), 
                                                face = "bold", size = 14))

#By gene
c9 <- subset(symp_eyo, `Genetic Group` == 'C9orf72')
n_c9 = nrow(c9)
mean(abs(c9$diff))
min(abs(c9$diff))
max(abs(c9$diff))

#histogram
c9_hist <- ggplot(c9, aes(diff)) + geom_histogram(binwidth = 1, fill = "#f8766d") +
  labs(x = "Observed - Estimated Age of Onset", y = "Number of Subjects") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0,14), breaks = seq(0,14, by = 2)) +
  scale_x_continuous(limits = c(-20,20), breaks = seq(-20, 20, by = 5))

#scatterplot
c9_scatter <- ggplot(c9, aes(est_age_onset, `Age at Onset`)) + geom_point(colour = "#f8766d") +
  labs(x = "Estimated Age at Onset", y = "Observed Age at Onset") +
  stat_smooth(method = "lm", size = 1.3) +
  stat_cor(label.y = 79) +
  theme_classic() +
  scale_y_continuous(breaks = seq(30, 80, by = 10)) +
  scale_x_continuous(breaks = seq(30, 80, by = 10)) +
  ggtitle(sprintf("C9orf72 (n = %s)", n_c9))

cor(c9$`Age at Onset`, c9$est_age_onset)

grn <- subset(symp_eyo, `Genetic Group` == 'GRN')
n_grn <- nrow((grn))
mean(abs(grn$diff))
min(abs(grn$diff))
max(abs(grn$diff))

#histogram
grn_hist <- ggplot(grn, aes(diff)) + geom_histogram(binwidth = 1, fill = "#00ba38") +
  labs(x = "Observed - Estimated Age of Onset", y = "Number of Subjects") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 7, by = 1)) +
  scale_x_continuous(limits = c(-20,20), breaks = seq(-20, 20, by = 5))

#scatterplot
grn_scatter <- ggplot(grn, aes(est_age_onset, `Age at Onset`)) + geom_point(colour = "#00ba38") +
  labs(x = "Estimated Age at Onset", y = "Observed Age at Onset") +
  stat_smooth(method = "lm", size = 1.3) +
  stat_cor(label.y = 82) +
  scale_y_continuous(breaks = seq(30, 80, by = 10)) +
  scale_x_continuous(breaks = seq(30, 80, by = 10)) +
  theme_classic() +
  ggtitle(sprintf("GRN (n = %s)", n_grn))

cor(grn$est_age_onset, grn$`Age at Onset`)

mapt <- subset(symp_eyo, `Genetic Group` == 'MAPT')
n_mapt <- nrow((mapt))
mean(abs(mapt$diff))
min(abs(mapt$diff))
max(abs(mapt$diff))

#histogram
mapt_hist <- ggplot(mapt, aes(diff)) + 
  geom_histogram(binwidth = 1, fill = "#83b0fc") +
  labs(x = "Observed - Estimated Age of Onset", y = "Number of Subjects") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(limits = c(-20,20), breaks = seq(-20, 20, by = 5))

#scatterplot
mapt_scatter <- ggplot(mapt, aes(est_age_onset, `Age at Onset`)) + geom_point(colour = "#83b0fc") +
  labs(x = "Estimated Age at Onset", y = "Observed Age at Onset") +
  stat_smooth(method = "lm", size = 1.3) +
  stat_cor(label.y = 70) +
  theme_classic() +
  scale_y_continuous(breaks = seq(30, 80, by = 10)) +
  scale_x_continuous(breaks = seq(30, 80, by = 10)) +
  ggtitle(sprintf("MAPT (n = %s)", n_mapt))

cor(mapt$`Age at Onset`, mapt$est_age_onset)

#combined figure
combined_gene <- annotate_figure(ggarrange(c9_scatter, grn_scatter, mapt_scatter, c9_hist, grn_hist, mapt_hist), 
                top = text_grob("Accuracy of Estimated Years to Onset: By Genetic Group", 
                                              face = "bold", size = 14))
#save figures
ggsave("C:/Users/Admin/Documents/McGill/PhD_Project/eyo_all.png", combined_all, 
       width = 10.6, height = 4.84)
ggsave("C:/Users/Admin/Documents/McGill/PhD_Project/eyo_gene.png", combined_gene,
       width = 9.46, height = 5.89)
