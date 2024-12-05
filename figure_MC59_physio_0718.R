

######################################################### Figure coral physiology #########################################################
library(rlang)
library(ggplot2)
library(readxl)
library(devtools)
library(plyr)
library(tidyr)
library(ggpubr)


######################## define color #######################
color_prob = c("#e1a800","#732479")
color_OA = c("#e2e2e2ff","#badf7cff","#4f8d57ff")
color_ProbNi = c("#D8CBC7", "#7A9B76","#7F557D","#673C4F")
# color_group = c("#E8E9EB","#1C7293","#1C3A6F","#BB4430","#53131E","#e2e2e2ff","#badf7cff","#4f8d57ff","#03a6c8ff","#077393ff")


######################## input file ########################
setwd("/Users/nanxiang/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/July/OA_MC59/R")
df = read_excel("./input/data_physio_OA_MC59_0707.xlsx")
df$Day = factor(df$Day, levels = c("0", "14","21"))
df$OA = factor(df$OA, levels = c("Ambient", "High OA", "Super high OA" ))
df$Ni = factor(df$Ni, levels = c("Non-Ni", "Ni"))
df$Probiotic = factor(df$Probiotic, levels = c("Non-prob","Probiotic"))
df$ProbNi = paste (df$Probiotic, df$Ni, sep = "+")
df$ProbNi = factor(df$ProbNi, levels = c("Non-prob+Non-Ni","Non-prob+Ni","Probiotic+Non-Ni", "Probiotic+Ni"))
df$ID=paste(df$Day, df$OA, df$ProbNi, sep = "_")


######################## PAM ########################
df$PAM = as.numeric(df$PAM)
sum.PAM <- ddply(df, c("ID"), summarise,
                 N = length (PAM),
                 mean = mean(PAM),
                 sd   = sd(PAM),
                 se   = sd / sqrt(N))
sum.PAM = separate(sum.PAM, ID, into = c("Day", "OA", "ProbNi"), sep = "_", remove = F)

sum.PAM$Day <- ordered(sum.PAM$Day, levels = c("0", "14","21"))
sum.PAM$OA <- ordered(sum.PAM$OA, levels = c("Ambient","High OA", "Super high OA"))
sum.PAM$ProbNi <- ordered(sum.PAM$ProbNi, levels = c("Non-prob+Non-Ni","Non-prob+Ni","Probiotic+Non-Ni", "Probiotic+Ni"))
sum.PAM$group=paste(sum.PAM$OA, sum.PAM$ProbNi, sep = "_")

PAM = ggplot(sum.PAM, aes(x=OA, y=mean, fill=ProbNi)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  labs (title="Photosynthetic efficiency", x="Day", y = "Dark-adapted quantym yield [Fv/Fm]") +
  facet_grid(~Day)   +
  theme_classic() + 
  scale_fill_manual(values=color_ProbNi) +
  theme(legend.key.size = unit(0.5, "cm"), legend.key.width = unit(0.5,"cm"), legend.position = 'right')  +
  guides(fill=guide_legend(ncol=2)) +
  coord_cartesian (ylim=c(0.3, 0.75)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


######################## Pnet ########################
df$Pnet = as.numeric(df$Pnet)
sum.Pnet <- ddply(df, c("ID"), summarise,
                  N = length (Pnet),
                  mean = mean(Pnet),
                  sd   = sd(Pnet),
                  se   = sd / sqrt(N))
sum.Pnet = separate(sum.Pnet, ID, into = c("Day", "OA", "ProbNi"), sep = "_", remove = F)

sum.Pnet$Day <- ordered(sum.Pnet$Day, levels = c("0", "14","21"))
sum.Pnet$OA <- ordered(sum.Pnet$OA, levels = c("Ambient","High OA", "Super high OA"))
sum.Pnet$ProbNi <- ordered(sum.Pnet$ProbNi, levels = c("Non-prob+Non-Ni","Non-prob+Ni","Probiotic+Non-Ni", "Probiotic+Ni"))
sum.Pnet$group=paste(sum.Pnet$OA, sum.Pnet$ProbNi, sep = "_")

Pn = ggplot(sum.Pnet, aes(x=OA, y=mean, fill=ProbNi)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  labs (title="Net photosynthesis", x="Day", y = "Oxygen flux (mg O2 hr^-1)") +
  facet_grid(~Day)   +
  theme_classic() +
  scale_fill_manual(values=color_ProbNi) +
  theme( legend.key.size = unit(0.5, "cm"), legend.key.width = unit(0.5,"cm"), legend.position = 'right')  +
  guides(fill=guide_legend(ncol=2)) +
  coord_cartesian (ylim=c(-0, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


######################## Pg ########################
df$Pg = as.numeric(df$Pg)
sum.Pg <- ddply(df, c("ID"), summarise,
                N = length (Pg),
                mean = mean(Pg),
                sd   = sd(Pg),
                se   = sd / sqrt(N))
sum.Pg = separate(sum.Pg, ID, into = c("Day", "OA", "ProbNi"), sep = "_", remove = F)

sum.Pg$Day <- ordered(sum.Pg$Day, levels = c("0", "14", "21"))
sum.Pg$OA <- ordered(sum.Pg$OA, levels = c("Ambient","High OA", "Super high OA"))
sum.Pg$ProbNi <- ordered(sum.Pg$ProbNi, levels = c("Non-prob+Non-Ni","Non-prob+Ni","Probiotic+Non-Ni", "Probiotic+Ni"))
sum.Pg$group=paste(sum.Pg$OA, sum.Pg$ProbNi, sep = "_")

Pg = ggplot(sum.Pg, aes(x=OA, y=mean, fill=ProbNi)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  labs (title="Gross photosynthesis", x="Day", y = "Oxygen flux (mg O2 hr^-1)") +
  facet_grid(~Day)   +
  theme_classic() +
  scale_fill_manual(values=color_ProbNi) +
  theme( legend.key.size = unit(0.5, "cm"), legend.key.width = unit(0.5,"cm"), legend.position = 'right')  +
  guides(fill=guide_legend(ncol=2)) +
  coord_cartesian (ylim=c(0, 2.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


######################## Resp ########################
df$Resp = as.numeric(df$R)
sum.Resp <- ddply(df, c("ID"), summarise,
                  N = length (R),
                  mean = mean(R),
                  sd   = sd(R),
                  se   = sd / sqrt(N))
sum.Resp = separate(sum.Resp, ID, into = c("Day", "OA", "ProbNi"), sep = "_", remove = F)

sum.Resp$Day <- ordered(sum.Resp$Day, levels = c("0", "14", "21"))
sum.Resp$OA <- ordered(sum.Resp$OA, levels = c("Ambient","High OA", "Super high OA"))
sum.Resp$ProbNi <- ordered(sum.Resp$ProbNi, levels = c("Non-prob+Non-Ni","Non-prob+Ni","Probiotic+Non-Ni", "Probiotic+Ni"))
sum.Resp$group=paste(sum.Resp$OA, sum.Resp$ProbNi, sep = "_")

Resp = ggplot(sum.Resp, aes(x=OA, y=mean, fill=ProbNi)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  labs (title="Respiration", x="Day", y = "Oxygen flux (mg O2 hr^-1)") +
  facet_grid(~Day)   +
  theme_classic() +
  scale_fill_manual(values=color_ProbNi) +
  theme( legend.key.size = unit(0.5, "cm"), legend.key.width = unit(0.5,"cm"), legend.position = 'right')  +
  guides(fill=guide_legend(ncol=2)) +
  coord_cartesian (ylim=c(0, 1.3)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


pdf("./output/MC59_photophysio_0802.pdf", width=8.5,height=8.5, pointsize = 12)
ggarrange(PAM, Pn, Pg, Resp + rremove("x.text"),
          legend = TRUE,
          # legend="bottom",
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
dev.off()





######################## light calci ########################
df$calci_lig = as.numeric(df$calci_lig)
sum.calci_lig <- ddply(df, c("ID"), summarise,
                       N = length (calci_lig),
                       mean = mean(calci_lig),
                       sd   = sd(calci_lig),
                       se   = sd / sqrt(N))
sum.calci_lig = separate(sum.calci_lig, ID, into = c("Day", "OA", "ProbNi"), sep = "_", remove = F)

sum.calci_lig$Day <- ordered(sum.calci_lig$Day, levels = c("0", "14", "21"))
sum.calci_lig$OA <- ordered(sum.calci_lig$OA, levels = c("Ambient","High OA", "Super high OA"))
sum.calci_lig$ProbNi <- ordered(sum.calci_lig$ProbNi, levels = c("Non-prob+Non-Ni","Non-prob+Ni","Probiotic+Non-Ni", "Probiotic+Ni"))
sum.calci_lig$group=paste(sum.calci_lig$OA, sum.calci_lig$ProbNi, sep = "_")

calci_lig = ggplot(sum.calci_lig, aes(x=OA, y=mean, fill=ProbNi)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  labs (title="Light calcification", x="Day", y = "light calcification (umol CaCO3 cm-2 h-1)") +
  facet_grid(~Day)   +
  theme_classic() + 
  scale_fill_manual(values=color_ProbNi) +
  theme(legend.key.size = unit(0.5, "cm"), legend.key.width = unit(0.5,"cm"), legend.position = 'right')  +
  guides(fill=guide_legend(ncol=2)) +
  coord_cartesian (ylim=c(-1, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


######################## dark calci ########################
df$calci_dar = as.numeric(df$calci_dar)
sum.calci_dar <- ddply(df, c("ID"), summarise,
                       N = length (calci_dar),
                       mean = mean(calci_dar),
                       sd   = sd(calci_dar),
                       se   = sd / sqrt(N))
sum.calci_dar = separate(sum.calci_dar, ID, into = c("Day", "OA", "ProbNi"), sep = "_", remove = F)

sum.calci_dar$Day <- ordered(sum.calci_dar$Day, levels = c("0", "14", "21"))
sum.calci_dar$OA <- ordered(sum.calci_dar$OA, levels = c("Ambient","High OA", "Super high OA"))
sum.calci_dar$ProbNi <- ordered(sum.calci_dar$ProbNi, levels = c("Non-prob+Non-Ni","Non-prob+Ni","Probiotic+Non-Ni", "Probiotic+Ni"))
sum.calci_dar$group=paste(sum.calci_dar$OA, sum.calci_dar$ProbNi, sep = "_")

calci_dar = ggplot(sum.calci_dar, aes(x=OA, y=mean, fill=ProbNi)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  labs (title="Dark calcification", x="Day", y = "dark calcification (umol CaCO3 cm-2 h-1)") +
  facet_grid(~Day)   +
  theme_classic() + 
  scale_fill_manual(values=color_ProbNi) +
  theme(legend.key.size = unit(0.5, "cm"), legend.key.width = unit(0.5,"cm"), legend.position = 'right')  +
  guides(fill=guide_legend(ncol=2)) +
  coord_cartesian (ylim=c(-1, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


pdf("./output/MC59_calcification_0802.pdf", width=8.5,height=8.5, pointsize = 12)
ggarrange(calci_lig, calci_dar + rremove("x.text"),
          legend = TRUE,
          # legend="bottom",
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
dev.off()




######################## Bouyant weight ########################
df$bouyant_weight = as.numeric(df$bouyant_weight)
sum.bouyant_weight <- ddply(df, c("ID"), summarise,
                            N = length (bouyant_weight),
                            mean = mean(bouyant_weight),
                            sd   = sd(bouyant_weight),
                            se   = sd / sqrt(N))
sum.bouyant_weight = separate(sum.bouyant_weight, ID, into = c("Day", "OA", "ProbNi"), sep = "_", remove = F)

sum.bouyant_weight$Day <- ordered(sum.bouyant_weight$Day, levels = c("21"))
sum.bouyant_weight$OA <- ordered(sum.bouyant_weight$OA, levels = c("Ambient","High OA", "Super high OA"))
sum.bouyant_weight$ProbNi <- ordered(sum.bouyant_weight$ProbNi, levels = c("Non-prob+Non-Ni","Non-prob+Ni","Probiotic+Non-Ni", "Probiotic+Ni"))
sum.bouyant_weight$group=paste(sum.bouyant_weight$OA, sum.bouyant_weight$ProbNi, sep = "_")

ggplot(subset(sum.bouyant_weight, Day == "21"), aes(x=OA, y=mean, fill=ProbNi)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  labs (title="Bouyant weight", x="Day", y = "Bouyant weight (g)") +
  facet_grid(~Day)   +
  theme_classic() + 
  scale_fill_manual(values=color_ProbNi) +
  theme(legend.key.size = unit(0.5, "cm"), legend.key.width = unit(0.5,"cm"), legend.position = 'right')  +
  guides(fill=guide_legend(ncol=2)) +
  coord_cartesian (ylim=c(-0.25, 0.25)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))






