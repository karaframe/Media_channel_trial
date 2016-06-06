
# Set-up ---------------------------
# Load packages
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(gtools)
library(rCharts)
library(threadr)
library(readr)
library(corrplot)
library(relaimpo)
library(pheatmap) 
library(gplots)

# Set global options
options(stringsAsFactors = FALSE)

# Clear all objects
# rm(list = ls(all = TRUE))

setwd("C:/Exercise")
Data <- read.csv("Data.csv")
# remove last record
Data <- Data[1:(nrow(Data)-1),]   # possible outlier (why is it there?????)
Data$ID <- 1:nrow(Data)



####### Data exploration ###############################################
########################################################################

# cells (respondent source) and their weight
# grouping data by cell and conunts bt media type

summary_cells <- Data %>% 
  group_by(cell) %>% 
  summarise(weight = sum(rweight, na.rm = TRUE))
summary_cells$perc_weight <- ((summary_cells$weight)/nrow(Data))*100
summary_cells$cell <- as.factor(summary_cells$cell)

# calculate midpoints of bars
summary_cells <- ddply(summary_cells, .(cell), 
                       transform, pos = cumsum(perc_weight) - (0.5 * perc_weight)
)


jpeg("C:/Exercise/plots/weight_cells.jpg",
     quality = 100, bg = "white", res = 200, width = 7, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)


p <- ggplot(data = summary_cells,
            aes(cell, perc_weight, fill = cell)) +
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  scale_fill_manual(values=c("#7f7fff", "#E69F00", "#a6a6a6", "#66b266")) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                            # Remove x-axis label
  ylab("weight (%)") +                                                        # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  xlab("respondent source (cell)") +          
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=15),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=18)) +
  geom_text(aes(label = paste(round(perc_weight), "%", sep = ""), y = pos), size = 8) +
  ggtitle("Weight (%) of each respondent source") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",  size=20))
p



par(oldpar)
dev.off()



# sum all data by media channels
summary_media <- Data %>% 
  summarise(tv = sum(tv, na.rm = TRUE),
            radio = sum(radio, na.rm = TRUE),
            ooh = sum(ooh, na.rm = TRUE),
            online = sum(online, na.rm = TRUE),
            social = sum(social, na.rm = TRUE))
row.names(summary_media) <- "counts"
summary_media <- t(summary_media)
summary_media <- as.data.frame(summary_media)
summary_media$media <- rownames(summary_media)
row.names(summary_media) <- NULL


# calculate midpoints of bars
summary_media <- ddply(summary_media, .(media), 
                       transform, pos = cumsum(counts) - (0.5 * counts)
)


jpeg("C:/Exercise/plots/respondents_media.jpg",
     quality = 100, bg = "white", res = 200, width = 7, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)

p <- ggplot(data = summary_media,
            aes(media, counts, fill = media)) +
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
  scale_fill_manual(values=c("#7f7fff", "#7fbf7f", "#ff0000", "#e5e500", "#8e8e8e")) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                            # Remove x-axis label
  ylab("Counts") +                                                        # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=18)) +
  xlab(" ") +          
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=15),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=18)) +
  geom_text(aes(label = round(counts), y = pos), size = 8) +
  ggtitle("Total respondents by media") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
p


par(oldpar)
dev.off()


# sum all data by media and by cell (respondent source)

summary_media_cell <- Data %>% 
  group_by(cell) %>%
  summarise(tv = (sum(tv, na.rm = TRUE)/738)*100,
            radio = (sum(radio, na.rm = TRUE)/464)*100,
            ooh = (sum(ooh, na.rm = TRUE)/382)*100,
            online = (sum(online, na.rm = TRUE)/555)*100,
            social = (sum(social, na.rm = TRUE)/541)*100)

summary_media_cell <- cbind(summary_media_cell[1], stack(summary_media_cell[2:6]))
colnames(summary_media_cell)[2] <- "contribution"
colnames(summary_media_cell)[3] <- "media"
summary_media_cell$cell <- as.factor(summary_media_cell$cell)

# calculate midpoints of bars
summary_media_cell$pos <- (summary_media_cell$contribution)/2


jpeg("C:/Exercise/plots/respondents_media_and_cell.jpg",
     quality = 100, bg = "white", res = 200, width = 13, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)


q <- ggplot(data = summary_media_cell, 
            aes(cell, contribution, fill = cell)) +
  geom_bar(stat = "identity") + facet_grid(. ~ media) + guides(fill=FALSE) +
  theme(strip.text.x = element_text(size = 22, colour = "black")) +
  scale_fill_manual(values=c("#7f7fff", "#E69F00", "#a6a6a6", "#66b266")) +
  theme( strip.text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  
  ylab("Contribution (%)") +            
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  xlab("respondent source (cell)") +           
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=15),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=18)) +
  geom_text(aes(label = paste(round(contribution), "%", sep = ""), y = pos), size = 6) +
  ggtitle("Contribution of respondent source for each channel") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
q


par(oldpar)
dev.off()

###################################################################################

# sum all data by KPI
summary_KPI <- Data %>% 
  summarise(sp_aw = sum(sp_aw, na.rm = TRUE),
            relv = sum(relv, na.rm = TRUE),
            laugh = sum(laugh, na.rm = TRUE),
            think = sum(think, na.rm = TRUE),
            recom = sum(recom, na.rm = TRUE),
            talk = sum(talk, na.rm = TRUE),
            purchase = sum(purchase, na.rm = TRUE),
            intent = sum(intent, na.rm = TRUE))
row.names(summary_KPI) <- "counts"
summary_KPI <- t(summary_KPI)
summary_KPI <- as.data.frame(summary_KPI)
summary_KPI$KPI <- rownames(summary_KPI)
row.names(summary_KPI) <- NULL


# calculate midpoints of bars
summary_KPI <- ddply(summary_KPI, .(KPI), 
                       transform, pos = cumsum(counts) - (0.5 * counts)
)


jpeg("C:/Exercise/plots/respondents_KPI.jpg",
     quality = 100, bg = "white", res = 200, width = 11, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)

p <- ggplot(data = summary_KPI,
            aes(KPI, counts, fill = KPI)) +
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
  scale_fill_manual(values=c("#7f7fff","#99cc99", "#ff0000", "#e5e500", "#4ca64c", "#ccccff", 
                             "#bfbfbf", "#E69F00")) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                            # Remove x-axis label
  ylab("Counts") +                                                        # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=18)) +
  xlab(" ") +          
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=15),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=18)) +
  geom_text(aes(label = round(counts), y = pos), size = 8) +
  ggtitle("Total respondents by KPI") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
p


par(oldpar)
dev.off()




# sum all data by KPIs and by cell (respondent source)
summary_KPI_cell <- Data %>% 
  group_by(cell) %>%
  summarise(sp_aw = (sum(sp_aw, na.rm = TRUE)/553)*100,
            relv = (sum(relv, na.rm = TRUE)/1180)*100,
            laugh = (sum(laugh, na.rm = TRUE)/1279)*100,
            think = (sum(think, na.rm = TRUE)/1044)*100,
            recom = (sum(recom, na.rm = TRUE)/1474)*100,
            talk = (sum(talk, na.rm = TRUE)/739)*100,
            purchase = (sum(purchase, na.rm = TRUE)/786)*100,
            intent = (sum(intent, na.rm = TRUE)/1413)*100)

summary_KPI_cell <- cbind(summary_KPI_cell[1], stack(summary_KPI_cell[2:9]))
colnames(summary_KPI_cell)[2] <- "contribution"
colnames(summary_KPI_cell)[3] <- "KPI"
summary_KPI_cell$cell <- as.factor(summary_KPI_cell$cell)

# calculate midpoints of bars
summary_KPI_cell$pos <- (summary_KPI_cell$contribution)/2



jpeg("C:/Exercise/plots/respondents_KPI_and_cell.jpg",
     quality = 100, bg = "white", res = 200, width = 14, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)


q <- ggplot(data = summary_KPI_cell, 
            aes(cell, contribution, fill = cell)) +
  geom_bar(stat = "identity") + facet_grid(. ~ KPI) + guides(fill=FALSE) +
  scale_fill_manual(values=c("#7f7fff", "#E69F00", "#a6a6a6", "#66b266")) +
  theme( strip.text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  
  ylab("Contribution (%)") +            
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size= 18)) +
  xlab("respondent source (cell)") +           
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=15),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=18)) +
  geom_text(aes(label = paste(round(contribution), "%", sep = ""), y = pos), size = 4) +
  ggtitle("Contribution of respondent source for each KPI") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
q

par(oldpar)
dev.off()

### relative impact of each of the channels (media) into driving of each of the KPIs
####################################################################################

###### Functions to be used in coorrelograms plots ##########################
#############################################################################

# funtion to define the p value (significance)
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#############################################################################

### Correlation plot for all the variables


# calculate correlation coefficients between media channels
mydata <- Data %>%
  dplyr::select(-respid,
                -cell,
                -rweight)

M <- cor(mydata, use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))
M[is.na(M)]=0
p.mat <- cor.mtest(M)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))



jpeg("C:/Exercise/plots/Correlations.jpg",
     quality = 100, bg = "white", res = 200, width = 7, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)


# correlation matrix
corrplot(M, method = "ellipse", type="upper", col=col(200),
         addCoef.col = "black", 
         tl.col="black", tl.srt=45, tl.cex = 0.8, 
         diag=FALSE)

par(oldpar)
dev.off()

###########################################################################################
# make a multilinear regression between each KPIs and media channels

#####------------------------------------------------------
# spontaneous brand awareness
mydata_sp_aw <- mydata %>%
  dplyr::select(- relv,
               - laugh,
               - think,
               - recom,
               - talk,
               - purchase,
               - intent)

fit_sp_aw <- lm(sp_aw ~ tv + radio + ooh + online + social,   data = mydata_sp_aw, na.action= na.exclude)
summary(fit_sp_aw) # show results
SIGNIF_sp_aw <- signif(summary(fit_sp_aw)$r.squared, 5) ### R2
SIGNIF_sp_aw <- as.data.frame(SIGNIF_sp_aw)

Coeff_sp_aw <- as.data.frame(coefficients(fit_sp_aw)) # model coefficients (intercef and slope from the regression)

###### STANDARD ERROR ---------------------------------------------------
# if we want to refer to the STANDARD ERRORS for the coefficient estimates the we can calucluated the variance in this way
STD_ERR_Coeff_sp_aw <- as.data.frame(summary(fit_sp_aw)$coefficients[,2])

IMPO_sp_aw <- calc.relimp(fit_sp_aw, type="lmg", rela=TRUE)
IMPACT_sp_aw <- as.data.frame(IMPO_sp_aw@lmg)*100
colnames(IMPACT_sp_aw) <- "IMPACT"
IMPACT_sp_aw$media <- rownames(IMPACT_sp_aw)
IMPACT_sp_aw$KPI <- "sp_aw"
row.names(IMPACT_sp_aw) <- NULL

plot(IMPO_sp_aw, sort = FALSE,
     main ="Relative Importance of media channels on spontaneous brand awareness (KPI)")


#####------------------------------------------------------
# relevance
mydata_relv <- mydata %>%
  dplyr::select(- sp_aw,
                - laugh,
                - think,
                - recom,
                - talk,
                - purchase,
                - intent)

fit_relv <- lm(relv ~ tv + radio + ooh + online + social,   data = mydata_relv, na.action= na.exclude)
summary(fit_relv) # show results
SIGNIF_relv <- signif(summary(fit_relv)$r.squared, 5) ### R2
SIGNIF_relv <- as.data.frame(SIGNIF_relv)

Coeff_relv <- as.data.frame(coefficients(fit_relv)) # model coefficients (intercef and slope from the regression)

STD_ERR_Coeff_relv <- as.data.frame(summary(fit_relv)$coefficients[,2])

IMPO_relv <- calc.relimp(fit_relv, type="lmg", rela=TRUE)
IMPACT_relv <- as.data.frame(IMPO_relv@lmg)*100
colnames(IMPACT_relv) <- "IMPACT"
IMPACT_relv$media <- rownames(IMPACT_relv)
IMPACT_relv$KPI <- "relv"
row.names(IMPACT_relv) <- NULL

plot(IMPO_relv, sort = FALSE,
     main ="Relative Importance of media channels on relevance (KPI)")



#####------------------------------------------------------
# laugh
mydata_laugh <- mydata %>%
  dplyr::select(- sp_aw,
                 - relv,
                - think,
                - recom,
                - talk,
                - purchase,
                - intent)

fit_laugh <- lm(laugh ~ tv + radio + ooh + online + social,   data = mydata_laugh, na.action= na.exclude)
summary(fit_laugh) # show results
SIGNIF_laugh <- signif(summary(fit_laugh)$r.squared, 5) ### R2
SIGNIF_laugh <- as.data.frame(SIGNIF_laugh)

Coeff_laugh <- as.data.frame(coefficients(fit_laugh)) # model coefficients (intercef and slope from the regression)

STD_ERR_Coeff_laugh <- as.data.frame(summary(fit_laugh)$coefficients[,2])

IMPO_laugh <- calc.relimp(fit_laugh, type="lmg", rela=TRUE)
IMPACT_laugh <- as.data.frame(IMPO_laugh@lmg)*100
colnames(IMPACT_laugh) <- "IMPACT"
IMPACT_laugh$media <- rownames(IMPACT_laugh)
IMPACT_laugh$KPI <- "laugh"
row.names(IMPACT_laugh) <- NULL

plot(IMPO_laugh, sort = FALSE,
     main ="Relative Importance of media channels on laugh (KPI)")


#####------------------------------------------------------
# think
mydata_think <- mydata %>%
  dplyr::select(- sp_aw,
                - relv,
                - laugh,
                - recom,
                - talk,
                - purchase,
                - intent)

fit_think <- lm(think ~ tv + radio + ooh + online + social,   data = mydata_think, na.action= na.exclude)
summary(fit_think) # show results
SIGNIF_think <- signif(summary(fit_think)$r.squared, 5) ### R2
SIGNIF_think <- as.data.frame(SIGNIF_think)

Coeff_think <- as.data.frame(coefficients(fit_think)) 

STD_ERR_Coeff_think <- as.data.frame(summary(fit_think)$coefficients[,2])

IMPO_think <- calc.relimp(fit_think, type="lmg", rela=TRUE)
IMPACT_think <- as.data.frame(IMPO_think@lmg)*100
colnames(IMPACT_think) <- "IMPACT"
IMPACT_think$media <- rownames(IMPACT_think)
IMPACT_think$KPI <- "think"
row.names(IMPACT_think) <- NULL

plot(IMPO_think, sort = FALSE,
     main ="Relative Importance of media channels on think (KPI)")



#####------------------------------------------------------
# recom
mydata_recom <- mydata %>%
  dplyr::select(- sp_aw,
                - relv,
                - laugh,
                - think,
                - talk,
                - purchase,
                - intent)

fit_recom <- lm(recom ~ tv + radio + ooh + online + social,   data = mydata_recom, na.action= na.exclude)
summary(fit_recom) # show results
SIGNIF_recom <- signif(summary(fit_recom)$r.squared, 5) ### R2
SIGNIF_recom <- as.data.frame(SIGNIF_recom)

Coeff_recom <- as.data.frame(coefficients(fit_recom)) 

STD_ERR_Coeff_recom <- as.data.frame(summary(fit_recom)$coefficients[,2])

IMPO_recom <- calc.relimp(fit_recom, type="lmg", rela=TRUE)
IMPACT_recom <- as.data.frame(IMPO_recom@lmg)*100
colnames(IMPACT_recom) <- "IMPACT"
IMPACT_recom$media <- rownames(IMPACT_recom)
IMPACT_recom$KPI <- "recom"
row.names(IMPACT_recom) <- NULL

plot(IMPO_recom, sort = FALSE,
     main ="Relative Importance of media channels on recommended (KPI)")



#####------------------------------------------------------
# talk
mydata_talk <- mydata %>%
  dplyr::select(- sp_aw,
                - relv,
                - laugh,
                - think,
                - recom,
                - purchase,
                - intent)

fit_talk <- lm(talk ~ tv + radio + ooh + online + social,   data = mydata_talk, na.action= na.exclude)
summary(fit_talk) # show results
SIGNIF_talk <- signif(summary(fit_talk)$r.squared, 5) ### R2
SIGNIF_talk <- as.data.frame(SIGNIF_talk)

Coeff_talk <- as.data.frame(coefficients(fit_talk)) 

STD_ERR_Coeff_talk <- as.data.frame(summary(fit_talk)$coefficients[,2])

IMPO_talk <- calc.relimp(fit_talk, type="lmg", rela=TRUE)
IMPACT_talk <- as.data.frame(IMPO_talk@lmg)*100
colnames(IMPACT_talk) <- "IMPACT"
IMPACT_talk$media <- rownames(IMPACT_talk)
IMPACT_talk$KPI <- "talk"
row.names(IMPACT_talk) <- NULL

plot(IMPO_talk, sort = FALSE,
     main ="Relative Importance of media channels on talk about the brand (KPI)")


#####------------------------------------------------------
# purchase
mydata_purchase <- mydata %>%
  dplyr::select(- sp_aw,
                - relv,
                - laugh,
                - think,
                - recom,
                - talk,
                - intent)

fit_purchase <- lm(purchase ~ tv + radio + ooh + online + social,   data = mydata_purchase, na.action= na.exclude)
summary(fit_purchase) # show results
SIGNIF_purchase <- signif(summary(fit_purchase)$r.squared, 5) ### R2
SIGNIF_purchase <- as.data.frame(SIGNIF_purchase)

Coeff_purchase <- as.data.frame(coefficients(fit_purchase)) 

STD_ERR_Coeff_purchase <- as.data.frame(summary(fit_purchase)$coefficients[,2])

IMPO_purchase <- calc.relimp(fit_purchase, type="lmg", rela=TRUE)
IMPACT_purchase <- as.data.frame(IMPO_purchase@lmg)*100
colnames(IMPACT_purchase) <- "IMPACT"
IMPACT_purchase$media <- rownames(IMPACT_purchase)
IMPACT_purchase$KPI <- "purchase"
row.names(IMPACT_purchase) <- NULL

plot(IMPO_purchase, sort = FALSE,
     main ="Relative Importance of media channels on the purchase of the brand (KPI)")


#####------------------------------------------------------
# intent
mydata_intent <- mydata %>%
  dplyr::select(- sp_aw,
                - relv,
                - laugh,
                - think,
                - recom,
                - talk,
                - purchase)

fit_intent <- lm(intent ~ tv + radio + ooh + online + social,   data = mydata_intent, na.action= na.exclude)
summary(fit_intent) # show results
SIGNIF_intent <- signif(summary(fit_intent)$r.squared, 5) ### R2
SIGNIF_intent <- as.data.frame(SIGNIF_intent)

Coeff_intent <- as.data.frame(coefficients(fit_intent)) 

STD_ERR_Coeff_intent <- as.data.frame(summary(fit_intent)$coefficients[,2])

IMPO_intent <- calc.relimp(fit_intent, type="lmg", rela=TRUE)
IMPACT_intent <- as.data.frame(IMPO_intent@lmg)*100
colnames(IMPACT_intent) <- "IMPACT"
IMPACT_intent$media <- rownames(IMPACT_intent)
IMPACT_intent$KPI <- "intent"
row.names(IMPACT_intent) <- NULL

plot(IMPO_intent, sort = FALSE,
     main ="Relative Importance of media channels on the intent to purchase the brand (KPI)")

##############

# rbind all IMPACTs for KPIs
IMPACTS_KPI <- rbind(IMPACT_sp_aw, IMPACT_relv, IMPACT_laugh, IMPACT_think,
                     IMPACT_recom, IMPACT_talk, IMPACT_purchase, IMPACT_intent)

# calculate midpoints of bars
IMPACTS_KPI$pos <- (IMPACTS_KPI$IMPACT)/2


jpeg('C:/Exercise/plots/Impacts_media_on_KPIs.jpg',
     quality = 100, bg = "white", res = 200, width = 18, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

q <- ggplot(data = IMPACTS_KPI, 
            aes(media, IMPACT, fill = media)) +
  geom_bar(stat = "identity") + facet_grid(. ~ KPI) +
  theme(strip.text.x = element_text(size = 24, colour = "black")) +
  guides(fill=FALSE) +
  scale_fill_manual(values=c("#7f7fff", "#7fbf7f", "#ff0000", "#e5e500", "#8e8e8e")) +
  theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5)) +
  theme(axis.text.x=element_text(size=18,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                     
  ylab("Relative Impact (%)") +                                                       
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
#  geom_text(aes(label = paste(round(IMPACT), "%", sep = ""), y = pos), size = 4) +
  geom_text(aes(label = paste(round(IMPACT),"%", sep = "")), size = 5, hjust = 0.5, vjust = -0.5) +
  ggtitle("Relative Impact of media channels on each KPI") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
q

par(oldpar)
dev.off()


#####################################################################################
######################################################################################
# corr between media channels

# calculate correlation coefficients between media channels
mydata_channels <- Data %>%
  dplyr::select(tv,
                radio,
                ooh,
                online,
                social)

M <- cor(mydata_channels, use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

jpeg("C:/Exercise/plots/Correlations_channels.jpg",
     quality = 100, bg = "white", res = 200, width = 7, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)


# correlation matrix
corrplot(M, method = "ellipse", type="upper", col=col(200),
         addCoef.col = "black", 
         tl.col="black", tl.srt=45, tl.cex = 0.8, 
         diag=FALSE)

par(oldpar)
dev.off()


###########################################################################################
# make a multilinear regression between media channels

#####------------------------------------------------------
# tv

fit_tv <- lm(tv ~ radio + ooh + online + social, data = mydata_channels, na.action= na.exclude)
summary(fit_tv) # show results
SIGNIF_tv <- signif(summary(fit_tv)$r.squared, 5) ### R2
SIGNIF_tv <- as.data.frame(SIGNIF_tv)

Coeff_tv <- as.data.frame(coefficients(fit_tv)) 
STD_ERR_Coeff_tv <- as.data.frame(summary(fit_tv)$coefficients[,2])

IMPO_tv <- calc.relimp(fit_tv, type="lmg", rela=TRUE)
IMPACT_tv <- as.data.frame(IMPO_tv@lmg)*100
colnames(IMPACT_tv) <- "IMPACT"
IMPACT_tv$channels <- rownames(IMPACT_tv)
IMPACT_tv$media <- "tv"
row.names(IMPACT_tv) <- NULL

plot(IMPO_tv, sort = FALSE,
     main ="Relative Importance of tv on media channels")


#####------------------------------------------------------
# radio

fit_radio <- lm(radio ~ tv + ooh + online + social, data = mydata_channels, na.action= na.exclude)
summary(fit_radio) # show results
SIGNIF_radio <- signif(summary(fit_radio)$r.squared, 5) ### R2
SIGNIF_radio <- as.data.frame(SIGNIF_radio)

Coeff_radio <- as.data.frame(coefficients(fit_radio)) 
STD_ERR_Coeff_radio <- as.data.frame(summary(fit_radio)$coefficients[,2])

IMPO_radio <- calc.relimp(fit_radio, type="lmg", rela=TRUE)
IMPACT_radio <- as.data.frame(IMPO_radio@lmg)*100
colnames(IMPACT_radio) <- "IMPACT"
IMPACT_radio$channels <- rownames(IMPACT_radio)
IMPACT_radio$media <- "radio"
row.names(IMPACT_radio) <- NULL

plot(IMPO_radio, sort = FALSE,
     main ="Relative Importance of radio on media channels")



#####------------------------------------------------------
# ooh

fit_ooh <- lm(ooh ~ radio + tv + online + social, data = mydata_channels, na.action= na.exclude)
summary(fit_ooh) # show results
SIGNIF_ooh <- signif(summary(fit_ooh)$r.squared, 5) ### R2
SIGNIF_ooh <- as.data.frame(SIGNIF_ooh)

Coeff_ooh <- as.data.frame(coefficients(fit_ooh)) 
STD_ERR_Coeff_ooh <- as.data.frame(summary(fit_ooh)$coefficients[,2])

IMPO_ooh <- calc.relimp(fit_ooh, type="lmg", rela=TRUE)
IMPACT_ooh <- as.data.frame(IMPO_ooh@lmg)*100
colnames(IMPACT_ooh) <- "IMPACT"
IMPACT_ooh$channels <- rownames(IMPACT_ooh)
IMPACT_ooh$media <- "ooh"
row.names(IMPACT_ooh) <- NULL

plot(IMPO_ooh, sort = FALSE,
     main ="Relative Importance of ooh on media channels")



#####------------------------------------------------------
# online

fit_online <- lm(online ~ radio + tv + ooh + social, data = mydata_channels, na.action= na.exclude)
summary(fit_online) # show results
SIGNIF_online <- signif(summary(fit_online)$r.squared, 5) ### R2
SIGNIF_online <- as.data.frame(SIGNIF_online)

Coeff_online <- as.data.frame(coefficients(fit_online)) 
STD_ERR_Coeff_online <- as.data.frame(summary(fit_online)$coefficients[,2])

IMPO_online <- calc.relimp(fit_online, type="lmg", rela=TRUE)
IMPACT_online <- as.data.frame(IMPO_online@lmg)*100
colnames(IMPACT_online) <- "IMPACT"
IMPACT_online$channels <- rownames(IMPACT_online)
IMPACT_online$media <- "online"
row.names(IMPACT_online) <- NULL

plot(IMPO_online, sort = FALSE,
     main ="Relative Importance of online on media channels")



#####------------------------------------------------------
# social

fit_social <- lm(social ~ radio + tv + ooh + online, data = mydata_channels, na.action= na.exclude)
summary(fit_social) # show results
SIGNIF_social <- signif(summary(fit_social)$r.squared, 5) ### R2
SIGNIF_social <- as.data.frame(SIGNIF_social)

Coeff_social <- as.data.frame(coefficients(fit_social)) 
STD_ERR_Coeff_social <- as.data.frame(summary(fit_social)$coefficients[,2])

IMPO_social <- calc.relimp(fit_social, type="lmg", rela=TRUE)
IMPACT_social <- as.data.frame(IMPO_social@lmg)*100
colnames(IMPACT_social) <- "IMPACT"
IMPACT_social$channels <- rownames(IMPACT_social)
IMPACT_social$media <- "social"
row.names(IMPACT_social) <- NULL

plot(IMPO_social, sort = FALSE,
     main ="Relative Importance of social on media channels")



##############

# rbind all IMPACTs for all media
IMPACTS_MEDIA <- rbind(IMPACT_tv, IMPACT_radio, IMPACT_ooh, IMPACT_social,
                     IMPACT_online)


# calculate midpoints of bars
IMPACTS_MEDIA <- ddply(IMPACTS_MEDIA, .(media), 
              transform, pos = cumsum(IMPACT) - (0.5 * IMPACT)
)


jpeg('C:/Exercise/plots/media_interactions.jpg',
     quality = 100, bg = "white", res = 200, width = 15, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


q <- ggplot(data = IMPACTS_MEDIA, 
            aes(media, IMPACT, fill = channels)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#7f7fff", "#7fbf7f", "#ff0000", "#e5e500", "#8e8e8e")) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
  theme(axis.text.x=element_text(size=18,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                     
  ylab("Relative Interaction (%)") +                                                       
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=22)) +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=25)) +
  geom_text(aes(label = paste(round(IMPACT), "%", sep = ""), y = pos), size = 9) +
  ggtitle("Interactions between media channels") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
q

par(oldpar)
dev.off()








########################################################################################
# cluster analysis media channels

# add column ID as it is
mydata_channels$ID <- 1:nrow(mydata_channels)


# Store unique identifiers
rownames <- mydata_channels$ID

# Give data frame unique row names
row.names(mydata_channels) <- rownames
mydata_channels$ID <- NULL
# Standardise variables (rescale data based on the meand and Standard deviation)
mydata_channels_standard <- data.frame(scale(mydata_channels))


############### PRINCIPAL COMPONENT ANALYSIS ################

Media.pca <- prcomp(mydata_channels_standard,
                   center = TRUE,
                   scale. = TRUE)

plot(Media.pca)
summary(Media.pca) ### cumulative
Prop_Var <- as.data.frame(Media.pca$sdev^2/sum(Media.pca$sdev^2)*100)


################ CLUSTER ANALYSIS #############################

# Calculate distance matrix
distance <- dist(mydata_channels_standard, method = "euclidean")
see_distance <- as.matrix(distance)

# Do a Ward hierarchical cluster analyis
fit <- hclust(distance, method = "ward.D")

# Dendogram
plot(fit, labels = FALSE)

# Create a cluster vector
# Clusters
k <- 5

# Group ## cut the cluster
cluster <- cutree(fit, k = k)
see_cluster <- as.matrix(cluster)

# Plot
rect.hclust(fit, k = k, border = "red")
# graphics.off()

# Give observations cluster variable 
mydata_post_cluster <- mydata_channels %>% 
  mutate(ID = rownames,         ### add flat name (new column)
         cluster = unname(cluster))   ### add cluster column

# Join cluster group to data
# Select
mydata_post_cluster <- mydata_post_cluster %>% 
  dplyr::select(ID, cluster)


mydata_post_cluster <- Data %>% 
  inner_join(mydata_post_cluster, "ID") ## "ID" is the common field to join

write.csv(mydata_post_cluster, file = "data_post_cluster.csv", row.names=TRUE)


cluster_1 <- subset(mydata_post_cluster, cluster == 1)
cluster_2 <- subset(mydata_post_cluster, cluster == 2)
cluster_3 <- subset(mydata_post_cluster, cluster == 3)
cluster_4 <- subset(mydata_post_cluster, cluster == 4)
cluster_5 <- subset(mydata_post_cluster, cluster == 5)

# make a data frame
clusters <- c("1", "2", "3", "4", "5")
records_clust <- c(1752, 382, 222, 274, 411)
Subgroups_ID <- data.frame(clusters, records_clust)


# calculate midpoints of bars
Subgroups_ID <- ddply(Subgroups_ID, .(clusters), 
                       transform, pos = cumsum(records_clust) - (0.5 * records_clust)
)


jpeg("C:/Exercise/plots/subgroups_ID.jpg",
     quality = 100, bg = "white", res = 200, width = 7, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)


p <- ggplot(data = Subgroups_ID,
            aes(clusters, records_clust, fill = clusters)) +
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  scale_fill_manual(values=c("#7f7fff", "#7fbf7f", "#ff0000", "#e5e500", "#8e8e8e")) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                            # Remove x-axis label
  ylab("Counts") +                                                        # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  xlab("subgroups") +          
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=22)) +
  geom_text(aes(label = round(records_clust), y = pos), size = 8) +
  ggtitle("Subgroups of respondent IDs") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",  size=20))
p


par(oldpar)
dev.off()


############################################################################################


summary_media_cluster <- mydata_post_cluster %>% 
  group_by(cluster) %>%
  summarise(tv = sum(tv, na.rm = TRUE),
            radio = sum(radio, na.rm = TRUE),
            ooh = sum(ooh, na.rm = TRUE),
            online = sum(online, na.rm = TRUE),
            social = sum(social, na.rm = TRUE))

summary_media_cluster <- cbind(summary_media_cluster[1], stack(summary_media_cluster[2:6]))
colnames(summary_media_cluster)[2] <- "counts"
colnames(summary_media_cluster)[3] <- "media"
summary_media_cluster$cluster <- as.factor(summary_media_cluster$cluster)

# calculate midpoints of bars
summary_media_cluster$pos <- (summary_media_cluster$counts)/2


jpeg("C:/Exercise/plots/respondents_media_cluster.jpg",
     quality = 100, bg = "white", res = 200, width = 18, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)


q <- ggplot(data = summary_media_cluster, 
            aes(cluster, counts, fill = cluster)) +
  geom_bar(stat = "identity") + facet_grid(. ~ media) + guides(fill=FALSE) +
  theme(strip.text.x = element_text(size = 24, colour = "black")) +
  scale_fill_manual(values=c("#7f7fff", "#7fbf7f", "#ff0000", "#e5e500", "#8e8e8e")) +
  theme( strip.text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  
  ylab("Counts") +            
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=18)) +
  xlab("Subgroup of respondent IDs") +           
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=19),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=20)) +
  geom_text(aes(label = round(counts), y = pos), size = 7) +
  ggtitle("Categorization of respondent ID for each channel") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
q


par(oldpar)
dev.off()



################################################

# sum all data by KPIs and by cluster 
summary_KPI_cluster <- mydata_post_cluster %>% 
  group_by(cluster) %>%
  summarise(sp_aw = sum(sp_aw, na.rm = TRUE),
            relv = sum(relv, na.rm = TRUE),
            laugh = sum(laugh, na.rm = TRUE),
            think = sum(think, na.rm = TRUE),
            recom = sum(recom, na.rm = TRUE),
            talk = sum(talk, na.rm = TRUE),
            purchase = sum(purchase, na.rm = TRUE),
            intent = sum(intent, na.rm = TRUE))

summary_KPI_cluster <- cbind(summary_KPI_cluster[1], stack(summary_KPI_cluster[2:9]))
colnames(summary_KPI_cluster)[2] <- "counts"
colnames(summary_KPI_cluster)[3] <- "KPI"
summary_KPI_cluster$cluster <- as.factor(summary_KPI_cluster$cluster)

# calculate midpoints of bars
summary_KPI_cluster$pos <- (summary_KPI_cluster$counts)/2



jpeg("C:/Exercise/plots/respondents_KPI_cluster.jpg",
     quality = 100, bg = "white", res = 200, width = 16, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)


q <- ggplot(data = summary_KPI_cluster, 
            aes(cluster, counts, fill = cluster)) +
  geom_bar(stat = "identity") + facet_grid(. ~ KPI) + guides(fill=FALSE) +
  scale_fill_manual(values=c("#7f7fff", "#7fbf7f", "#ff0000", "#e5e500", "#8e8e8e")) +
  theme( strip.text = element_text(size = 22)) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  
  ylab("Counts") +            
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size= 18)) +
  xlab("Subgroup of respondent IDs") +           
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=15),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=18)) +
  geom_text(aes(label = round(counts), y = pos), size = 4) +
  ggtitle("Categorization of respondent ID for each KPI") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
q

par(oldpar)
dev.off()




























#####################################################################################

####### covariance matrix for M #####################################################
#### it is about the relationshp among VARIABLES (columns) of thematrix "mydata" 

# Standardise variables (rescale data based on the meand and Standard deviation)
M_standard <- data.frame(scale(mydata_channels))

#### covariance matrix #################
M_cov <- cov(mydata_channels, use='pairwise') 
M_cov_norm <- cov(M_standard, use='pairwise')  #### standardized

### Covariance Matrix plot

heatmap <- pheatmap(M_cov, col=bluered(256), cluster_cols=TRUE, 
                    fontsize_row=15,fontsize_col=15, border_color=NA, margins=c(7,7))

heatmap <- pheatmap(M_cov_norm, col=bluered(256), cluster_cols=TRUE, 
                    fontsize_row=15,fontsize_col=15, border_color=NA, margins=c(7,7))

