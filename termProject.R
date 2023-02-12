library(dplyr)
library(ggplot2)
library(depmixS4)
library(ggbiplot)
library(mice)
#setwd("C:/Users/Omar Elshehawi/Desktop/CMPT 318/Term Project - 318")

#--------------1. Feature Engineering-------------------

df <- read.table("TermProjectData.txt", header = T, sep = ",")
df$Week <- strftime(df$Date, format = "%V")
df$Day<- weekdays(as.Date(df$Date))

init = mice(df, maxit=0) 
predM = init$predictorMatrix
set.seed(100)
imputed = mice(df, predictorMatrix=predM, m=5)
df <- complete(imputed)
  
scaled_df <- df %>% mutate_at(c('Global_active_power','Global_reactive_power','Voltage','Global_intensity','Sub_metering_1','Sub_metering_2','Sub_metering_3'),~(scale(.) %>% as.vector())) 

#mean(scaled_df$Voltage , na.rm=TRUE)
#sd(scaled_df$Voltage, na.rm=TRUE)
df_omit <- na.omit(df)
df_omit2 <- na.omit(scaled_df)
  
pca <- prcomp(df_omit[3:9],scale=TRUE)
pca2 <- prcomp(df_omit2[3:9])

plot(pca$x[,1],pca$x[,2])

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
barplot(pca.var.per,main="Scree Plot", xlab="PC",ylab="percent variation")
summary(pca)

pca.data <- data.frame(Sample=rownames(pca$x), X=pca$x[,1], Y=pca$x[,2])
ggplot(data=pca.data, aes(x=X,y=Y,label=Sample))+geom_text()+xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

ggbiplot(pca, elipse=TRUE)

loading_scores <- pca$rotation[,1]
comp_scores <- abs(loading_scores)
comp_score_ranked <- sort(comp_scores, decreasing=TRUE)
pca$rotation[names(comp_score_ranked),1]

library(factoextra)
var <- get_pca_var(pca)
head(var$contrib)
#---------------------2. HMM--------------------------

train_df <- (filter(scaled_df, (scaled_df$Day == "Monday") & (scaled_df$Time >= "16:00:00") & (scaled_df$Time < "20:00:00")))
###########
ntimes_test <- rep(240, 31)
ntimes_train <- rep(240, 123)
index_split_filled <- sum(ntimes_test_filled)
test_df <- slice(train_df_filledNA, (1:index_split_filled))
train_df <- slice(train_df_filledNA, -(1:index_split_filled))


# train multivariate models
set.seed(1)
mod21 <- depmix(response = list(train_df$Global_intensity ~ 1, train_df$Global_reactive_power ~ 1), data = train_df, nstates = 4, 
               family=list(multinomial("identity"),multinomial("identity")), ntimes = ntimes_train)
fm21 <- fit(mod21)
Ll_train21 <- (forwardbackward(mod21, return.all = FALSE)$logLike)/nrow(train_df)
###############
mod22 <- depmix(response = list(train_df$Global_intensity ~ 1, train_df$Global_reactive_power ~ 1), data = train_df, nstates = 8, 
               family=list(multinomial("identity"),multinomial("identity")), ntimes = ntimes_train)
fm22 <- fit(mod22)
Ll_train22 <- (forwardbackward(mod22, return.all = FALSE)$logLike)/nrow(train_df)
#########
mod23 <- depmix(response = list(train_df$Global_intensity ~ 1, train_df$Global_reactive_power ~ 1), data = train_df, nstates = 12, 
               family=list(multinomial("identity"),multinomial("identity")), ntimes = ntimes_train)
fm23 <- fit(mod23)
Ll_train2 <- (forwardbackward(mod23, return.all = FALSE)$logLike)/nrow(train_df)
###########
mod2 <- depmix(response = list(train_df$Global_intensity ~ 1, train_df$Global_reactive_power ~ 1), data = train_df, nstates = 15, 
               family=list(multinomial("identity"),multinomial("identity")), ntimes = ntimes_train)
fm2 <- fit(mod2)
Ll_train2 <- (forwardbackward(mod2, return.all = FALSE)$logLike)/nrow(train_df)

mod3 <- depmix(response = list(train_df$Global_intensity ~ 1, train_df$Global_reactive_power ~ 1), data = train_df, nstates = 18, 
               family=list(multinomial("identity"),multinomial("identity")), ntimes = ntimes_train)
fm3 <- fit(mod3)
Ll_train3 <- (forwardbackward(mod3, return.all = FALSE)$logLike)/nrow(train_df)

mod4 <- depmix(response = list(train_df$Global_intensity ~ 1, train_df$Global_reactive_power ~ 1), data = train_df, nstates = 20, 
               family=list(multinomial("identity"),multinomial("identity")), ntimes = ntimes_train)
fm4 <- fit(mod4)
Ll_train4 <- (forwardbackward(mod4, return.all = FALSE)$logLike)/nrow(train_df)



###testing####
mod2_test <- depmix(list(test_df$Global_intensity~1, test_df$Global_reactive_power~1), data=test_df, nstates=20, family=list(gaussian(),gaussian()), ntimes=ntimes_test)
mod2_test <- setpars(mod2_test,getpars(fm4))
forwardbackward(mod2_test)

#---------------------3. Anomalies----------------------

df_anomaly1 <- na.omit(read.table("DataWithAnomalies1.txt", header = T, sep = ","))
df_anomaly1$Date <- as.POSIXlt(df_anomaly1$Date, format = "%d/%m/%Y")
df_anomaly1$Day<- weekdays(df_anomaly1$Date)
df_anomaly1$Week <- strftime(df_anomaly1$Date, format = "%V")
df_anomaly1 <- filter(df_anomaly1, df_anomaly1$Day == "Monday")

df_anomaly2 <- na.omit(read.table("DataWithAnomalies2.txt", header = T, sep = ","))
df_anomaly2$Date <- as.POSIXlt(df_anomaly2$Date, format = "%d/%m/%Y")
df_anomaly2$Day<- weekdays(df_anomaly2$Date)

df_anomaly3 <- na.omit(read.table("DataWithAnomalies3.txt", header = T, sep = ","))
df_anomaly3$Date <- as.POSIXlt(df_anomaly3$Date, format = "%d/%m/%Y")
df_anomaly3$Day<- weekdays(df_anomaly3$Date)

week <- 50
year <- 2009

anomaly1 <- data.frame()
#anomaly2 <- data.frame()

ntimes_anom <- c()

while(year <= 2010) {
  while(week <= 53) {
    # week_df <- filter(df_anomaly2, (df_anomaly2$Week == formatC(week, width=2, flag=0) &
    #                                   format(df_anomaly2$Date, format="%Y")==year ))
    # period_df <- filter(week_df, week_df$Time>= "16:00:00" & week_df$Time < "20:00:00") 
    # anomaly2 <- rbind(anomaly2, period_df)  
    # 
    week_df <- filter(df_anomaly1, (df_anomaly1$Week == formatC(week, width=2, flag=0) &
                                      format(df_anomaly1$Date, format="%Y")==year ))
    # get a single time window = 4 hour sunset period
    period_df <- filter(week_df, week_df$Time>= "16:00:00" & week_df$Time < "20:00:00") 
    # concatenate weekly time windows to create training data
    anomaly1 <- rbind(anomaly1, period_df)  
    # record size of each time window (account for removed NA rows)
    if (nrow(period_df) != 0) {
      ntimes_anom <- append(ntimes_anom, nrow(period_df))
    }
    week <- week+1
  }
  year <- year+1
  week <- 1
}

anomaly2 <- na.omit(filter(df_anomaly2, (df_anomaly2$Day == "Monday") & (df_anomaly2$Time >= "16:00:00") & (df_anomaly2$Time < "20:00:00")))

anomaly3 <- na.omit(filter(df_anomaly3, (df_anomaly3$Day == "Monday") & (df_anomaly3$Time >= "16:00:00") & (df_anomaly3$Time < "20:00:00")))


mod_anomaly1 <- depmix(list(anomaly1$Global_intensity~1, anomaly1$Global_reactive~1), data=anomaly1, nstates=10, family=list(multinomial("identity"),multinomial("identity")), ntimes=ntimes_anom)
mod_anomaly1<-setpars(mod_anomaly1, getpars(fm2))
forwardbackward(mod_anomaly1)

mod_anomaly2 <- depmix(list(anomaly2$Global_intensity~1, anomaly2$Global_reactive~1), data=anomaly2, nstates=10, family=list(multinomial("identity"),multinomial("identity")), ntimes=ntimes_anom)
mod_anomaly2<-setpars(mod_anomaly2, getpars(fm2))
forwardbackward(mod_anomaly2)

mod_anomaly3 <- depmix(list(anomaly3$Global_intensity~1, anomaly3$Global_reactive~1), data=anomaly3, nstates=10, family=list(multinomial("identity"),multinomial("identity")), ntimes=ntimes_anom)
mod_anomaly3<-setpars(mod_anomaly3, getpars(fm2))
forwardbackward(mod_anomaly3)


