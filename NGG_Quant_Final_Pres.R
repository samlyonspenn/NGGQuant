packages <- c("ggplot2", "dplyr", "lavaan", "plyr", "cowplot", "rmarkdown",
              "readr", "caTools", "bitops")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

#read in file
library(readr)
df<-read_csv(file = "Documents/NGG Quant/Allen_etal_2016_MODIFIED_DATA.csv")
df$subj<-as.factor(df$subj)
df$Condition<-as.factor(df$Condition)


library(dplyr)
library(ggpubr)

#power analysis and plot
library(pwr2)
pwr.2way(a=2,b=2,alpha=0.05,size.A=20,size.B=20,f.A=0.3, f.B=0.3)
df.power<-read_csv(file = "Documents/NGG Quant/power.csv")
ggplot(df.power, aes(N,Power))+
  geom_point()+
  geom_smooth(method=loess)+
  theme_minimal()+
  geom_hline(yintercept=0.80, linetype="dotted")+
  geom_vline(xintercept=22, linetype="dotted")



#qq plot to check normality
qqnorm(df$t2ca, pch = 1, frame = FALSE)
qqline(df$t2ca, col = "steelblue", lwd = 2)

#shapiro wilkes test to check normality
shapiro.test(df$t2ca)

#raincloud plot of M-bias across conditions
ggplot(df,aes(x=Condition,y=t2ca, fill = Condition, colour = Condition))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(Condition)+0.25, y = t2ca),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  ylab('t2ca')+xlab('Condition')+coord_flip()+theme_cowplot()+guides(fill = "none", colour = "none") +
  scale_color_OkabeIto()+
  scale_fill_OkabeIto()+
  ylab("M-Bias")

#bartlett test to check normality, also a res X fit plot
bartlett.test(t2ca ~ Condition, data = df)
df.aov<-aov(t2ca ~ Condition, data=df)
plot(df.aov,1)
summary(df.aov)

#run 2 x 2 anova: noise (high vs. low), arousal (arousing vs. neutral)
df2<-read_csv(file = "Documents/NGG Quant/Allen_etal_2016_anova.csv")
df2$subj<-as.factor(df2$subj)
df2$Arousal<-as.factor(df2$Arousal)
df2$Noise<-as.factor(df2$Noise)
df.aov2 <- aov(t2ca ~ Arousal * Noise, data = df2)
summary(df.aov2)
TukeyHSD(df.aov2) #post-hoc comparisons


#load heart rate data
df.hrv<-read_csv(file = "Documents/NGG Quant/example.beats.csv")
#plot heart acceleration by high vs. low confidence
ggplot(df.hrv, aes(Time,Acc, group=Confidence, fill=Confidence)) + 
  geom_smooth(method=loess, se=TRUE)+
  theme_minimal()+
  ylab("Heart acceleration")+xlab("Time (ms)")+
  scale_color_OkabeIto()+
  scale_fill_OkabeIto()+
  geom_vline(xintercept=c(1000), linetype="dotted")
