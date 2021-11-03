packages <- c("ggplot2", "dplyr", "lavaan", "plyr", "cowplot", "rmarkdown",
              "readr", "caTools", "bitops")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

#read in file
library(readr)
df<-read_csv(file = "Documents/NGG Quant/allen_data.csv")
df<-df[1:50,]
df$subj<-as.factor(df$subj)
df$Condition<-as.factor(df$Condition)

#create violin plot
fig1<-ggplot(df, aes(Condition,d.prime,fill=Condition)) +
  geom_violin() +
  geom_point()+
  geom_jitter(width=0.15, alpha=0.5)+
  geom_boxplot(width = .2, position = position_dodge(.9)) +
  theme_minimal()+
  scale_color_OkabeIto()+
  scale_fill_OkabeIto()+
  theme(legend.position = "none")

# create raincloud plot
library(PupillometryR)
library(cowplot)
library(colorblindr)
fig2<-ggplot(df,aes(x=Condition,y=d.prime, fill = Condition, colour = Condition))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(Condition)+0.25, y = d.prime),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  ylab('d.prime')+xlab('Condition')+coord_flip()+theme_cowplot()+guides(fill = "none", colour = "none") +
  scale_color_OkabeIto()+
  scale_fill_OkabeIto()

#show figures
fig1
fig2

#check figures for colorblind accommodations 
cvd_grid(fig1)
cvd_grid(fig2)


