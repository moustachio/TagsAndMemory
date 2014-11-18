library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)

setwd("~/git/TagsAndMemory/")
source("functions.R")

plot_labels <- -6:6

before_cols <- paste0("V",4:9)
after_cols <- paste0("V",11:16)
all_cols <- paste0("V",4:16)

tagged <- get_data("data/tagged.csv",
                   before_cols,after_cols,all_cols)
untagged <- get_data("data/untagged.csv",
                     before_cols,after_cols,all_cols)

##only tagged with max 
tagged <- tagged[V10 >= V4 &
                   V10 >= V5 &
                   V10 >= V6 &
                   V10 >= V7 &
                   V10 >= V8 &
                   V10 >= V9 &
                   V10 >= V11 &
                   V10 >= V12 &
                   V10 >= V13 &
                   V10 >= V14 &
                   V10 >= V15 &
                   V10 >= V16,]
##only unique time series
dup <- which(!duplicated(tagged,by=c("V1","V2")))
tagged <- tagged[dup,]
set.seed(0)
data <- rbind(tagged,sample_n(untagged,nrow(tagged)))

##Check ccdf
t <- data[,length(id), by=c("sum","tagged")]
t[,norm := V1/max(V1), by=tagged]
ggplot(t, aes(sum,norm,color=tagged)) + geom_point() + scale_x_log10() + scale_y_log10()
ggplot(t, aes(sum,V1,color=tagged)) + geom_point() + scale_x_log10() + scale_y_log10()

###############################
#########HYPOTHESIS 1##########
###############################

##melt the data by month
data_melt <- melt(data,id=c("id","tagged"),measure=all_cols)

##plot the distributions. 
#There is more listening after the tag for tagged users, and more before for non-tagged
p_eda <- ggplot(data_melt, aes(value,fill=tagged)) 
p_eda <- p_eda + geom_histogram(position='dodge') 
p_eda <- p_eda + facet_wrap(~variable,scales="free_y") + scale_x_log10()

##the distributions are pretty skewed, so lets double check using logs....same thing. 
normal_ci <- data_melt[,list(mean=mean(value),
                             median=median(value),
                             se=sd(value)/sqrt(length(id))),
                       by=c("tagged","variable")]
setnames(normal_ci,"variable","month")
normal_ci[,mean_norm:=mean/max(mean),by=tagged]
normal_ci[,se_norm:=se/max(mean),by=tagged]
normal_ci[,median_norm:=median/max(median),by=tagged]

p1 <- ggplot(normal_ci, aes(x=month,y=mean_norm,
                            linetype=tagged, group=tagged))
p1 <- p1 + geom_line(size=.8)
p1 <- p1 + geom_pointrange(aes(x=month,
                               ymin=mean_norm-1.96*se_norm,
                               ymax=mean_norm+1.96*se_norm)) 
p1 <- p1 + scale_x_discrete(labels=plot_labels) + scale_linetype_discrete("Tagged",labels=c("No","Yes")) + xlab("Months Before/After Peak (at 0)") + ylab("Mean Listens (normalized)")
ggsave("paper/taggedVUntaggedSimple.png",p1, dpi=400,w=10,h=6)
############################
#####REGRESSION ANALYSIS####
############################

### okay. we should  run the regression model to make sure that even if we control for previous listening behavior, there is still an increase in listening with tagging.
reg_data <- data[log(V10) < 7.5]
reg_data$tagged <- factor(reg_data$tagged)


p2 <- ggplot() + stat_smooth(data=reg_data, aes(V10, after_sum, linetype=tagged),method='gam',formula=y~s(x)) + xlab("Listens in peak month") + ylab("Listens in\nfollowing 6 months") + scale_linetype_discrete("Tagged",labels=c("No","Yes")) + scale_x_log10() + scale_y_log10()
ggsave("paper/taggedVUntaggedRegression.png",p2, dpi=400,w=10,h=6)

res_gam <- gam(log(after_sum)~s(log(V10))+s(log(before_sum))+tagged,data=reg_data)
res_gam2 <- gam(log(after_sum)~s(log(V10))+tagged,data=reg_data)
res_gam3 <- gam(log(after_sum)~s(log(V10))+s(log(V9+.1))+s(log(V8+.1))+s(log(V7+.1))+s(log(V6+.1))+s(log(V5+.1))+s(log(V4+.1))+tagged,data=reg_data)

res_lm <- lm(log(after_sum)~poly(log(V10),2)+tagged,data=reg_data)

AIC(res_gam)
AIC(res_gam2)
AIC(res_gam3)
AIC(res_lm)

##resid vs fit looks reasonable
ggplot(data.frame(y=res_gam3$fitted.values,t=reg_data$tagged,x=log(reg_data$after_sum)), aes(y,y-x,color=t)) + geom_point()

vis.gam(res_gam3)

###############################
#########HYPOTHESIS 2##########
###############################

tagged <- get_data("data/tagged.csv",
                   before_cols,after_cols,all_cols)

tagged_used <- tagged[V10 >= V4 &
                        V10 >= V5 &
                        V10 >= V6 &
                        V10 >= V7 &
                        V10 >= V8 &
                        V10 >= V9 &
                        V10 >= V11 &
                        V10 >= V12 &
                        V10 >= V13 &
                        V10 >= V14 &
                        V10 >= V15 &
                        V10 >= V16,]
dup <- which(!duplicated(tagged_used,by=c("V1","V2")))
tagged_used <- tagged_used[dup,]

tagged_not_used <- tagged[!(V10 >= V4 &
                              V10 >= V5 &
                              V10 >= V6 &
                              V10 >= V7 &
                              V10 >= V8 &
                              V10 >= V9 &
                              V10 >= V11 &
                              V10 >= V12 &
                              V10 >= V13 &
                              V10 >= V14 &
                              V10 >= V15 &
                              V10 >= V16),]
dup <- which(!duplicated(tagged_not_used,by=c("V1","V2")))
tagged_not_used <- tagged_not_used[dup,]

tags_for_reg <- rbind(tagged_used,tagged_not_used)

##controlling for previous listening behavior, which tags ultimately best predict future listening behavior
setnames(tags_for_reg, c("V1","V2","V3"), c("user","artist","tag"))
tag_counts <- tags_for_reg[,length(user),by=c("tag")]

tag_names <- fread("data/tag_names.tsv")
setnames(tag_names,c("tag_id","name","ent1","ent2"))

tagged_pred <- tags_for_reg[tag %in% tag_counts[V1 > 5,]$tag,]
tagged_pred$tag_factor <- factor(tagged_pred$tag)

res_tag <- bam(log(after_sum)~s(log(V10+.01),bs="cr")+tag_factor,data=tagged_pred)

library(stringr)
sum_res_tag <- summary(res_tag)

df <- data.frame(sum_res_tag$p.table[which(sum_res_tag$p.table[,4] < .001), ])

pos_dat <- df[df$Estimate > 0,]
pos_dat$tag_id <- as.integer(sub("tag_factor","",row.names(pos_dat)))
pos_dat <- merge(pos_dat,tag_names,by="tag_id")
arrange(pos_dat,desc(Estimate))[,c("name","Estimate")]

neg_dat <- df[df$Estimate < 0,]
neg_dat$tag_id <- as.integer(sub("tag_factor","",row.names(neg_dat)))
neg_dat <- merge(neg_dat,tag_names,by="tag_id")
arrange(neg_dat,desc(Estimate))[,c("name","Estimate")]

all_sig_coeff <- rbind(pos_dat,neg_dat)
setnames(tag_counts,c("tag_id","count"))
all_sig_coeff <- merge(all_sig_coeff,tag_counts,by="tag_id")
ggplot(all_sig_coeff, aes(Estimate,count)) + geom_point() + scale_y_log10()

global_freq <- fread("global_tag_freqs.tsv")

global_freq <- merge(global_freq,all_sig_coeff,by="tag_id",all.x=T)
global_freq <- global_freq[tag_id %in% tag_counts[count > 5,]$tag_id]
global_freq$type <- "Not sig."
global_freq[Estimate > .5 ]$type <- "Sig., x>.5"
global_freq[Estimate < -.5 ]$type <- "Sig., x<-.5"
global_freq[Estimate > -.5 & Estimate < 0 ]$type <- "Sig.,-.5<x<0"
global_freq[Estimate > 0 & Estimate < .5 ]$type <- "Sig.,0<x<.5"


global_freq$type <- factor(global_freq$type, levels=rev(c("Not sig.",
                                                          "Sig., x<-.5",
                                                          "Sig.,-.5<x<0",
                                                          "Sig.,0<x<.5",
                                                          "Sig., x>.5")), order=T)

boot_res <- global_freq[,as.list(smean.cl.boot(freq)),by=type]

p3 <-ggplot(global_freq,aes(Estimate,freq)) + geom_point() +  scale_y_log10() + ylab("Global Popularity") + xlab("Regression Coefficient") + geom_hline(y=11064.2, color='red') + geom_hline(y=15641, color='red')
ggsave("paper/tagRegressionWithMoreData.png", dpi=400,w=10,h=6)





user_freq <- fread("userCountsByTag.tsv")
setnames(user_freq, c("tag_id","tag_name","freq"))
user_freq <- merge(user_freq,all_sig_coeff,by="tag_id",all.x=T)
user_freq <- user_freq[tag_id %in% tag_counts[count > 5,]$tag_id]
user_freq$type <- "Not sig."
user_freq[Estimate > .5 ]$type <- "Sig., x>.5"
user_freq[Estimate < -.5 ]$type <- "Sig., x<-.5"
user_freq[Estimate > -.5 & Estimate < 0 ]$type <- "Sig.,-.5<x<0"
user_freq[Estimate > 0 & Estimate < .5 ]$type <- "Sig.,0<x<.5"


user_freq$type <- factor(user_freq$type, levels=rev(c("Not sig.",
                                                          "Sig., x<-.5",
                                                          "Sig.,-.5<x<0",
                                                          "Sig.,0<x<.5",
                                                          "Sig., x>.5")), order=T)

boot_res <- user_freq[,as.list(smean.cl.boot(freq)),by=type]

p4 <-ggplot(user_freq,aes(Estimate,freq)) + geom_point() +  scale_y_log10() + ylab("Number of unique users") + xlab("Regression Coefficient") + geom_hline(y=671.2, color='red') + geom_hline(y=835.47, color='red')
ggsave("paper/tagRegressionWithMoreDataPeople.png", dpi=400,w=10,h=6)


