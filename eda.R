library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)

setwd("~/git/TagsAndMemory/")
source("functions.R")

plot_labels <- c("6M before peak",
            "5M before peak",
            "4M before peak",
            "3M before peak",
            "2M before peak",
            "1M before peak",
            "Peak",
            "1M after peak",
            "2M after peak",
            "3M after peak",
            "4M after peak",
            "5M after peak",
            "6M after peak")

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
p1 <- p1 + scale_x_discrete(labels=plot_labels) + theme(axis.text.x=element_text(angle=45,hjust=1)) + scale_linetype_discrete("Tagged",labels=c("No","Yes")) + xlab("Month") + ylab("Mean Listens\n(normalized)")

############################
#####REGRESSION ANALYSIS####
############################

### okay. we should  run the regression model to make sure that even if we control for previous listening behavior, there is still an increase in listening with tagging.
reg_data <- data[log(V10) < 7.5]
reg_data$tagged <- factor(reg_data$tagged)

ggplot() + stat_smooth(data=reg_data, aes(log(V10), log(after_sum), linetype=tagged),method='gam',formula=y~s(x)) + xlab("Log of listens in peak month") + ylab("Log of listens in\nfollowing 6 months") + scale_linetype_discrete("Tagged",labels=c("No","Yes"))


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

##controlling for previous listening behavior, which tags ultimately best predict future listening behavior
setnames(tagged, c("V1","V2","V3"), c("user","artist","tag"))
tag_counts <- tagged[,length(user),by=c("tag")]

tag_names <- fread("data/tag_names.tsv")
setnames(tag_names,c("tag_id","name","ent1","ent2"))

tagged_pred <- tagged[tag %in% tag_counts[V1 > 10,]$tag,]
tagged_pred$tag_factor <- factor(tagged_pred$tag)

res_tag <- bam(log(after_sum)~s(log(V10),bs="cr")+tag_factor,data=tagged_pred)

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

global_freq$type <- factor(global_freq$type, levels=)
boot_res <- global_freq[,as.list(smean.cl.boot(freq)),by=type]

ggplot(boot_res, aes(type,Mean,ymin=Lower,ymax=Upper, color=type=="Not sig.")) + geom_pointrange() + scale_y_log10() + coord_flip() + ylab("Frequency of tagging") + xlab("Regression Result") + scale_color_discrete("",labels=c("Significant","Not significant")) 

