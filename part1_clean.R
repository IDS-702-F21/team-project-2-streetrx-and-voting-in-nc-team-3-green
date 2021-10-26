
library(ggplot2)
library(dplyr)
library(lme4)
library(stargazer)
library(lattice)
library(caret)
library(influence.ME) 
library(sjPlot)
library(texreg)
library(rrtable)

####################################################################3
####################################################################
################# Part I #####################################
load("./Data/streetrx.RData")
head(streetrx)
colnames(streetrx)
streetrx_morphine <- streetrx[streetrx['api_temp']=="morphine",]
nrow(streetrx_morphine)
streetrx_morphine<-na.omit(streetrx_morphine)
streetrx_morphine <- streetrx_morphine[,! colnames(streetrx_morphine) %in% c("yq_pdate","price_date","city","country","Primary_Reason")] #drop columns
#rename source = "" to unknown
streetrx_morphine$source <- ifelse(streetrx_morphine$source=="", "Unknown",as.character(streetrx_morphine$source))
#rename state = USA to unknown
streetrx_morphine$state <- ifelse(streetrx_morphine$state=="USA","Unknown",as.character(streetrx_morphine$state))
streetrx_morphine<-streetrx_morphine %>%filter(streetrx_morphine$state!="")

nrow(streetrx_morphine)
head(streetrx_morphine)


streetrx_morphine$logppm<-log(streetrx_morphine$ppm) # add a logppm column
streetrx_morphine$mgstr_c <- streetrx_morphine$mgstr-mean(streetrx_morphine$mgstr) #center mgstr
#factorize categorical variable: source, form_temp and bulk_purchase
streetrx_morphine$bulk_fac <- factor(streetrx_morphine$bulk_purchase,levels=c("0 Not bulk purchase","1 Bulk purchase"),labels=c("No","Yes"))
streetrx_morphine$source_rename <- ifelse(streetrx_morphine$source%in%c("Personal","Heard it","Internet","Internet Pharmacy","Unknown"),as.character(streetrx_morphine$source),"Internet")
streetrx_morphine$source_fac <- factor(streetrx_morphine$source_rename)
streetrx_morphine$form_fac <- factor(streetrx_morphine$form_temp)
levels(streetrx_morphine$form_fac)

################################################
###explore the response variable, price per milogram
summary(streetrx_morphine$ppm)
ggplot(data=streetrx_morphine,aes(x=ppm))+geom_histogram(bins=50)
qqnorm(streetrx_morphine$ppm)
# super skewed, it is not even outliers is skewing it
# try log
streetrx_morphine <- streetrx_morphine[streetrx_morphine$ppm!=0,] #0 dollar must be a not trustworthy data
ggplot(data=streetrx_morphine,aes(x=logppm))+geom_histogram(bins=50)
qqnorm(streetrx_morphine$logppm)
###log: improve normality

#######################################################
#EDA

###
#by region?
streetrx_morphine %>% count(USA_region)

ggplot(streetrx_morphine,
       aes(x=USA_region, y=logppm,, fill=USA_region)) +
  geom_boxplot() +
  labs(title="Log ppm by USA_region",
       x="USA region",y="log(ppm)") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

###
#by state-level hierarchical?
streetrx_morphine %>% count(state)
median(table(streetrx_morphine['state']))
# some groups/state does not have a lot of data
#sample a few, 25
set.seed(1000)
sample_state <- sample(unique(streetrx_morphine$state),25,replace=F)
ggplot(streetrx_morphine[is.element(streetrx_morphine$state,sample_state),],
       aes(x=state, y=logppm,, fill=state)) +
  geom_boxplot() +
  labs(title="Log ppm by state",
       x="state",y="log(ppm)") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))
#yes??, but some only have <50
#look at state with >100 entries, since median count = 108 
sample_state100 <- which(table(streetrx_morphine$state)> 100)
ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state100)),],
       aes(x=state, y=logppm,, fill=state)) +
  geom_boxplot() +
  labs(title="Log ppm by state with >100 entries",
       x="state",y="log(ppm)") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))
# the variation across states is much more than the variation across regions

###categorical variables
#1) source
ggplot(streetrx_morphine,aes(x=source_fac, y=logppm, fill=source_fac)) +
  geom_boxplot() + scale_fill_brewer(palette="Set1") +
  labs(title="Log(ppm) vs Source", x="source",y="Log(ppm)") +
  theme_classic() + theme(legend.position="none")
#can't say it is different

# state and source
sample_state <- sample(unique(streetrx_morphine$state),8,replace=F)
ggplot(streetrx_morphine[is.element(streetrx_morphine$state,sample_state),],
       aes(x=source_fac, y=logppm, fill=source_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs source by state",
       x="source",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=4)
#lets only do it for state with count>100: sample_state100 total29
ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state100)[1:10]),],
       aes(x=source_fac, y=logppm, fill=source_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs source by state with >100 first 10/29",
       x="source",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=5)

ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state)[11:20]),],
       aes(x=source_fac, y=logppm, fill=source_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs source by state with >100 11-20/29",
       x="source",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=5)


ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state)[21:29]),],
       aes(x=source_fac, y=logppm, fill=source_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs source by state with >100 21-29/29",
       x="source",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=5)
# slope on source_fac varies with state? yes? we can try comparing AIC values with and without

#2) bulk
streetrx_morphine %>% count(bulk_fac)

#just by bulk
ggplot(streetrx_morphine,aes(x=bulk_fac, y=logppm, fill=bulk_fac)) +
  geom_boxplot() + scale_fill_brewer(palette="Greens") +
  labs(title="Log(ppm) vs Bulk purchase", x="bulk purchase?",y="Log(ppm)") +
  theme_classic() + theme(legend.position="none")
## hard to see the difference on log sacle, how bout the originla scale?
ggplot(streetrx_morphine,aes(x=bulk_fac, y=ppm, fill=bulk_fac)) +
  geom_boxplot() + scale_fill_brewer(palette="Greens") +
  labs(title="ppm vs Bulk purchase", x="bulk purchase?",y="ppm") +
  theme_classic() + theme(legend.position="none")
#even worse

## bulk effect by state
ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state100))[1:10],],
       aes(x=bulk_fac, y=logppm, fill=bulk_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs bulk purchase? by state with >100 1-10/29",
       x="bulk purchase?",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=5)

ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state100)[11:20]),],
       aes(x=bulk_fac, y=logppm, fill=bulk_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs bulk purchase? by state with >100 11-20/29",
       x="bulk purchase?",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=5)

ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state100)[21:29]),],
       aes(x=bulk_fac, y=logppm, fill=bulk_fac)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs bulk purchase? by state with >100, 21-29/29",
       x="bulk purchase?",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=5)
#seems like slope on bulk_fac do not vary much with state?

# 3)form fac
streetrx_morphine %>% count(form_fac)
#since only 5 are in suppository, we should not look at this effect

### numerical variable:
# mgstr_c/mgstr
ggplot(streetrx_morphine,aes(x=mgstr,y=logppm))+geom_point()+geom_smooth(method="lm",col="red3")+labs(title="mgstr VS log(ppm)")
# mgstr should be included in the final model

ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state)),],
       aes(x=mgstr, y=logppm, fill=form_fac)) +
  geom_point() +geom_smooth(method="lm",col="red3")+
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs mgstr by state with >50 first 10/28",
       x="source",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=5)

#random slope on mgstr across states?
ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state100[1:10])),],
       aes(x=mgstr,y=logppm, fill=form_fac))+
  geom_point() +geom_smooth(method="lm",col="red3")+
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs mgstr by state with >100 1-10/29",
       x="source",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=5)
ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state100[11:20])),],
       aes(x=mgstr,y=logppm, fill=form_fac))+
  geom_point() +geom_smooth(method="lm",col="red3")+
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs mgstr by state with >100 11-20/29",
       x="source",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=5)
ggplot(streetrx_morphine[is.element(streetrx_morphine$state,names(sample_state100[21:29])),],
       aes(x=mgstr,y=logppm, fill=form_fac))+
  geom_point() +geom_smooth(method="lm",col="red3")+
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs mgstr by state with >100 21-29/29",
       x="source",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=5)

###
#interaction between variables other than states and region
ggplot(streetrx_morphine,
       aes(x=mgstr, y=logppm, fill=bulk_fac)) +
  geom_point() +geom_smooth(method="lm",col="red3")+
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs mgstr by bulk_fac",
       x="source",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ bulk_fac,ncol=2)

ggplot(streetrx_morphine,
       aes(x=mgstr, y=logppm, fill=source_fac)) +
  geom_point() +geom_smooth(method="lm",col="red3")+
  scale_fill_brewer(palette="Set1") +
  labs(title="log(ppm) vs mgstr by source_fac",
       x="source",y="log(ppm") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ source_fac,ncol=5)

ggplot(streetrx_morphine,aes(x=bulk_fac, y=logppm, fill=form_fac)) +
  geom_boxplot() + scale_fill_brewer(palette="Greens") +
  labs(title="Log(ppm) vs bulk by source_fac", x="form",y="Log(ppm)") +
  theme_classic() + theme(legend.position="none")+
  facet_wrap(~source_fac)

############################################3
########Modeling

###
##baseline
baseline <- lm(logppm~mgstr_c+source_fac+bulk_fac,data=streetrx_morphine)
summary(baseline)
baseline$coefficients
plot(baseline)
plot(baseline,which=4)
plot(streetrx_morphine$mgstr_c,baseline$residuals)
# checking linearity? The distribution of residuals does not appear to have a trend. Linearity: checked 

###
#AIC select variables
nullModel<- lm(logppm~1,data= streetrx_morphine)
fullModel <- lm(logppm~mgstr_c+source_fac+form_fac+bulk_fac+mgstr_c:source_fac+mgstr_c:bulk_fac+source_fac:bulk_fac,data=streetrx_morphine)
stepwise <- step(nullModel,scope = formula(fullModel),direction="both",trace=0)#,k=log(nrow(streetrx_morphine)))
stepwise$call
# our EDA suggests bulk_fac might not be a significant factor, let's to a Anova F test to confirm if bulk_fac is really needed
model1 <- lm(formula = logppm ~ mgstr_c + bulk_fac + source_fac, data = streetrx_morphine)
model2 <- lm(formula = logppm ~ mgstr_c +  source_fac, data = streetrx_morphine)
anova(model1,model2)#anova F test: yes
# check if source_fac is really significant:yes
model3 <- lm(formula = logppm ~ mgstr_c +  bulk_fac, data = streetrx_morphine)
anova(model1,model3)

resall <- data.frame(fitted = predict(model3),res = residuals(model3),mgstr_c = streetrx_morphine$mgstr_c)
ggplot(resall, aes(x=fitted,y=res))+geom_point(alpha=I(0.1))+geom_smooth(method="lm",col="red3")+labs(title="Fitted VS Residual")


summary(model1)
plot(model1)
plot(model1,which=4)
#check no need to check VIC since only one is numerical
#interesting cluster: fitted values<-2.0
streetrx_morphine$logppm_pred <- predict(model1)
a<-streetrx_morphine%>%filter(logppm_pred< -2.0)
b<-streetrx_morphine%>%filter(logppm_pred>= -2.0)
hist(a$mgstr_c)
hist(b$mgstr_c)

####
#hierarchical model
# 1) try hierarchical with state
hier_model1 <- lmer(logppm~mgstr_c+bulk_fac+source_fac+(1|state),data=streetrx_morphine)
summary(hier_model1)

# 2) try it with adding region, two level
hier_model2 <- lmer(logppm~mgstr_c+bulk_fac+source_fac+(1|state)+
                      (1|USA_region),data=streetrx_morphine)
summary(hier_model2)

hier_model21 <- lmer(logppm~mgstr_c+bulk_fac+source_fac+
                      (1|USA_region),data=streetrx_morphine)
summary(hier_model21)
AIC(hier_model1)
AIC(hier_model2) #not better, random interepct with state only
AIC(hier_model21)
# 3) random slope on mgstr_c
hier_model3 <- lmer(logppm~mgstr_c+bulk_fac+source_fac+(mgstr_c|state),data=streetrx_morphine)
summary(hier_model3)
anova(hier_model1,hier_model3) 
AIC(hier_model1)
AIC(hier_model3)
# AIC much smaller, p<0.05 yes!! keep varying slope on mgstr_c
# across states, the slope on mgstr_c is not significantly different (std = 0.088)
# what exactly explains residual std of 0.90? variation across states is too small compared to the variations among each observation. std =0.90.

# 4) add bulk?
hier_model4 <- lmer(logppm~mgstr_c+bulk_fac+source_fac+(mgstr_c+bulk_fac|state),data=streetrx_morphine)
summary(hier_model4)
AIC(hier_model4)
anova(hier_model3,hier_model4) # won't add bulk

# 5) add random slope for source?
hier_model5 <- lmer(logppm~mgstr_c+bulk_fac+source_fac+(mgstr_c+source_fac|state),data=streetrx_morphine)
summary(hier_model5)
AIC(hier_model5)
anova(hier_model3,hier_model5)  # no

########
#final model
finalmodel <- hier_model3
resall <- data.frame(fitted = predict(finalmodel),res = residuals(finalmodel),mgstr_c = streetrx_morphine$mgstr_c)
ggplot(resall, aes(x=fitted,y=res))+geom_point(alpha=I(0.1))+geom_smooth(method="lm",col="red3")+labs(title="Fitted VS Residual of state X")

plot(finalmodel)
plot(streetrx_morphine$mgstr_c,residuals(finalmodel))

mi <- seq(1,nrow(streetrx_morphine),1)[streetrx_morphine$state=="Michigan"]
res <- data.frame(fitted = predict(finalmodel),res = residuals(finalmodel),mgstr_c = streetrx_morphine$mgstr_c)[mi,]
ggplot(res, aes(x=fitted,y=res))+geom_point()+geom_smooth(method="lm",col="red3")+labs(title="Fitted VS Residual of state X")

stargazer(hier_model3,report="v*c*s*p",ci=T)#,type="text")
model_html <- tab_model(finalmodel,file = "temp.html")
summary(finalmodel)
confint(finalmodel)##???
dotplot(ranef(finalmodel,condVar=TRUE))$state
# check outliers by cook's distance
# infl <- influence(finalmodel, obs = TRUE)
# plot(infl, which = "cook",xlab="Cook's distance")

# interpretation
fixef(finalmodel)
1-exp(fixef(finalmodel))
rand_effects <- ranef(finalmodel,condVar=TRUE)$state
rand_effects[c("Massachusetts","Michigan","North Carolina"),]# look at a few examples
basline <-exp(fixef(finalmodel)[1]+rand_effects[c("Massachusetts","Michigan","North Carolina"),"(Intercept)"])
mgstr_effects <- exp(fixef(finalmodel)[2]+rand_effects[c("Massachusetts","Michigan","North Carolina"),"mgstr_c"])
data.frame(state =c("Massachusetts","Michigan","North Carolina"), basline=basline,`mgstr_c(multiplicative effect)`=mgstr_effects)

# prediction
predictdataNC <- data.frame(mgstr_c=round(seq(min(streetrx_morphine$mgstr),max(streetrx_morphine$mgstr),0.1),2))
predictdataNC$source_fac <- "Heard it"
predictdataNC$bulk_fac <- "No"
predictdataNC$state <- "North Carolina"
predictdataNC$predict_logppm <-predict(finalmodel,predictdataNC)

predictdataMA <- data.frame(mgstr_c=round(seq(min(streetrx_morphine$mgstr),max(streetrx_morphine$mgstr),0.1),2))
predictdataMA$source_fac <- "Heard it"
predictdataMA$bulk_fac <- "No"
predictdataMA$state <- "Massachusetts"
predictdataMA$predict_logppm <-predict(finalmodel,predictdataMA)
  
predictdataMI <- data.frame(mgstr_c=round(seq(min(streetrx_morphine$mgstr),max(streetrx_morphine$mgstr),0.1),2))
predictdataMI$source_fac <- "Heard it"
predictdataMI$bulk_fac <- "Yes"
predictdataMI$state <- "Michigan"
predictdataMI$predict_logppm <-predict(finalmodel,predictdataMI)

predictdataSC <- data.frame(mgstr_c=round(seq(min(streetrx_morphine$mgstr),max(streetrx_morphine$mgstr),0.1),2))
predictdataSC$source_fac <- "Heard it"
predictdataSC$bulk_fac <- "No"
predictdataSC$state <- "South Carolina"
predictdataSC$predict_logppm <-predict(finalmodel,predictdataSC)

predictdataOK <- data.frame(mgstr_c=round(seq(min(streetrx_morphine$mgstr),max(streetrx_morphine$mgstr),0.1),2))
predictdataOK$source_fac <- "Heard it"
predictdataOK$bulk_fac <- "No"
predictdataOK$state <- "Oklahoma"
predictdataOK$predict_logppm <-predict(finalmodel,predictdataOK)

predictdataSD <- data.frame(mgstr_c=round(seq(min(streetrx_morphine$mgstr),max(streetrx_morphine$mgstr),0.1),2))
predictdataSD$source_fac <- "Heard it"
predictdataSD$bulk_fac <- "No"
predictdataSD$state <- "South Dakota"
predictdataSD$predict_logppm <-predict(finalmodel,predictdataSD)

predictdataCA <- data.frame(mgstr_c=round(seq(min(streetrx_morphine$mgstr),max(streetrx_morphine$mgstr),0.1),2))
predictdataCA$source_fac <- "Heard it"
predictdataCA$bulk_fac <- "No"
predictdataCA$state <- "California"
predictdataCA$predict_logppm <-predict(finalmodel,predictdataCA)

predictdata <- bind_rows(list(predictdataNC,predictdataMA,predictdataMI,predictdataOK, predictdataSC,predictdataSD,predictdataCA))

ggplot(data=predictdata, aes(x = mgstr_c, y = predict_logppm,group=state,color=state))+geom_line()+labs(title="Predicted log(ppm) by State",
