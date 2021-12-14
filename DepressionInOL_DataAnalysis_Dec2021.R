#Code for analyses for the manuscript "Depression in online learning: aspects of courses that help and hinder"

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(nnet)
library(car)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggrepel)
library(binomTools)
library(generalhoslem)
library(caret)
library(OptimalCutpoints)
library(factoextra)
library(foreign)
library(lavaan)
library(reghelper)
library(tidytext)
library(ggpubr)
library(viridis)
library(readxl)

my.data <- read.csv("DepressioninOL_deIDdata_Dec2021.csv")

my.data$depression2<-as.factor(my.data$depression2)
my.data$GPA<-as.numeric(my.data$GPA)
my.data$race2 <- factor(my.data$race2, ordered = F) 
my.data$race2 <- relevel(my.data$race2, ref = "white")
my.data$gen.stat2 <- factor(my.data$gen.stat2, ordered = F) 
my.data$gen.stat2 <- relevel(my.data$gen.stat2, ref = "non.fgen")
my.data$financially.stable2 <- factor(my.data$financially.stable2, ordered = F) 
my.data$financially.stable2 <- relevel(my.data$financially.stable2, ref = "yes")
my.data$division <- factor(my.data$division, ordered = F) 
my.data$division <- relevel(my.data$division, ref = "upper")
my.data$financially.stable3 <- NA
my.data[my.data$financially.stable2 == "sometimes" & !is.na(my.data$financially.stable2),]$financially.stable3 <- "no"
my.data[my.data$financially.stable2 == "no" & !is.na(my.data$financially.stable2),]$financially.stable3 <- "no"
my.data[my.data$financially.stable2 == "yes" & !is.na(my.data$financially.stable2),]$financially.stable3 <- "yes"
my.data$financially.stable3 <- factor(my.data$financially.stable3, ordered = F) 
my.data$financially.stable3 <- relevel(my.data$financially.stable3, ref = "yes")
my.data$severity.depression.online <- factor(my.data$severity.depression.online, ordered = F) 
my.data$severity.depression.online <- relevel(my.data$severity.depression.online, ref = "little")

####RQ1:To what extent do students in online college science courses have depression?----

mod1<-glm(depression2 ~ gender2+race2+lgbtq2+gen.stat2+financially.stable3+STEM.major.clean+division+GPA, na.action = na.omit, data = my.data, family = binomial)
beta(mod1)
vif(mod1)
residualPlots(mod1, terms = ~1)
influencePlot(mod1)

mod1df <- as.data.frame(summary(mod1)$coefficients[,-3])
mod1df$predictor<-rownames(mod1df)
mod1df <- mod1df %>%
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
mod1df$or<- exp(mod1df$est)

tmp<-as.data.frame(beta(mod1)$coefficients[,1])
tmp$predictor <-rownames(tmp)
colnames(tmp)<- c("stdest", "predictor2")

mod1comb <- cbind(mod1df, tmp)

####RQ2: What is the severity of depression for students in online science courses?----

my.data$severity.depression2 <- NA
my.data[my.data$severity.depression.online == "little",]$severity.depression2 <- "little.mild"
my.data[my.data$severity.depression.online == "mild",]$severity.depression2 <- "little.mild"
my.data[my.data$severity.depression.online == "moderate",]$severity.depression2 <- "Moderate"
my.data[my.data$severity.depression.online == "severe",]$severity.depression2 <- "Severe"
my.data$severity.depression2<-as.factor(my.data$severity.depression2)

mod2<-multinom(severity.depression2 ~ gender2+race2+lgbtq2+gen.stat2+financially.stable3+STEM.major.clean+division+GPA, na.action = na.omit, data = my.data)
z <- summary(mod2)$coefficients/summary(mod2)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2

plot(predict(mod2, type="probs"), residuals(mod2, type="probs"))

mod2df <- as.data.frame(summary(mod2)$coefficients)
moderatedf <- mod2df[1,]
severedf<-mod2df[2,]
moderatedf<-moderatedf %>% pivot_longer(cols = everything())
moderatedf$severity<- "moderate"
severedf<-severedf %>% pivot_longer(cols = everything())
severedf$severity<- "severe"
severedf <- severedf %>%
  dplyr::rename(predictor = name, est = value) %>%
  as.data.frame()
moderatedf <- moderatedf %>%
  dplyr::rename(predictor = name, est = value) %>%
  as.data.frame()

tmp <- rbind(moderatedf, severedf)

mod2df2 <- as.data.frame(summary(mod2)$standard.errors)
moderatedf2 <- mod2df2[1,]
severedf2<-mod2df2[2,]
moderatedf2<-moderatedf2 %>% pivot_longer(cols = everything())
moderatedf2$severity<- "moderate"
severedf2<-severedf2 %>% pivot_longer(cols = everything())
severedf2$severity<- "severe"
severedf2 <- severedf2 %>%
  dplyr::rename(predictor = name, se = value) %>%
  as.data.frame()
moderatedf2 <- moderatedf2 %>%
  dplyr::rename(predictor = name, se = value) %>%
  as.data.frame()

tmp2<- rbind(moderatedf2, severedf2)

moderatedf3 <- as.data.frame(p[1,])
moderatedf3$severity <- "moderate"
moderatedf3$predictor <- row.names(moderatedf3)
colnames(moderatedf3) <- c("pval", "severity", "predictor")

severedf3<- as.data.frame(p[2,])
severedf3$severity<- "severe"
severedf3$predictor <- row.names(severedf3)
colnames(severedf3) <- c("pval", "severity", "predictor")

tmp3 <-rbind(moderatedf3, severedf3)

mod2_output2 <- cbind(tmp, tmp2, tmp3)

mod2_output<-cbind(moderate, severe)

mod2df_combined %>%
  filter(predictor %in% c("anxiety.disorder2yes","divisionlower","financially.stable3no",
                          "gen.stat2fgen","gender2woman",
                          "GPA","lgbtq2yes", 
                          "race2asian", "race2black",
                          "race2latinx", "STEM.major.cleanyes")) %>%
  ggplot(aes(x = est, y = predictor, color = predictor)) +
  geom_point(size = 5) +
  geom_vline(aes(xintercept = 0), linetype="dashed", size=.7, color = "grey25") + 
  geom_errorbarh(aes(xmin = est-(1.96*se), xmax = est+(1.96*se)), 
                 position=ggstance::position_dodgev(height=0.5), alpha = 1, size = 2) +  
  labs(x = "", y = "", title = "", color = "") + 
  theme_classic() +
  theme(legend.position = "right", 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(family="Helvetica", color = "black", size = 14, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(), 
        legend.key.height = unit(1.25, "cm"),
        legend.title = element_text(family="Helvetica", color = "black", size = 16, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 14, face = "bold"),
        strip.text = element_text(family="Helvetica", color = "black", size = 18, face = "bold"),
        strip.background = element_rect(color = "white", size = 1),
        plot.title = element_blank()) +
  scale_color_brewer(palette = "Set3", direction = -1,
                     labels = c("Anxiety","Lower division","Financially unstable",
                                "First-gen", "Woman", "GPA", "LGBTQ+",
                                "Asian", "Black", "Latinx",
                                "STEM major")) +
  xlim(-2, 4) +
  guides(colour = guide_legend(reverse = T, override.aes = list(size=4, shape = 19, linetype = 0))) + 
  facet_wrap(.~severity, ncol = 2,
             labeller = labeller(severity = c("moderate" = "Moderate", 
                                              "severe" = "Severe")))

###RQ3: What aspects of online science courses worsen depression?----
col_worsen<-which(colnames(my.data) %in% c("idep.camera", "idep.comm.instructor","idep.compare","idep.diff.help.instructor","idep.diff.help.students","idep.diff.know.instructor","idep.diff.know.students","idep.home.distract","idep.nav.tech","idep.not.inperson","idep.nothing","idep.other" ,"idep.pace","idep.personal.tech","idep.proctor.exam","idep.questions","idep.talk.unknown"))

worsen.descriptives <- do.call(rbind, lapply(col_worsen, function(x){
  tmp<-as.data.frame(table(my.data[,x]))
  tmp$pct <- (tmp$Freq/sum(tmp$Freq))*100
  tmp$count<-tmp$Freq
  tmp$factor <- colnames(my.data)[x]
  return(tmp)
}))

worsen.table<-worsen.descriptives %>%
  filter(Var1==1)

factors.worsen <- do.call(rbind, lapply(c("idep.camera", "idep.comm.instructor","idep.compare","idep.diff.help.instructor","idep.diff.help.students","idep.diff.know.instructor","idep.diff.know.students","idep.home.distract","idep.nav.tech","idep.not.inperson","idep.nothing","idep.other" ,"idep.pace","idep.personal.tech","idep.proctor.exam","idep.questions","idep.talk.unknown"), function(x){
  tmp.mod <- as.data.frame(summary(glm(formula = as.formula(paste0(x, "~gender2+race2+lgbtq2+gen.stat2+financially.stable3+STEM.major.clean+division+GPA")), data = my.data))$coefficients[,-3])
  tmp.mod$factor <- x
  tmp.mod$predictor <- rownames(tmp.mod)
  tmp.mod$stdest <- beta(glm(formula = as.formula(paste0(x, "~gender2+race2+lgbtq2+gen.stat2+financially.stable3+STEM.major.clean+division+GPA")), data = my.data))$coefficients[,1]
  return(tmp.mod)
})) 
factors.worsen <- factors.worsen %>%
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`, stdest = stdest) %>%
  as.data.frame()
factors.worsen$or<- exp(factors.worsen$est)
factors.worsen$qval <- NA
predictors_output <- c("gender2woman","race2asian","race2black" ,"race2latinx","lgbtq2yes","gen.stat2fgen" ,
                       "financially.stable3no", "STEM.major.cleanyes","divisionlower", "GPA")
for(i in 1:length(predictors_output)){
  factors.worsen[factors.worsen$predictor == predictors_output[i],]$qval<-p.adjust(p = factors.worsen[factors.worsen$predictor == predictors_output[i],]$pval, method = "fdr")
}

###RQ4:What aspects of online science courses help manage depression?----

col_manage<-which(colnames(my.data) %in% c("ddep.anonymous","ddep.answer.qs","ddep.clear.comm","ddep.easy.help.instructor","ddep.easy.help.students","ddep.easy.know.instructor","ddep.easy.know.students","ddep.flex.when","ddep.flex.where","ddep.instr.cares","ddep.not.seen","ddep.nothing","ddep.other"))

manage.descriptives <- do.call(rbind, lapply(col_manage, function(x){
  tmp<-as.data.frame(table(my.data[,x]))
  tmp$pct <- (tmp$Freq/sum(tmp$Freq))*100
  tmp$count<-tmp$Freq
  tmp$factor <- colnames(my.data)[x]
  return(tmp)
}))
manage.table<-manage.descriptives %>%
  filter(Var1==1)

factors.manage <- do.call(rbind, lapply(c("ddep.anonymous","ddep.answer.qs","ddep.clear.comm","ddep.easy.help.instructor","ddep.easy.help.students","ddep.easy.know.instructor","ddep.easy.know.students","ddep.flex.when","ddep.flex.where","ddep.instr.cares","ddep.not.seen","ddep.nothing","ddep.other"), function(x){
  tmp.mod <- as.data.frame(summary(glm(formula = as.formula(paste0(x, "~gender2+race2+lgbtq2+gen.stat2+financially.stable3+STEM.major.clean+division+GPA")), data = my.data))$coefficients[,-3])
  tmp.mod$factor <- x
  tmp.mod$predictor <- rownames(tmp.mod)
  tmp.mod$stdest <- beta(glm(formula = as.formula(paste0(x, "~gender2+race2+lgbtq2+gen.stat2+financially.stable3+STEM.major.clean+division+GPA")), data = my.data))$coefficients[,1]
  return(tmp.mod)
})) 
factors.manage <- factors.manage %>%
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>%
  as.data.frame()
factors.manage$or <- exp(factors.manage$est)
factors.manage$qval <- NA
predictors_output <- c("gender2woman","race2asian","race2black" ,"race2latinx","lgbtq2yes","gen.stat2fgen" ,
                       "financially.stable3no", "STEM.major.cleanyes","divisionlower", "GPA")
for(i in 1:length(predictors_output)){
  factors.manage[factors.manage$predictor == predictors_output[i],]$qval<-p.adjust(p = factors.manage[factors.manage$predictor == predictors_output[i],]$pval, method = "fdr")
}

