library(ggplot2)
library(reshape2)
library(plyr)
library(ggbiplot)
source("~/Dropbox/Work/Grad_school/Research/Utilities/summarySE.R")

###########################
# Priors
###########################

priors <- read.csv("../data/priors/long.csv")
numSubjects <- length(unique(priors$workerID))
priors$stateRating <- factor(priors$stateRating, labels=c("terrible", "bad", "neutral", "good", "amazing"))
priors.states.raw <- with(priors, table(imageID, stateRating)) + 1 #laplace smoothing
priors.states <- as.data.frame(prop.table(priors.states.raw, margin = 1))
colnames(priors.states)[3] <- "prior"
# plot distribution over states
ggplot(priors.states, aes(x=stateRating, y=prior)) +
  geom_bar(stat="identity", fill="gray", color="black") + 
  ylab("probability") +
  facet_wrap(~imageID, ncol=3) +
  theme_bw()

priors.states$imageID.reordered <- factor(priors.states$imageID, levels=c(3, 2, 1, 6, 5, 4, 8, 7, 9),
                                          labels=c("Weather context 1", "Weather context 2", "Weather context 3", 
                                                   "Weather context 4", "Weather context 5", "Weather context 6", 
                                                   "Weather context 7", "Weather context 8", "Weather context 9"))

priors.states$imageCategory <- ifelse(as.numeric(priors.states$imageID) <= 3, "Positive", 
                                      ifelse(as.numeric(priors.states$imageID) >= 7,
                                                                                     "Negative", "Neutral"))
priors.states$imageCategory <- factor(priors.states$imageCategory, levels=c("Positive", "Neutral", "Negative"))
ggplot(priors.states, aes(x=stateRating, y=prior)) +
  geom_point(size=3, aes(color=imageCategory))+
  geom_line(aes(group=imageID, color=imageCategory)) +
  ylab("Proportion of participants") +
  xlab("Weather state") +
  facet_wrap(~imageID.reordered, ncol=3) +
  theme_bw() +
  scale_color_manual(values=c("#fc4e2a", "#99d8c9", "#2b8cbe"), name="Weather type") +
  ylim(c(0, 1))

ggplot(priors.states, aes(x=stateRating, y=prior, color=imageID)) +
  geom_point(size=3) +
  geom_line(aes(group=imageID), color="gray") +
  ylab("Proportion of subjects") +
  xlab("State") +
  #facet_wrap(~imageID, ncol=3) +
  theme_bw()

# affect ratings

comps <- princomp(data=priors, ~ sad + disgusted + angry + neutral + content + happy + excited, cor=FALSE, scores=TRUE)
#comps <- princomp(data=priors, ~ sad + disgusted + angry + content + happy + excited, cor=FALSE, scores=TRUE)
summary(comps)
biplot(comps)

ggbiplot(comps, pc.biplot= TRUE, obs.scale = 1, var.scale = 1, 
         circle = FALSE, labels.size=5, alpha=0.2, varname.size=3.5, varname.adjust=1) +
  theme_bw() +
  scale_y_reverse() + 
  xlab("PC1: Valence (69.1% var.)") +
  ylab("PC2: Arousal (13.9% var.)") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor=element_blank(), panel.grid.major=element_blank()) 
  
loadings(comps)
scores <- summary(comps)$scores
scores.z <- scale(scores)
scores.probit <- pnorm(scores.z)
priors.pca.probit <- cbind(priors, scores.probit)
priors.pca.probit.long <-  melt(data=priors.pca.probit, id.vars=c("workerID", "gender", "age", "income", "language",
                                                                  "order", "imageID", "imageCategory",  "stateRating"),
                                measure.vars=c("Comp.1", "Comp.2"))

priors.pca.probit.long$value.corrected <- ifelse(priors.pca.probit.long$variable=="Comp.2",
                                                 1- priors.pca.probit.long$value,
                                                 priors.pca.probit.long$value)

priors.pca.probit.summary <- summarySE(priors.pca.probit.long, measurevar="value.corrected",
                                       groupvars=c("variable", "stateRating"))

ggplot(priors.pca.probit.summary, aes(x=stateRating, y=value.corrected, color=variable)) +
  geom_point() +
  geom_line(aes(group=variable)) +
  #geom_point(size=2) +
  #geom_line(aes(group=variable)) +
  geom_errorbar(aes(ymin=value.corrected-ci, ymax=value.corrected+ci), width=0.05) +
  #facet_wrap(~ stateRating, ncol=3) +
  theme_bw() +
  xlab("Weather state rating") +
  ylab("Probability") +
  scale_color_discrete(name="Affect dimension", labels=c("Positive valence", "High arousal"))
  
priors.pca.probit.summary <- summarySE(priors.pca.probit.long, measurevar="value.corrected",
                                       groupvars=c("variable", "stateRating"))

ggplot(priors.pca.probit.summary, aes(x=stateRating, y=value.corrected, color=variable)) +
  geom_point() +
  geom_line(aes(group=variable)) +
  #geom_point(size=2) +
  #geom_line(aes(group=variable)) +
  geom_errorbar(aes(ymin=value.corrected-ci, ymax=value.corrected+ci), width=0.05) +
  #facet_wrap(~ stateRating, ncol=3) +
  theme_bw() +
  xlab("Weather state rating") +
  ylab("Probability") +
  scale_color_discrete(name="Affect dimension", labels=c("Positive valence", "High arousal"))


####################################
# Interpretation data
####################################
interp <- read.csv("../data/interpretation/long_askIrony.csv")
numSubjects <- length(unique(interp$workerID))

interp$stateRating <- factor(interp$stateRating, labels=c("terrible", "bad", "neutral", "good", "amazing"))
interp$imageCategory <- factor(interp$imageCategory, levels=c("terrible", "bad", "ok", "good", "amazing"),
                               labels=c("terrible", "bad", "neutral", "good", "amazing"))
interp$utterance <- factor(interp$utterance, levels=c("terrible", "bad", "ok", "good", "amazing"),
                           labels=c("terrible", "bad", "neutral", "good", "amazing"))
interp$imageID.utterance <- paste(interp$imageID, interp$utterance, sep=",")

# irony

irony.summary <- summarySE(interp, measurevar="ironyRating", groupvars=c("imageID", "imageCategory", "utterance"))
irony.summary$imageValence <- ifelse(irony.summary$imageID == 4, "neutral",
                                     ifelse(irony.summary$imageID == 7 | irony.summary$imageID == 8 | irony.summary$imageID == 9,
                                            "neg", "pos"))
irony.summary$utteranceValence <- ifelse(irony.summary$utterance == "neutral", "neutral",
                                         ifelse(irony.summary$utterance == "amazing" | irony.summary$utterance == "good", "pos",
                                                "neg"))
irony.summary$consistent <- ifelse(irony.summary$imageValence == irony.summary$utteranceValence, 1, 0)

summary(lm(data=irony.summary, ironyRating ~ consistent))

ggplot(irony.summary, aes(x=utterance, y=ironyRating)) +
  geom_bar(stat="identity", color="black", fill="grey", position=position_dodge()) +
  geom_errorbar(aes(ymin=ironyRating-se, ymax=ironyRating+se), position=position_dodge(0.9), width=0.2) +
  facet_wrap(~imageID, ncol=3) +
  theme_bw() +
  ylab("Irony rating")


# state
interp.states.raw <- with(interp, table(imageID.utterance, stateRating)) + 1
interp.states <- as.data.frame(prop.table(interp.states.raw, margin = 1))
interp.states <- cbind(interp.states,ldply(strsplit(as.character(interp.states$imageID.utterance),",")))
colnames(interp.states)[2] <- "state"
colnames(interp.states)[4] <- "imageID"
colnames(interp.states)[5] <- "utterance"
interp.states$utterance <- factor(interp.states$utterance, levels=c("terrible", "bad", "neutral", "good", "amazing"))
interp.states$state <- factor(interp.states$state, levels=c("terrible", "bad", "neutral", "good", "amazing"))

#quartz()

ggplot(interp.states, aes(x=state, y=Freq)) +
  geom_bar(stat="identity", fill="gray", color="black") + 
  ylab("probability") +
  facet_grid(imageID~utterance) +
  theme_bw()

# Compare irony and state distribution
colnames(interp.states)[3] <- "prob"
interp.states.wide <- reshape(data=interp.states, direction="wide",
                              v.names="prob", idvar=c("imageID", "utterance"), timevar="state")
literal.states <- interp.states
colnames(literal.states)[3] <- "prior"
epsilon <- 0.001
literal.states$prior <- ifelse(literal.states$state == literal.states$utterance, 1-4*epsilon, epsilon)
literal.states.wide <- reshape(data=literal.states, direction="wide",
                               v.names="prior", idvar=c("imageID", "utterance"), timevar="state")


kl <- join(interp.states.wide, literal.states.wide, by=c("imageID", "utterance"))
kl$KL <- 
  kl$prob.terrible * log(kl$prob.terrible / kl$prior.terrible) +
  kl$prob.bad * log(kl$prob.bad / kl$prior.bad) +
  kl$prob.neutral * log(kl$prob.neutral / kl$prior.neutral) +
  kl$prob.good * log(kl$prob.good / kl$prior.good) +
  kl$prob.amazing * log(kl$prob.amazing / kl$prior.amazing) 

kl <- join(kl, irony.summary, by=c("imageID", "utterance"))
with(kl, cor.test(KL, ironyRating))

summary(lm(data=kl, ironyRating ~ KL))
summary(lm(data=kl, ironyRating ~ consistent))
summary(lm(data=kl, ironyRating ~ KL + consistent))
lm.valence.kl <- lm(data=kl, ironyRating ~ utteranceValence * imageValence + KL)
lm.valence <- lm(data=kl, ironyRating ~ utteranceValence * imageValence)

anova(lm.valence, lm.valence.kl)

ggplot(kl, aes(x=KL, y=ironyRating, color=utterance)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~imageID, ncol=3)

# affect

affect.posterior <- predict(comps, interp)
#### z score and pnorm
affect.posterior.probit <- pnorm(scale(affect.posterior))
affect.pca <- cbind(interp, affect.posterior.probit)
affect.pca.long <-  melt(data=affect.pca, id.vars=c("workerID", "gender", "age", "income", "language",
                                                    "order", "imageID", "imageCategory",  "utterance", 
                                                    "stateRating"),
                         measure.vars=c("Comp.1", "Comp.2"))

affect.pca.long$value.corrected <- ifelse(affect.pca.long$variable == "Comp.1", affect.pca.long$value, 
                                          1 - affect.pca.long$value)

###############
# Split half
############### 
splithalf.cors <- data.frame(valence=NULL, arousal=NULL)
t = 1
while (t <= 1000) {
  ii <- seq_len(nrow(affect.pca))
  ind1 <- sample(ii, nrow(affect.pca) / 2) 
  ind2 <- ii[!ii %in% ind1] 
  h1 <- affect.pca[ind1, ] 
  h2 <- affect.pca[ind2, ]
  h1.valence.summary <- summarySE(h1, measurevar="Comp.1", groupvars=c("imageID", "utterance"))
  h1.arousal.summary <- summarySE(h1, measurevar="Comp.2", groupvars=c("imageID", "utterance"))
  h2.valence.summary <- summarySE(h2, measurevar="Comp.1", groupvars=c("imageID", "utterance"))
  h2.arousal.summary <- summarySE(h2, measurevar="Comp.2", groupvars=c("imageID", "utterance"))
  colnames(h1.valence.summary)[4] <- "valence1"
  colnames(h2.valence.summary)[4] <- "valence2"
  colnames(h1.arousal.summary)[4] <- "arousal1"
  colnames(h2.arousal.summary)[4] <- "arousal2"
  if (nrow(h1.valence.summary) == nrow(h2.valence.summary) & nrow(h1.arousal.summary) == nrow(h2.arousal.summary)) {
    splithalf.valence <- join(h1.valence.summary, h2.valence.summary, by=c("imageID", "utterance"))
    splithalf.arousal <- join(h1.arousal.summary, h2.arousal.summary, by=c("imageID", "utterance"))
    t <- t+1
    valence.cor <- with(splithalf.valence, cor(valence1, valence2))
    arousal.cor <- with(splithalf.arousal, cor(arousal1, arousal2))
    this.frame <- data.frame(valence=valence.cor,
                             arousal=arousal.cor)
    splithalf.cors <- rbind(splithalf.cors, this.frame)
  }
}

prophet <- function(reliability, length) {
  prophecy <- length * reliability / (1 + (length - 1)*reliability)
  return (prophecy)
}

splithalf.cors <- splithalf.cors[complete.cases(splithalf.cors),]

splithalf.cors$valence.proph <- prophet(splithalf.cors$valence, 2)
splithalf.cors$arousal.proph <- prophet(splithalf.cors$arousal, 2)

valence.proph.summary  <- summarySE(data=splithalf.cors, measurevar="valence.proph", groupvars=NULL)
arousal.proph.summary  <- summarySE(data=splithalf.cors, measurevar="arousal.proph", groupvars=NULL)

######################
# affect
#######################


affect.pca.summary <- summarySE(affect.pca.long, measurevar="value.corrected",
                                groupvars=c("imageID", "utterance", "variable"))

ggplot(affect.pca.summary, aes(x=utterance, y=value.corrected, color=variable)) +
  geom_point() +
  geom_line(aes(group=variable)) +
  geom_errorbar(aes(ymin=value.corrected-se, ymax=value.corrected+se), width=0.05) +
  facet_wrap(~imageID, ncol=3) +
  theme_bw()


####################################
# Model
####################################
interp.valence <- subset(affect.pca.summary, variable=="Comp.1")
interp.arousal <- subset(affect.pca.summary, variable=="Comp.2")

filenames <- read.csv("../model/parsedOutputsWithParams_smoothed/filenames.txt", header=FALSE)
models <- data.frame(name=NULL, cor.state=NULL, cor.valence=NULL, cor.arousal=NULL, cor=NULL)
for (f in filenames$V1) {
  model <- read.csv(paste("../model/parsedOutputsWithParams_smoothed/", f, sep=""))
  model$utterance <- factor(model$utterance, levels=c("terrible", "bad", "ok", "good", "amazing"),
                            labels=c("terrible", "bad", "neutral", "good", "amazing"))
  
  # by state
  model$state <- factor(model$state, levels=c("terrible", "bad", "neutral", "good", "amazing"))
  model.state <- aggregate(data=model, probability ~ imageID + utterance + state, FUN=sum)
  comp.state <- join(interp.states, model.state, by=c("imageID", "utterance", "state"))
  r.state <- with(comp.state, cor(prob, probability))
  
  # by valence
  model.valence <- aggregate(data=model, probability ~ imageID + utterance + valence, FUN=sum)
  model.valence.pos <- subset(model.valence, valence=="pos")
  comp.valence <- join(interp.valence, model.valence.pos, by=c("imageID", "utterance"))
  r.valence <- with(comp.valence, cor(value.corrected, probability))
   
  # by arousal
  model.arousal <- aggregate(data=model, probability ~ imageID + utterance + arousal, FUN=sum)
  model.arousal.high <- subset(model.arousal, arousal=="high")
  comp.arousal <- join(interp.arousal, model.arousal.high, by=c("imageID", "utterance"))
  r.arousal <- with(comp.arousal, cor(value.corrected, probability))
  
  r <- mean(c(r.state, r.valence, r.arousal))
  d <- data.frame(name=f, cor.state=r.state, cor.valence=r.valence, cor.arousal=r.arousal, cor=r)
  models <- rbind(models, d)
}

models <- models[with(models, order(-cor)), ]

bestName <- as.character(subset(models, cor==max(models$cor))$name)

model <- read.csv(paste("../model/parsedOutputsWithParams_smoothed/", bestName, sep=""))
model$utterance <- factor(model$utterance, levels=c("terrible", "bad", "ok", "good", "amazing"),
                          labels=c("terrible", "bad", "neutral", "good", "amazing"))
model$state <- factor(model$state, levels=c("terrible", "bad", "neutral", "good", "amazing"))
model.state <- aggregate(data=model, probability ~ imageID + utterance + state, FUN=sum)
model.valence <- aggregate(data=model, probability ~ imageID + utterance + valence, FUN=sum)
model.arousal <- aggregate(data=model, probability ~ imageID + utterance + arousal, FUN=sum)
comp.state <- join(interp.states, model.state, by=c("imageID", "utterance", "state"))

# ggplot(model.state, aes(x=state, y=probability)) +
#   geom_bar(stat="identity", color="black", fill="grey") +
#   facet_grid(imageID~utterance) +
#   theme_bw()

# ggplot(model.valence, aes(x=valence, y=probability)) +
#   geom_bar(stat="identity", color="black", fill="grey") +
#   facet_grid(imageID~utterance) +
#   theme_bw()

# ggplot(model.arousal, aes(x=arousal, y=probability)) +
#   geom_bar(stat="identity", color="black", fill="grey") +
#   facet_grid(imageID~utterance) +
#   theme_bw()
#####################################
# Model human comparison
#####################################

#####
# State 
#####
comp.state <- join(interp.states, model.state, by=c("imageID", "utterance", "state"))
with(comp.state, cor.test(prob, probability))
comp.state$literal <- ifelse(comp.state$state==comp.state$utterance, "literal", "non")

ggplot(comp.state, aes(x=probability, y=prob)) +
  #geom_text(aes(label=state)) +
  geom_point(color="black") +
  #facet_grid(imageID ~ utterance) +
  geom_smooth(method=lm, color="gray") +
  #ylim(c(0, 1)) +
  #xlim(c(0, 1)) +
  theme_bw() +
  xlab("Model") +
  ylab("Humans")

colnames(model.state)[4] <- "prob"
interp.states$type <- "humans"
model.state$type <- "model"
interp.states$imageID.utterance <- NULL
comp.state.long <- rbind(interp.states, model.state)
comp.state.long$imageID.reordered <- factor(comp.state.long$imageID, levels=c(3, 2, 1, 6, 5, 4, 8, 7, 9),
                                                    labels=c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9"))
comp.state.long$imageCategory <- ifelse(as.numeric(comp.state.long$imageID) <= 3, "Positive", 
                                      ifelse(as.numeric(comp.state.long$imageID) >= 7,
                                             "Negative", "Neutral"))
comp.state.long$imageCategory <- factor(comp.state.long$imageCategory, levels=c("Positive", "Neutral", "Negative"))
ggplot(comp.state.long, aes(x=state, y=prob, color=imageCategory)) +
  geom_point(size=3) +
  geom_line(aes(group=type, linetype=type)) +
  facet_grid(imageID.reordered ~ utterance) +
  theme_bw() +
  scale_color_manual(values=c("#fc4e2a", "#99d8c9", "#2b8cbe"), name="Weather type") +
  scale_linetype_discrete(name="Interpretation") +
  ylab("Probability") +
  xlab("Weather state") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

#####
# Valence
#####
interp.valence <- subset(affect.pca.summary, variable=="Comp.1")
model.valence.pos <- subset(model.valence, valence=="pos")
comp.valence <- join(interp.valence, model.valence.pos, by=c("imageID", "utterance"))
with(comp.valence, cor.test(value.corrected, probability))
ggplot(comp.valence, aes(x=probability, y=value.corrected)) +
  geom_point() +
  theme_bw() +
  xlab("Model") +
  ylab("Humans") +
  geom_smooth(method=lm, color="gray") 

#####
# Arousal
#####
interp.arousal <- subset(affect.pca.summary, variable=="Comp.2")
model.arousal.high <- subset(model.arousal, arousal=="high")
comp.arousal <- join(interp.arousal, model.arousal.high, by=c("imageID", "utterance"))
with(comp.arousal, cor.test(value.corrected, probability))
ggplot(comp.arousal, aes(x=probability, y=value.corrected)) +
  geom_point() +
  theme_bw() +
  #facet_wrap(~imageID, ncol=3) +
  xlab("Model") +
  ylab("Humans") +
  geom_smooth(method=lm, color="gray")

##################
# Combined scatter plot
###################
comp.state$interp <- "state"
comp.valence$interp <- "valence"
comp.arousal$interp <- "arousal"
comp.all <- data.frame(interp=c(comp.state$interp, comp.valence$interp, comp.arousal$interp),
                       imageID=c(comp.state$imageID, comp.valence$imageID, comp.arousal$imageID),
                       human=c(comp.state$prob, comp.valence$value.corrected, comp.arousal$value.corrected),
                       model=c(comp.state$probability, comp.valence$probability, comp.arousal$probability))
comp.all$interp <- factor(comp.all$interp, levels=c("state", "valence", "arousal"))
ggplot(comp.all, aes(x=model, y=human)) +
  geom_point() +
  geom_smooth(method=lm, color="grey") +
  facet_grid(interp~.) +
  theme_bw() +
  ylim(c(0, 1))

#########################
# Add priors
#########################
####
# State
####
colnames(priors.states)[2] <- "state"
priors.states$utterance <- "prior"
colnames(priors.states)[3] <- "prob"
interp.states$imageID.utterance <- NULL
colnames(interp.states)[2] <- "prob"

comp.prior.state <- rbind(priors.states, interp.states)
comp.prior.state$utterance <- factor(comp.prior.state$utterance, levels=c("prior", "terrible", "bad", "neutral",
                                                                          "good", "amazing"))
colors.utterances <- c("#990000", "#ff9966", "#66cc99", "#3399ff", "#003399")
ggplot(comp.prior.state, aes(x=state, y=prob, color=utterance)) +
  geom_point(size=3) +
  geom_line(aes(group=utterance)) +
  theme_bw() +
  facet_wrap(~imageID, ncol=3) +
  scale_color_manual(values=c("gray", colors.utterances)) +
  ylab("Proportion of subjects") +
  xlab("State")

#####
# Valence
#####
priors.affect.byImage.summary <- summarySE(priors.pca.probit.long, measurevar="value.corrected",
                                       groupvars=c("variable", "imageID"))

priors.valence <- subset(priors.affect.byImage.summary, variable=="Comp.1")
priors.valence$N <- NULL
priors.valence$sd <- NULL
priors.valence$se <- NULL
priors.valence$ci <- NULL

interp.valence <- rename(interp.valence, replace=c("value.corrected"="probability"))
interp.valence$N <- NULL
interp.valence$sd <- NULL
interp.valence$se <- NULL
interp.valence$type <- "human"
model.valence.pos$variable <- "Comp.1"
model.valence.pos$valence <- NULL
model.valence.pos$ci <- 0
model.valence.pos$type <- "model"
comp.valence.long <- rbind(interp.valence, model.valence.pos)

comp.prior.valence <- join(priors.valence, comp.valence.long, by="imageID")
comp.prior.valence$utterance <- factor(comp.prior.valence$utterance, levels=c("prior", "terrible", "bad",
                                                                          "neutral", "good", "amazing"))
comp.prior.valence$imageID.reordered <- factor(comp.prior.valence$imageID, levels=c(3, 2, 1, 6, 5, 4, 8, 7, 9),
                                          labels=c("Weather context 1", "Weather context 2", "Weather context 3", 
                                                   "Weather context 4", "Weather context 5", "Weather context 6", 
                                                   "Weather context 7", "Weather context 8", "weather contextw 9"))

colnames(comp.prior.valence)[3] <- "prior"

comp.prior.valence$imageCategory <- ifelse(as.numeric(comp.prior.valence$imageID) <= 3, "Positive", 
                                        ifelse(as.numeric(comp.prior.valence$imageID) >= 7,
                                               "Negative", "Neutral"))
comp.prior.valence$imageCategory <- factor(comp.prior.valence$imageCategory, levels=c("Positive", "Neutral", "Negative"))
ggplot(comp.prior.valence, aes(x=utterance, y=probability, color=imageCategory)) +
  geom_point() +
  geom_line(aes(group=type, linetype=type)) +
  geom_hline(aes(yintercept=prior), color="grey", linetype=2) +
  #geom_bar(stat="identity", color="black", fill="grey") +
  geom_errorbar(aes(ymin=probability-ci, ymax=probability+ci), width=0.05) +
  theme_bw() +
  facet_wrap(~imageID.reordered, ncol=3) +
  scale_color_manual(values=c("#fc4e2a", "#99d8c9", "#2b8cbe"), name="Weather type", guide=FALSE) +
  scale_linetype_discrete(name="Interpretation", labels=c("Human", "Model"), guide=FALSE) +
  xlab("Utterance") +
  ylab("Probability of positive valence")

#####
# Arousal
#####
priors.arousal <- subset(priors.affect.byImage.summary, variable=="Comp.2")

priors.arousal$N <- NULL
priors.arousal$sd <- NULL
priors.arousal$se <- NULL
priors.arousal$ci <- NULL

interp.arousal <- rename(interp.arousal, replace=c("value.corrected"="probability"))
interp.arousal$N <- NULL
interp.arousal$sd <- NULL
interp.arousal$se <- NULL
interp.arousal$type <- "human"
model.arousal.high$variable <- "Comp.2"
model.arousal.high$arousal <- NULL
model.arousal.high$ci <- 0
model.arousal.high$type <- "model"
comp.arousal.long <- rbind(interp.arousal, model.arousal.high)

comp.prior.arousal <- join(priors.arousal, comp.arousal.long, by="imageID")
comp.prior.arousal$utterance <- factor(comp.prior.arousal$utterance, levels=c("prior", "terrible", "bad",
                                                                              "neutral", "good", "amazing"))
comp.prior.arousal$imageID.reordered <- factor(comp.prior.arousal$imageID, levels=c(3, 2, 1, 6, 5, 4, 8, 7, 9),
                                               labels=c("Weather context 1", "Weather context 2", "Weather context 3", 
                                                        "Weather context 4", "Weather context 5", "Weather context 6", 
                                                        "Weather context 7", "Weather context 8", "weather contextw 9"))

colnames(comp.prior.arousal)[3] <- "prior"

comp.prior.arousal$imageCategory <- ifelse(as.numeric(comp.prior.arousal$imageID) <= 3, "Positive", 
                                           ifelse(as.numeric(comp.prior.arousal$imageID) >= 7,
                                                  "Negative", "Neutral"))
comp.prior.arousal$imageCategory <- factor(comp.prior.arousal$imageCategory, levels=c("Positive", "Neutral", "Negative"))
ggplot(comp.prior.arousal, aes(x=utterance, y=probability, color=imageCategory)) +
  geom_point() +
  geom_line(aes(group=type, linetype=type)) +
  geom_hline(aes(yintercept=prior), color="grey", linetype=2) +
  #geom_bar(stat="identity", color="black", fill="grey") +
  geom_errorbar(aes(ymin=probability-ci, ymax=probability+ci), width=0.05) +
  theme_bw() +
  facet_wrap(~imageID.reordered, ncol=3) +
  scale_color_manual(values=c("#fc4e2a", "#99d8c9", "#2b8cbe"), name="Weather type") +
  scale_linetype_discrete(name="Interpretation", labels=c("Human", "Model")) +
  xlab("Utterance") +
  ylab("Probability of high arousal")

comp.prior.arousal <- join(priors.arousal, interp.arousal, by="imageID")
comp.prior.arousal$utterance <- factor(comp.prior.arousal$utterance, levels=c("prior", "terrible", "bad",
                                                                              "neutral", "good", "amazing"))
comp.prior.arousal$imageID.reordered <- factor(comp.prior.arousal$imageID, levels=c(3, 2, 1, 6, 5, 4, 8, 7, 9),
                                               labels=c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9"))

colnames(comp.prior.arousal)[4] <- "prior"



ggplot(comp.prior.arousal, aes(x=utterance, y=value.corrected)) +
  geom_point() +
  geom_line(aes(group=imageID.reordered)) +
  geom_hline(aes(yintercept=prior), color="grey", linetype=2) +
  #geom_bar(stat="identity", color="black", fill="grey") +
  geom_errorbar(aes(ymin=value.corrected-ci, ymax=value.corrected+ci), width=0.05) +
  theme_bw() +
  facet_wrap(~imageID.reordered, ncol=3) +
  scale_color_manual(values=c("gray", colors.utterances)) +
  xlab("Utterance") +
  ylab("Probability of high arousal")

priors.arousal <- subset(priors.pca.probit.summary, variable=="Comp.2")
comp.prior.arousal <- rbind(priors.arousal, interp.arousal)
comp.prior.arousal$utterance <- factor(comp.prior.arousal$utterance, levels=c("prior", "terrible", "bad",
                                                                              "neutral", "good", "amazing"))
ggplot(comp.prior.arousal, aes(x=utterance, y=value)) +
  geom_bar(stat="identity", color="black", fill="grey") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=0.2) +
  theme_bw() +
  facet_wrap(~imageID, ncol=3) 

#################################
# Toy model simulation
#################################
toy.priors <- data.frame(imageID=c(1,1,1,1,1,2,2,2,2,2), utterance=c("prior", "prior", "prior", "prior", "prior",
                                                                       "prior", "prior", "prior", "prior", "prior"),
                         state=c("terrible", "bad", "neutral", "good", "amazing", 
                                                                   "terrible", "bad", "neutral", "good", "amazing"),
                         probability=c(0.1, 0.5, 0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.5))

toy.1 <- read.csv("../model/modeltoy-1affect.csv")
toy.1.state <- aggregate(data=toy.1, probability ~ imageID + utterance + state, FUN=sum)
toy.1.state.priors <- rbind(toy.priors, toy.1.state)
toy.1.state.priors$state <- factor(toy.1.state.priors$state, levels=c("terrible", "bad", "neutral", "good", "amazing"))
toy.1.state.priors$imageID <- factor(toy.1.state.priors$imageID, labels=c("Bad weather", "Amazing weather"))
ggplot(toy.1.state.priors, aes(x=state, y=probability, color=utterance)) +
  geom_point() +
  geom_line(aes(group=utterance)) +
  facet_grid(.~imageID) +
  theme_bw() +
  xlab("Weather state") +
  ylab("Probability") +
  scale_color_manual(values=c("grey", "black"), labels=c("Prior", '"The weather is terrible."'), name="") +
  ylim(c(0, 0.6))

toy.2 <- read.csv("../model/modeltoy-2affect.csv")
toy.2.state <- aggregate(data=toy.2, probability ~ imageID + utterance + state, FUN=sum)
toy.2.state.priors <- rbind(toy.priors, toy.2.state)
toy.2.state.priors$state <- factor(toy.2.state.priors$state, levels=c("terrible", "bad", "neutral", "good", "amazing"))
toy.2.state.priors$imageID <- factor(toy.2.state.priors$imageID, labels=c("Bad weather", "Amazing weather"))
ggplot(toy.2.state.priors, aes(x=state, y=probability, color=utterance)) +
  geom_point() +
  geom_line(aes(group=utterance)) +
  facet_grid(.~imageID) +
  theme_bw() +
  xlab("Weather state") +
  ylab("Probability") +
  scale_color_manual(values=c("grey", "black"), labels=c("Prior", '"The weather is terrible."'), name="") +
  ylim(c(0, 0.6))

########
# Plot in one figure
########
toy.1.state$affect <- "Valence"
toy.2.state$affect <- "Valence and Arousal"
toy.priors$affect <- "Prior"
toy.model.comp <- rbind(toy.1.state, toy.2.state, toy.priors)
toy.model.comp$state <- factor(toy.model.comp$state, levels=c("terrible", "bad", "neutral", "good", "amazing"))
toy.model.comp$imageID <- factor(toy.model.comp$imageID, labels=c("Bad weather", "Amazing weather"))
ggplot(toy.model.comp, aes(x=state, y=probability, color=affect)) +
  geom_point() +
  geom_line(aes(group=affect, linetype=affect)) +
  facet_grid(.~imageID) +
  theme_bw() +
  xlab("Weather state") +
  ylab("Probability") +
  scale_color_manual(values=c("grey", "#8da0cb", "#d95f02"), name="") +
  scale_linetype_manual(values=c(2, 1, 1), guide=FALSE) +
  #scale_color_brewer(palette="Set2") +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

