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
                                          labels=c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9"))


ggplot(priors.states, aes(x=stateRating, y=prior)) +
  geom_point(size=3, color="gray") +
  geom_line(aes(group=imageID), color="gray") +
  ylab("Proportion of participants") +
  xlab("Weather state rating") +
  facet_wrap(~imageID.reordered, ncol=3) +
  theme_bw()

ggplot(priors.states, aes(x=stateRating, y=prior, color=imageID)) +
  geom_point(size=3) +
  geom_line(aes(group=imageID), color="gray") +
  ylab("Proportion of subjects") +
  xlab("State") +
  #facet_wrap(~imageID, ncol=3) +
  theme_bw()

# affect ratings

comps <- princomp(data=priors, ~ sad + disgusted + angry + neutral + content + happy + excited, cor=FALSE, scores=TRUE)
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

ggplot(comp.state, aes(x=probability, y=prob, color=literal)) +
  #geom_text(aes(label=state)) +
  geom_point() +
  #facet_grid(imageID ~ utterance) +
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
ggplot(comp.state.long, aes(x=state, y=prob, color=type)) +
  geom_point(size=3) +
  geom_line(aes(group=type)) +
  facet_grid(imageID.reordered ~ utterance) +
  theme_bw() +
  scale_color_manual(values=c("#003366", "#3399ff"), name="") +
  scale_linetype_discrete(legend=NULL) +
  ylab("Probability of weather state given utterance") +
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
  ylab("Humans") 

#####
# Arousal
#####
interp.arousal <- subset(affect.pca.summary, variable=="Comp.2")
model.arousal.high <- subset(model.arousal, arousal=="high")
comp.arousal <- join(interp.arousal, model.arousal.high, by=c("imageID", "utterance"))
with(comp.arousal, cor.test(value.corrected, probability))
ggplot(comp.arousal, aes(x=probability, y=value.corrected, color=utterance)) +
  geom_point() +
  theme_bw() +
  #facet_wrap(~imageID, ncol=3) +
  xlab("Model") +
  ylab("Humans")
  

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
comp.prior.valence <- join(priors.valence, interp.valence, by="imageID")
comp.prior.valence$utterance <- factor(comp.prior.valence$utterance, levels=c("prior", "terrible", "bad",
                                                                          "neutral", "good", "amazing"))
comp.prior.valence$imageID.reordered <- factor(comp.prior.valence$imageID, levels=c(3, 2, 1, 6, 5, 4, 8, 7, 9),
                                          labels=c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9"))

colnames(comp.prior.valence)[4] <- "prior"

ggplot(comp.prior.valence, aes(x=utterance, y=value.corrected)) +
  geom_point() +
  geom_line(aes(group=imageID.reordered)) +
  geom_hline(aes(yintercept=prior), color="grey", linetype=2) +
  #geom_bar(stat="identity", color="black", fill="grey") +
  geom_errorbar(aes(ymin=value.corrected-ci, ymax=value.corrected+ci), width=0.05) +
  theme_bw() +
  facet_wrap(~imageID.reordered, ncol=3) +
  scale_color_manual(values=c("gray", colors.utterances)) +
  xlab("Utterance") +
  ylab("Probability of positive valence")

#####
# Arousal
#####
priors.arousal <- subset(priors.affect.byImage.summary, variable=="Comp.2")
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

