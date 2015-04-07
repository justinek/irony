library(ggplot2)
library(reshape2)
library(plyr)
library(ggbiplot)
library(lme4)
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
  
priors.pca.probit.byImage

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

t.test(subset(irony.summary, consistent==1)$ironyRating, subset(irony.summary, consistent==0)$ironyRating)
summary(lm(data=irony.summary, ironyRating ~ consistent))
summary(lm(data=irony.summary, ironyRating ~ utteranceValence * imageValence))

ggplot(irony.summary, aes(x=utterance, y=ironyRating)) +
  geom_bar(stat="identity", color="black", fill="grey", position=position_dodge()) +
  geom_errorbar(aes(ymin=ironyRating-se, ymax=ironyRating+se), position=position_dodge(0.9), width=0.2) +
  facet_wrap(~imageID, ncol=3) +
  theme_bw() +
  ylab("Irony rating") 


# Mixed model
interp$imageValence <- ifelse(interp$imageID == 4, "neutral",
                                     ifelse(interp$imageID == 7 | interp$imageID == 8 | interp$imageID == 9,
                                            "neg", "pos"))
interp$utteranceValence <- ifelse(interp$utterance == "neutral", "neutral",
                                         ifelse(interp$utterance == "amazing" | interp$utterance == "good", "pos",
                                                "neg"))
interp$consistent <- ifelse(interp$imageValence == interp$utteranceValence, 1, 0)

lm.irony <- lmer(data=interp, ironyRating ~ imageValence * utteranceValence + (1| workerID), REML=FALSE) 
summary(lm.irony)

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

interp.states$imageID.reordered <- factor(interp.states$imageID, levels=c(3, 2, 1, 6, 5, 4, 8, 7, 9),
                                               labels=c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9"))

interp.states$priorCat <- ifelse(interp.states$imageID <=3, "Positive", ifelse(interp.states$imageID >=7, "Negative", "Neutral"))
interp.states$priorCat <- factor(interp.states$priorCat, levels=c("Positive", "Neutral", "Negative"))
ggplot(interp.states, aes(x=state, y=Freq)) +
  #geom_bar(stat="identity", fill="gray", color="black") + 
  geom_point(aes(color=priorCat)) +
  geom_line(aes(group=imageID, color=priorCat)) +
  ylab("probability") +
  facet_grid(imageID.reordered~utterance) +
  theme_bw() +
  scale_color_manual(values=c("#fc4e2a", "#99d8c9", "#2b8cbe"), name="Weather type") +
  ylim(c(0, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create plot of just w2, w6, w8 for illustration purposes
interp.states.subset <- subset(interp.states, (imageID.reordered=="w2" | imageID.reordered=="w6" | imageID.reordered=="w8")
                               & utterance=="terrible") 

interp.states.subset$imageID.reordered <- factor(interp.states.subset$imageID.reordered, levels=c("w8", "w6", "w2"))
ggplot(interp.states.subset, aes(x=state, y=Freq)) +
  #geom_bar(stat="identity", fill="gray", color="black") + 
  geom_point(aes(color=priorCat)) +
  geom_line(aes(group=imageID, color=priorCat)) +
  ylab("probability") +
  facet_grid(.~imageID.reordered) +
  theme_bw() +
  #theme(axis.text.x = element_text(angle= 90, hjust=1)) +
  scale_color_manual(values=c("#fc4e2a", "#99d8c9", "#2b8cbe"), guide=FALSE)
  

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

# states
splithalf.state <- data.frame(cors=NULL)
t = 1
while (t <= 100) {
  nWorkers <- length(unique(interp$workerID))
  ii <- seq_len(nWorkers)
  indices <- sample(ii, nWorkers, replace = TRUE)
  ind1 <- indices[1:ceiling(nWorkers/2)]
  ind2 <- indices[(ceiling(nWorkers/2) + 1):length(indices)] 
  #ind1 <- sample(ii, nWorkers / 2) 
  #ind2 <- ii[!ii %in% ind1] 
  interp.1 <- subset(interp, workerID %in% ind1)
  interp.2 <- subset(interp, workerID %in% ind2)
  states.1 <- as.data.frame(prop.table(with(interp.1, table(imageID.utterance, stateRating)) + 1, margin=1))
  states.2 <- as.data.frame(prop.table(with(interp.2, table(imageID.utterance, stateRating)) + 1, margin=1))
  colnames(states.1)[3] <- "prob.1"
  colnames(states.2)[3] <- "prob.2"
  if (nrow(states.1) == nrow(states.2)) {
    states.split <- join(states.1, states.2, by=c("imageID.utterance", "stateRating"))
    t <- t+1
    r <- with(states.split, cor(prob.1, prob.2))
    this.frame <- data.frame(cors=r)
    splithalf.state <- rbind(splithalf.state, this.frame)
  }
}

splithalf.state <- subset(splithalf.state, !is.na(cors))

prophet <- function(reliability, length) {
  prophecy <- length * reliability / (1 + (length - 1)*reliability)
  return (prophecy)
}

splithalf.state$proph <- prophet(splithalf.state$cors, 2)

split.cor.state <- summarySE(splithalf.state, measurevar=c("proph", groupvars=NULL))

# affects

splithalf.cors <- data.frame(valence=NULL, arousal=NULL)
t = 1
while (t <= 1000) {
  nWorkers <- length(unique(affect.pca$workerID))
  ii <- seq_len(nWorkers)
  indices <- sample(ii, nWorkers, replace = TRUE)
  ind1 <- indices[1:ceiling(nWorkers/2)]
  ind2 <- indices[(ceiling(nWorkers/2) + 1):length(indices)] 
  
  #ind1 <- sample(ii, nWorkers / 2) 
  #ind2 <- ii[!ii %in% ind1] 
  h1 <- subset(affect.pca, workerID %in% ind1)
  h2 <- subset(affect.pca, workerID %in% ind2)
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

splithalf.cors <- splithalf.cors[complete.cases(splithalf.cors),]

splithalf.cors$valence.proph <- prophet(splithalf.cors$valence, 2)
splithalf.cors$arousal.proph <- prophet(splithalf.cors$arousal, 2)

valence.proph.summary  <- summarySE(data=splithalf.cors, measurevar="valence.proph", groupvars=NULL)
arousal.proph.summary  <- summarySE(data=splithalf.cors, measurevar="arousal.proph", groupvars=NULL)

#################
# Irony split-half
#################
irony.cors <- data.frame(cors=NULL)
t = 1
while (t <= 100) {
  nWorkers <- length(unique(interp$workerID))
  ii <- seq_len(nWorkers)
  indices <- sample(ii, nWorkers, replace = TRUE)
  ind1 <- indices[1:ceiling(nWorkers/2)]
  ind2 <- indices[(ceiling(nWorkers/2) + 1):length(indices)] 
  
  #ind1 <- sample(ii, nWorkers / 2) 
  #ind2 <- ii[!ii %in% ind1] 
  h1 <- subset(interp, workerID %in% ind1)
  h2 <- subset(interp, workerID %in% ind2)
  h1.summary <- summarySE(h1, measurevar="ironyRating", groupvars=c("imageID", "utterance"))
  h2.summary <- summarySE(h2, measurevar="ironyRating", groupvars=c("imageID", "utterance"))
  colnames(h1.summary)[4] <- "irony1"
  colnames(h2.summary)[4] <- "irony2"
  if (nrow(h1.summary) == nrow(h2.summary)) {
    splithalf.i <- join(h1.summary, h2.summary, by=c("imageID", "utterance"))
    t <- t+1
    i.cor <- with(splithalf.i, cor(irony1, irony2))
    this.frame <- data.frame(cors=i.cor)
    irony.cors <- rbind(irony.cors, this.frame)
  }
}

irony.cors <- subset(irony.cors, !is.na(cors))
irony.cors$proph <- prophet(irony.cors$cors, 2)

irony.cors.summary  <- summarySE(data=irony.cors, measurevar="proph", groupvars=NULL)

######################

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

affect.pca.summary$imageID.reordered <- factor(affect.pca.summary$imageID, levels=c(3, 2, 1, 6, 5, 4, 8, 7, 9),
                                               labels=c("w1", "w2", "w3", 
                                                        "w4", "w5", "w6", 
                                                        "w7", "w8", "w9"))
# subset for presentation (w2, w6, w8)
affect.pca.summary.subset <- subset(affect.pca.summary, (imageID.reordered == "w2" | imageID.reordered=="w6" | imageID.reordered=="w8")
                                    & utterance=="terrible")
affect.pca.summary.subset$imageID.reordered <- factor(affect.pca.summary.subset$imageID.reordered, levels=c("w8", "w6", "w2"))
ggplot(affect.pca.summary.subset, aes(x=variable, y=value.corrected)) +
  geom_bar(stat="identity") +
  theme_bw() +
  facet_grid(.~imageID.reordered)

priors.affect.byImage.summary$imageID.reordered <- factor(priors.affect.byImage.summary$imageID, levels=c(3, 2, 1, 6, 5, 4, 8, 7, 9),
                                               labels=c("w1", "w2", "w3", 
                                                        "w4", "w5", "w6", 
                                                        "w7", "w8", "w9"))


####################################
# Model
####################################
interp.valence <- subset(affect.pca.summary, variable=="Comp.1")
interp.arousal <- subset(affect.pca.summary, variable=="Comp.2")

filenames <- read.csv("../model/parsedOutputsWithParams_singleGoal/filenames.txt", header=FALSE)
models <- data.frame(name=NULL, cor.state=NULL, cor.valence=NULL, cor.arousal=NULL, cor=NULL)
for (f in filenames$V1) {
  model <- read.csv(paste("../model/parsedOutputsWithParams_singleGoal/", f, sep=""))
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

model <- read.csv(paste("../model/parsedOutputsWithParams_singleGoal/", bestName, sep=""))
####
# Model with no arousal goal
####
#model <- read.csv("../model/model0-noArousal.csv")
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
splithalf.state <- as.data.frame(splithalf.state)
splithalf.state$modelCor <- best.state
splithalf.state$modelPercent <- splithalf.state$modelCor / splithalf.state$proph
summarySE(splithalf.state, measurevar="modelPercent", groupvars=NULL)

# getValence <- function(state) {
#   if (state=="terrible" | state=="bad") {
#     return("neg")
#   } else if (state=="neutral") {
#     return("neutral")
#   } else {
#     return("pos")
#   }
# }

#####################
# POTENTIAL FIGURE
#####################
ggplot(comp.state, aes(x=probability, y=prob, color=literal)) +
  #geom_text(aes(label=state)) +
  geom_point() +
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

#####################
# POTENTIAL FIGURE
#####################

p.model.state <- ggplot(comp.state.long, aes(x=state, y=prob, color=imageCategory)) +
  geom_point(size=3) +
  geom_line(aes(group=type, linetype=type)) +
  facet_grid(imageID.reordered ~ utterance) +
  theme_bw() +
  scale_color_manual(values=c("#fc4e2a", "#99d8c9", "#2b8cbe"), name="Weather type") +
  scale_linetype_discrete(name="Interpretation") +
  ylab("Probability") +
  xlab("Weather state") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("../writing/cogsci2015/model-state.pdf", width=10, height=9, units="in")

#####
# Valence
#####
splithalf.cors$modelValence <- with(comp.valence, cor(value.corrected, probability))
splithalf.cors$modelArousal <- with(comp.arousal, cor(value.corrected, probability))
splithalf.cors$valencePercent <- splithalf.cors$modelValence / splithalf.cors$valence.proph
splithalf.cors$arousalPercent <- splithalf.cors$modelArousal /splithalf.cors$arousal.proph
summarySE(splithalf.cors, measurevar="valencePercent", groupvars=NULL)
summarySE(splithalf.cors, measurevar="arousalPercent", groupvars=NULL)

interp.valence <- subset(affect.pca.summary, variable=="Comp.1")
model.valence.pos <- subset(model.valence, valence=="pos")
comp.valence <- join(interp.valence, model.valence.pos, by=c("imageID", "utterance"))
with(comp.valence, cor.test(value.corrected, probability))
comp.valence$utteranceValence <- ifelse(comp.valence$utterance=="terrible" | comp.valence$utterance=="bad", "neg",
                                        ifelse(comp.valence$utterance == "neutral", "neutral", "pos"))
comp.valence$interpValence <- ifelse(comp.valence$probability < 0.5, "neg", "pos")
comp.valence$valenceFlip <- ifelse(comp.valence$utteranceValence == comp.valence$interpValence, "no", "yes")
#####################
# POTENTIAL FIGURE
#####################

ggplot(comp.valence, aes(x=probability, y=value.corrected, color=utterance)) +
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

#####################
# POTENTIAL FIGURE
#####################
ggplot(comp.arousal, aes(x=probability, y=value.corrected, color=utterance)) +
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
                       model=c(comp.state$probability, comp.valence$probability, comp.arousal$probability),
                       utterance=c(as.character(comp.state$utterance), as.character(comp.valence$utterance), 
                                   as.character(comp.arousal$utterance)))
comp.all$interp <- factor(comp.all$interp, levels=c("state", "valence", "arousal"), labels=c("State", "Valence", "Arousal"))
comp.all$utterance <- factor(comp.all$utterance, levels=c("terrible", "bad", "neutral", "good", "amazing"))
ggplot(comp.all, aes(x=model, y=human, color=utterance)) +
  geom_point() +
  geom_smooth(aes(group=interp), method=lm, color="grey") +
  facet_grid(.~interp) +
  theme_bw() +
  ylim(c(0, 1)) +
  scale_color_manual(values=c("#2c7bb6", "#abd9e9", "gray", "#fdae61", "#d7191c"), name="Utterance") +
  xlab("Model") +
  ylab("Human") +
  theme(axis.text.x = element_text(size=11), axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=15), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size = 16),
        legend.text=element_text(size=14), legend.title=element_text(size=15))

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

###################
# Add priors to correlate with posterior ratings
###################
comp.prior.state.wide <- join(priors.states, interp.states, by=c("imageID", "state"))
colnames(comp.prior.state.wide)[3] <- "prior"
with(comp.prior.state.wide, cor.test(prior, prob))

# comp.prior.state <- rbind(priors.states, interp.states)
# comp.prior.state$utterance <- factor(comp.prior.state$utterance, levels=c("prior", "terrible", "bad", "neutral",
#                                                                           "good", "amazing"))
# colors.utterances <- c("#990000", "#ff9966", "#66cc99", "#3399ff", "#003399")
# ggplot(comp.prior.state, aes(x=state, y=prob, color=utterance)) +
#   geom_point(size=3) +
#   geom_line(aes(group=utterance)) +
#   theme_bw() +
#   facet_wrap(~imageID, ncol=3) +
#   scale_color_manual(values=c("gray", colors.utterances)) +
#   ylab("Proportion of subjects") +
#   xlab("State")

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

summary(lm(data=comp.prior.valence, probability ~ utterance))
summary(lm(data=comp.prior.valence, probability ~ prior))
summary(lm(data=comp.prior.valence, probability ~ utterance * prior))

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

colnames(comp.prior.arousal)[3] <- "prior"

summary(lm(data=comp.prior.arousal, probability ~ prior))
summary(lm(data=comp.prior.arousal, probability ~ utterance))
summary(lm(data=comp.prior.arousal, probability ~ utterance * prior))

ggplot(comp.prior.arousal, aes(x=utterance, y=probability)) +
  geom_point() +
  geom_line(aes(group=imageID.reordered)) +
  geom_hline(aes(yintercept=prior), color="grey", linetype=2) +
  #geom_bar(stat="identity", color="black", fill="grey") +
  geom_errorbar(aes(ymin=probability-ci, ymax=probability+ci), width=0.05) +
  theme_bw() +
  facet_wrap(~imageID.reordered, ncol=3) +
  scale_color_manual(values=c("gray", colors.utterances)) +
  xlab("Utterance") +
  ylab("Probability of high arousal")

################################
# Model's prediction of irony
###############################
priors.byState.valence <- subset(priors.pca.probit.summary, variable=="Comp.1")
colnames(priors.byState.valence)[2] <- "utterance"
colnames(priors.byState.valence)[4] <- "literalValence"
model.valence.litComp <- join(model.valence.pos, priors.byState.valence, by=c("utterance"))
model.valence.litComp.irony <- join(model.valence.litComp, irony.summary, by=c("imageID", "utterance"))

model.valence.litComp.irony$valenceFlip <- abs(model.valence.litComp.irony$probability - model.valence.litComp.irony$literalValence)
model.valence.litComp.irony$valenceDiff <- model.valence.litComp.irony$probability - model.valence.litComp.irony$literalValence


summary(lm(data=model.valence.litComp.irony, ironyRating ~ probability * literalValence))
summary(lm(data=model.valence.litComp.irony, ironyRating ~ valenceFlip))
with(model.valence.litComp.irony, cor.test(ironyRating, valenceFlip))
summary(lm(data=model.valence.litComp.irony, ironyRating ~ valenceDiff))
model.valence.litComp.irony$literalValenceCat <- ifelse(model.valence.litComp.irony$literalValence < 0.5, "neg", "pos")
model.valence.litComp.irony$inferredValenceCat <- ifelse(model.valence.litComp.irony$probability < 0.5, "neg", "pos")
model.valence.litComp.irony$litInferredConsistent <- ifelse(model.valence.litComp.irony$literalValenceCat==
                                                              model.valence.litComp.irony$inferredValenceCat, 1, 0)
model.valence.litComp.irony$consistent <- factor(model.valence.litComp.irony$consistent)
model.valence.litComp.irony$litInferredConsistent <- factor(model.valence.litComp.irony$litInferredConsistent)
summary(lm(data=model.valence.litComp.irony, ironyRating ~ consistent))
summary(lm(data=model.valence.litComp.irony, ironyRating ~ litInferredConsistent))

ggplot(model.valence.litComp.irony, aes(x=valenceFlip, y=ironyRating, color=utterance)) +
  geom_point() +
  theme_bw()



#################################
# Toy model simulation
#################################
toy.priors <- data.frame(imageID=c(1,1,1,1,1,2,2,2,2,2), utterance=c("prior", "prior", "prior", "prior", "prior",
                                                                       "prior", "prior", "prior", "prior", "prior"),
                         state=c("terrible", "bad", "neutral", "good", "amazing", 
                                                                   "terrible", "bad", "neutral", "good", "amazing"),
                         probability=c(0.1, 0.5, 0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.5))


toy.priors$state <- factor(toy.priors$state, levels=c("terrible", "bad", "neutral", "good", "amazing"))
toy.priors$imageID <- factor(toy.priors$imageID, labels=c("Bad weather", "Amazing weather"))

ggplot(toy.priors, aes(x=state, y=probability, color="gray")) +
  geom_point() +
  geom_line(aes(group=imageID), linetype=2) +
  facet_grid(.~imageID) +
  theme_bw() +
  xlab("Weather state") +
  ylab("Probability") +
  scale_color_manual(values=c("grey"), labels=c("Prior"), name="", guide=FALSE) +
  ylim(c(0, 0.6))

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
  geom_line(aes(group=utterance, linetype=utterance)) +
  facet_grid(.~imageID) +
  theme_bw() +
  xlab("Weather state") +
  ylab("Probability") +
  scale_color_manual(values=c("grey", "#8da0cb"), labels=c("Prior", "Valence QUD"), name="", guide=FALSE) +
  scale_linetype_manual(values=c(2, 1), guide=FALSE) +
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

toy.ok <- read.csv("../model/modeltoy-2affectok.csv")
toy.ok.state <- aggregate(data=toy.ok, probability ~ imageID + utterance + state, FUN=sum)
toy.ok.state.priors <- rbind(toy.priors, toy.ok.state)
toy.ok.state.priors$state <- factor(toy.ok.state.priors$state, levels=c("terrible", "bad", "neutral", "good", "amazing"))
toy.ok.state.priors$imageID <- factor(toy.ok.state.priors$imageID, labels=c("Bad weather", "Amazing weather"))
ggplot(toy.ok.state.priors, aes(x=state, y=probability, color=utterance)) +
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
  scale_color_manual(values=c("grey", "#8da0cb", "#d95f02"), name="", guide=FALSE) +
  scale_linetype_manual(values=c(2, 1, 1), guide=FALSE) +
  #scale_color_brewer(palette="Set2") +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  ylim(0, 0.6)

#################################################################
# Model comparisons
#################################################################
interp.states
interp.valence
interp.arousal

###################
# Just prior
###################
########
# State
########
m.justPrior.state <- priors.states
colnames(m.justPrior.state)[3] <- "model"
m.justPrior.state$imageCategory <- NULL
m.justPrior.state$imageID.reordered <- NULL
m.justPrior.state$utterance <- NULL
m.justPrior.state <- join(m.justPrior.state, interp.states, by=c("imageID", "state"))
with(m.justPrior.state, cor.test(model, prob))
ggplot(m.justPrior.state, aes(x=model, y=prob, color=utterance)) +
  geom_point() +
  theme_bw()

#########
# Valence
#########
m.justPrior.valence <- priors.valence
colnames(m.justPrior.valence)[3] <- "model"
m.justPrior.valence <- join(m.justPrior.valence, interp.valence, by="imageID")
with(m.justPrior.valence, cor.test(model, value.corrected))
ggplot(m.justPrior.valence, aes(x=model, y=value.corrected, color=utterance)) +
  geom_point() +
  theme_bw()

#########
# Arousal
#########
m.justPrior.arousal <- priors.arousal
colnames(m.justPrior.arousal)[3] <- "model"
m.justPrior.arousal <- join(m.justPrior.arousal, interp.arousal, by="imageID")
with(m.justPrior.arousal, cor.test(model, value.corrected))
ggplot(m.justPrior.arousal, aes(x=model, y=value.corrected, color=utterance)) +
  geom_point() +
  theme_bw()

####################
# Just literal
####################
#######
# State
#######
m.literal.state <- interp.states
m.literal.state$model <- ifelse(m.literal.state$state == m.literal.state$utterance, 1, 0)
with(m.literal.state, cor.test(model, prob))
ggplot(m.literal.state, aes(x=model, y=prob, color=utterance)) +
  geom_point() +
  theme_bw()
########
# Valence
########
m.literal.valence <- priors.byState.valence
colnames(m.literal.valence)[4] <- "model"
m.literal.valence <- join(interp.valence, m.literal.valence, by=c("utterance", "variable"))
with(m.literal.valence, cor.test(model, value.corrected))
ggplot(m.literal.valence, aes(x=model, y=value.corrected, color=utterance)) +
  geom_point() +
  theme_bw()

########
# Arousal
########
m.literal.arousal <- summarySE(data=subset(priors.pca.probit.long, variable=="Comp.2"), measurevar=c("value.corrected"),
                               groupvars=c("stateRating"))
colnames(m.literal.arousal)[3] <- "model"
colnames(m.literal.arousal)[1] <- "utterance"
m.literal.arousal <- join(interp.arousal, m.literal.arousal, by=c("utterance"))
with(m.literal.arousal, cor.test(model, value.corrected))
ggplot(m.literal.arousal, aes(x=model, y=value.corrected, color=utterance)) +
  geom_point() +
  theme_bw()

###########################
# Just valence goal
###########################
filenames <- read.csv("../model/parsedOutputsWithParams_noArousal/filenames.txt", header=FALSE)
models.noA <- data.frame(name=NULL, cor.state=NULL, cor.valence=NULL, cor.arousal=NULL, cor=NULL)
for (f in filenames$V1) {
  model.noA <- read.csv(paste("../model/parsedOutputsWithParams_noArousal/", f, sep=""))
  model.noA$utterance <- factor(model.noA$utterance, levels=c("terrible", "bad", "ok", "good", "amazing"),
                            labels=c("terrible", "bad", "neutral", "good", "amazing"))
  
  # by state
  model.noA$state <- factor(model.noA$state, levels=c("terrible", "bad", "neutral", "good", "amazing"))
  model.state.noA <- aggregate(data=model.noA, probability ~ imageID + utterance + state, FUN=sum)
  comp.state.noA <- join(interp.states, model.state.noA, by=c("imageID", "utterance", "state"))
  r.state <- with(comp.state.noA, cor(prob, probability))
  
  # by valence
  model.valence.noA <- aggregate(data=model.noA, probability ~ imageID + utterance + valence, FUN=sum)
  model.valence.pos.noA <- subset(model.valence.noA, valence=="pos")
  comp.valence.noA <- join(interp.valence, model.valence.pos.noA, by=c("imageID", "utterance"))
  r.valence <- with(comp.valence.noA, cor(value.corrected, probability))
  
  # by arousal
  model.arousal.noA <- aggregate(data=model.noA, probability ~ imageID + utterance + arousal, FUN=sum)
  model.arousal.high.noA <- subset(model.arousal.noA, arousal=="high")
  comp.arousal.noA <- join(interp.arousal, model.arousal.high.noA, by=c("imageID", "utterance"))
  r.arousal <- with(comp.arousal.noA, cor(value.corrected, probability))
  
  r <- mean(c(r.state, r.valence, r.arousal))
  d <- data.frame(name=f, cor.state=r.state, cor.valence=r.valence, cor.arousal=r.arousal, cor=r)
  models.noA <- rbind(models.noA, d)
}

models.noA <- models.noA[with(models.noA, order(-cor)), ]

bestName.noA <- as.character(subset(models.noA, cor==max(models.noA$cor))$name)

model.noArousal <- read.csv(paste("../model/parsedOutputsWithParams_noArousal/", bestName.noA, sep=""))
model.noArousal$utterance <- factor(model.noArousal$utterance, levels=c("terrible", "bad", "ok", "good", "amazing"),
                          labels=c("terrible", "bad", "neutral", "good", "amazing"))
model.noArousal$state <- factor(model.noArousal$state, levels=c("terrible", "bad", "neutral", "good", "amazing"))

#############
# State
#############
model.noArousal.state <- aggregate(data=model.noArousal, probability ~ imageID + utterance + state, FUN=sum)
colnames(model.noArousal.state)[4] <- "model"
model.noArousal.state <- join(model.noArousal.state, interp.states, by=c("state", "imageID", "utterance"))
with(model.noArousal.state, cor.test(model, prob))
ggplot(model.noArousal.state, aes(x=model, y=prob)) +
  geom_point() +
  theme_bw()

########
# Valence
########
model.noArousal.valence <- subset(aggregate(data=model.noArousal, probability ~ imageID + utterance + valence, FUN=sum),
                                  valence=="pos")
colnames(model.noArousal.valence)[4] <- "model"
model.noArousal.valence <- join(model.noArousal.valence, interp.valence, by=c("imageID", "utterance"))
with(model.noArousal.valence, cor.test(model, value.corrected))
ggplot(model.noArousal.valence, aes(x=model, y=value.corrected)) +
  geom_point() +
  theme_bw()

#########
# Arousal
#########
model.noArousal.arousal <- subset(aggregate(data=model.noArousal, probability ~ imageID + utterance + arousal, FUN=sum),
                                  arousal=="high")
colnames(model.noArousal.arousal)[4] <- "model"
model.noArousal.arousal <- join(model.noArousal.arousal, interp.arousal, by=c("imageID", "utterance"))
with(model.noArousal.arousal, cor.test(model, value.corrected))
ggplot(model.noArousal.arousal, aes(x=model, y=value.corrected)) +
  geom_point() +
  theme_bw()

######################################
# Visualize
######################################
#######
# State
#######
m.justPrior.state$type <- "prior"
m.literal.state$type <- "literal"
model.noArousal.state$type <- "just valence QUD"
model.all.state<- comp.state
model.all.state$imageID.utterance <- NULL
model.all.state$literal <- NULL
colnames(model.all.state)[5] <- "model"
model.all.state$type <- "all"

m.comparison.state <- rbind(m.justPrior.state, m.literal.state, model.noArousal.state, model.all.state)

ggplot(m.comparison.state, aes(x=state, y=model, color=type)) +
  geom_point() +
  geom_line(aes(group=type)) +
  facet_grid(imageID ~ utterance) +
  theme_bw()

########
# Valence
########
m.justPrior.valence$variable <- NULL
m.justPrior.valence$variable <- NULL
m.justPrior.valence$N <- NULL
m.justPrior.valence$sd <- NULL
m.justPrior.valence$se <- NULL
m.justPrior.valence$ci <- NULL
m.justPrior.valence$type <- "prior"

m.literal.valence$variable <- NULL
m.literal.valence$N <- NULL
m.literal.valence$N <- NULL
m.literal.valence$sd <- NULL
m.literal.valence$sd <- NULL
m.literal.valence$se <- NULL
m.literal.valence$se <- NULL
m.literal.valence$ci <- NULL
m.literal.valence$ci <- NULL
m.literal.valence$type <- "literal"

model.noArousal.valence$valence <- NULL
model.noArousal.valence$variable <- NULL
model.noArousal.valence$N <- NULL
model.noArousal.valence$se <- NULL
model.noArousal.valence$sd <- NULL
model.noArousal.valence$ci <- NULL
model.noArousal.valence$type <- "just valence QUD"

model.all.valence <- comp.valence
model.all.valence$variable <- NULL
model.all.valence$Comp.1 <- NULL
model.all.valence$N <- NULL
model.all.valence$sd <- NULL
model.all.valence$se <- NULL
model.all.valence$ci <- NULL
model.all.valence$valence <- NULL
model.all.valence$utteranceValence <- NULL
model.all.valence$interpValence <- NULL
model.all.valence$valenceFlip <- NULL
colnames(model.all.valence)[4] <- "model"
model.all.valence$type <- "all"

m.comparison.valence <- rbind(m.justPrior.valence, m.literal.valence, model.noArousal.valence, model.all.valence)

ggplot(m.comparison.valence, aes(x=type, y=model, color=type)) +
  #geom_point() +
  #geom_line(aes(group=type)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(imageID ~ utterance) +
  theme_bw()


m.comparison.valence.long <- m.comparison.valence
m.comparison.valence.long$value.corrected <- NULL
colnames(m.comparison.valence.long)[2] <- "probability"
interp.valence.simplified <- interp.valence
interp.valence.simplified$variable <- NULL
interp.valence.simplified$N <- NULL
interp.valence.simplified$sd <- NULL
interp.valence.simplified$se <- NULL
interp.valence.simplified$ci <- NULL
colnames(interp.valence.simplified)[3] <- "probability"
interp.valence.simplified$type <- "human"

m.comparison.valence.long <- rbind(m.comparison.valence.long, interp.valence.simplified)

ggplot(m.comparison.valence.long, aes(x=type, y=probability, fill=type)) +
  #geom_point() +
  #geom_line(aes(group=type)) +
  geom_bar(stat="identity", color="black") +
  facet_grid(imageID ~ utterance) +
  theme_bw()

