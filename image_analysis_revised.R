#library(cjoint)
library(cregg)
library(Amelia)
library(ggplot2)
library(ggtext)
library(dotwhisker)
library(fmsb)
library(stargazer)
library(broom)

#source("conjoint_wrangle.R")
cjframe<-read.csv("conjoint_frame.csv",encoding="UTF-8")
dat<-read.csv("experience_frame.csv",encoding="UTF-8")

################ Differences in Parameter Estimates ###########################

basictheme<-theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=19),
        axis.text.y=element_markdown(size=15),
        axis.title = element_text(size=15),
        legend.position="none")

#unconditional binary outcomes, AMCE
m1c<-cj(data=cjframe[!is.na(cjframe$binchoice),],
       binchoice~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id,
       by=~image,
       estimate="mm_difference")

m1d<-cj(data=cjframe[!is.na(cjframe$binchoice),],
        binchoice~prof+crime+race+sex+politician+econ+crime+slogan,
        id=~id,
        by=~image,
        estimate="mm")

p1<-plot(m1c[m1c$feature!="model",])+
  basictheme+
  scale_color_manual(values=rep("black",22))+
  ggtitle("Difference in MM on Vote Choice\nBetween Image and Text Treatment")+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=.5)+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","Corruption","Crime","Employment",
                            "**(Slogan)**\n","Right",
                            "Left","**(Economic Policy)**","Political Novice",
                            "Incumbent",
                            "**(Political Experience)**","Male","Female",
                            "**(Sex)**","Black",
                            "Pardo","White","**(Race)**","Video Surveillance",
                            "Prevention","Iron Fist",
                            "**(Security Proposal)**",
                            "Business Owner","Soldier","Doctor","Pastor",
                            "Police","Professor","**(Profession)**"))+
  theme(axis.title.y=element_blank())+
  geom_vline(xintercept=0)+
  xlim(-.13,.13)

png(filename="vote_choice_mm_diff.png",
    width=500,height=500)
p1
dev.off()

################# Omnibus Tests #################################

omni1<-cj_anova(data=cjframe,binchoice~prof+crime+race+sex+politician+econ+
                  crime+slogan,
                id=~id,
                by=~image)

################# Survey Experience #############################

#Ease of use
hard1<-glm(data=dat,hard~image,
           family="binomial")

hard2<-glm(data=dat,hard~image+age+sex+income_ord+education_ord,
           family="binomial")

#Just ATE
stargazer(hard1,hard2,
          covariate.labels = c("Conjoint Medium (Reference=Table)",
                               "Age (Years)",
                               "Sex (Reference=Female)",
                               "Income",
                               "Education"),
          keep=c("image"),
          column.labels = c("Unadjusted","Adjusted"),
          dep.var.labels.include = FALSE,
          style="apsr",
          type="latex")

#Full Model
stargazer(hard1,hard2,
          covariate.labels = c("Conjoint Medium (Reference=Table)",
                               "Age (Years)",
                               "Sex (Reference=Female)",
                               "Income",
                               "Education"),
          style="apsr",
          dep.var.labels.include = FALSE,
          type="latex")

#Predict delta p
newdat<-data.frame("image"=c(0,1),
                   "age"=rep(mean(dat$age,na.rm=TRUE),2),
                   "sex"=names(sort(table(dat$sex),
                                    decreasing = TRUE)[1]),
                   "income_ord"=as.numeric(names(sort(table(dat$income_ord),
                                           decreasing = TRUE)[1])),
                   "education_ord"=as.numeric(names(sort(table(dat$education_ord),
                                              decreasing = TRUE)[1])))

predshard1<-predict(hard1,newdat,type="response",se.fit=TRUE)
predshard2<-predict(hard2,newdat,type="response",se.fit=TRUE)

#Nonresponse
nr1<-glm(data=dat,nonresponse_choice_count~image,
         family="poisson")

nr2<-glm(data=dat,nonresponse_choice_count~image+age+sex+income_ord+education_ord,
         family="poisson")

#Just ATE
stargazer(nr1,nr2,
          covariate.labels = c("Conjoint Medium (Reference=Table)",
                               "Age (Years)",
                               "Sex (Reference=Female)",
                               "Income",
                               "Education"),
          keep=c("image"),
          column.labels = c("Unadjusted","Adjusted"),
          dep.var.labels.include = FALSE,
          style="apsr",
          type="latex")

#Full Model
stargazer(nr1,nr2,
          covariate.labels = c("Conjoint Medium (Reference=Table)",
                               "Age (Years)",
                               "Sex (Reference=Female)",
                               "Income",
                               "Education"),
          style="apsr",
          dep.var.labels.include = FALSE,
          type="latex")

#Predict change in count
predsnr<-predict(nr2,newdat,type="response",se.fit=TRUE)

################# Irrelevant Features ###########################
noise1<-cj(data=cjframe[(!is.na(cjframe$binchoice)&cjframe$image==1),],
           binchoice~racesex,
           id=~id,
           by=~deck,
           estimate="amce")

pnoise<-plot(noise1,group="deck")+
  scale_color_grey(na.translate=FALSE,
                   labels=c("Deck 1","Deck 2","Deck 3"))+
  theme_minimal()+
  geom_errorbarh(aes(xmin=lower,xmax=upper),
                 position=position_dodge(width=.75),
                 height=.5)+
  guides(color=guide_legend(title="Photo Deck"))+
  ggtitle("Marginal Mean for Individual Models Representing Each Demographic Group")+
  scale_y_discrete(name="Feature",
                   labels=c("Pardo Male","Black Female","Black Male",
                            "Pardo Female","White Female","White Male",
                            "(Demographic Group)"))+
  theme(axis.title.y=element_blank())+
  geom_vline(xintercept=0.5)

png(filename="irrelevant.png",
    width=500,height=500)
pnoise
dev.off()

noise2<-cj(data=cjframe[(!is.na(cjframe$binchoice)&cjframe$image==1),],
           binchoice~prof+crime+race+sex+politician+econ+crime+slogan+style,
           id=~id,
           estimate="amce")

pnoise<-plot(noise2[noise2$feature=="style",])+
  basictheme+
  scale_color_manual(values=rep("black",22))+
  guides(color=guide_legend(title="Photo Deck"))+
  ggtitle("AMCE for Santinho Style")+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=.5)+
  scale_y_discrete(name="Feature",
                   labels=c("One (Reference)","Two","Three","Four","Five","Six"))+
  theme(axis.title.y=element_blank())+
  geom_vline(xintercept=0)

png(filename="style_effect.png",
    width=500,height=500)
pnoise
dev.off()


