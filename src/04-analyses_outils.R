#######################
# analyses_outils.R  #
#######################

#Je charge les scripts utilisés:
source ("src/objects_outils.R") #charge les librairies et les objets
source ("src/functions_outils.R") #charge les fonctions utilisées


################
#  QUESION 1   #
################



#description de la base hdrs:

#nb de visites
a <- dwh
b <- apply(dwh[,grep("tot",colnames(dwh))],2,function(x)!is.na(x))
a$nvisites <- apply(b,1,sum)
qplot(as.factor(a[,"nvisites"]),xlab="Nombre de visites effectuées", ylab="Nombre de patients", fill=I("slategray2"), col=I("lavenderblush4"))

#nb de patients à J56
dl %>% filter(time==56 & !is.na(HAMD1)) %>% count



#1/valider hamilton au temps t=0

summary(dl0)
summary(sc0)

#A- analyse d'items: 

#A1-diagramme en baton
pl <- lapply(colnames(dl0)[-1], function(.x) qplot(as.factor(dl0[,.x]),xlab=NULL, main=paste("plot", .x),fill=I("navajowhite3"), col=I("pink4")))
ml <- marrangeGrob(pl,ncol=4,nrow=5,top = NULL)
print(ml)

#A2- nb de NA
#qplot aurait affiché les NA s'il y en avait, mais 2 vérifications vallent mieux qu'une
table(apply(apply(dl0[,grep("HAM",colnames(dl0))],2,is.na),1,sum))

#A3- corrélation entre items
couplesup0.2 <- get_items_correlation (dl0)
kable(couplesup0.2[order(abs(couplesup0.2[3])), ]) 

#B- structure dimensionnelle
#B1- diagramme des valeurs propres
scree.plot(dl0[,-1], simu=20)
scree.plot(dl0[,-1], type="R", simu=20)

#B2-Analyse factorielle
fit.3 <- factanal(dl0[, -1], factors = 3, rotation = "varimax")
print(fit.3, digits=2, cutoff=.1,sort=F)
fit.2 <- factanal(dl0[, -1], factors = 2, rotation = "varimax")
print(fit.2,digits=2, cutoff=.1,sort=F) 
fit.1 <-factanal(dl0[, -1], factors = 1, rotation = "varimax")
print(fit.1,digits=2, cutoff=.1,sort=F) #fait une rotation varimax automatiquement


#C- cohérence interne
#Je ne fais qu'un seul cronbach car le score est calculé par la somme de tous les items
set.seed(123) #pour avoir un résultat reproductible
BootCronCi(dl0,100)

#D- validité concourrante et divergente

#Description des données manquantes pour sc0

sclbis <- read.csv2("data/SCL90.csv")
for (i in colnames(sclbis)[grep("Q",colnames(sclbis))]){
  sclbis[,i] <- as.numeric(as.character(sclbis[ ,i])) #"NAs introduce by coercion" car il ya des ND et des ""
  #Je change les valeurs aberrantes (superieures à 4) en NA
  sclbis[,i] <- ifelse (sclbis[,i] > 4, NA, sclbis[,i])
}
sclbis <- sclbis %>% filter (VISIT=="J0")
table(apply(sclbis[ ,grep("Q",colnames(sclbis))],1,sum),useNA = "a") #112 questionnaires complets
table(apply(apply(sclbis[ ,grep("Q",colnames(sclbis))],2,is.na),1,sum),useNA = "a") #moins de 12 items manquants par patient

#combiens de questions remplies par combien de patients
table(apply(apply(sc0[,grep("Q",colnames(sc0))],2,is.na),1,sum))

#calcul du score global de Hamilton
dl0$global <- apply(dl0[,-1],1,sum) #de 0 à 52

#corrélation entre score de hamilton et sous score de scl90:
a <- round(cor(dl0$global,sc0[ ,dimensions],use="complete.obs"),2)
kable(data.frame(dimensions = dimensions[order(abs(as.numeric(a)),decreasing = T)], correlation = as.numeric(a)[order(abs(as.numeric(a)),decreasing = T)]))
#le score de hamilton corrèle le plus avec dimension 1(somatisation), 2(symptomes obsessionels), 4(dépression)

#ACP focalisée:
dlsc0 <- merge(dl0[,c("NUMERO","global")], sc0[ ,c("NUMERO",dimensions)],by="NUMERO",all.x=T,all.y=F)
dlsc0$Hamilton <- dlsc0$global
fpca(y="Hamilton",x=dimensions,data=dlsc0)



#valider hamilton au temps t=56


summary(dl56)
summary(sc56)

#A- analyse d'items: 

#A1-diagramme en baton
pl <- lapply(colnames(dl56)[-1], function(.x) qplot(as.factor(dl56[,.x]),xlab=NULL, main=paste("plot", .x),fill=I("navajowhite3"), col=I("pink4")))
ml <- marrangeGrob(pl,ncol=2,nrow=3,top = NULL)
print(ml)

#A2- distribution des NA selon les sujets
table(apply(apply(dl56[,grep("HAM",colnames(dl56))],2,is.na),1,sum))
#120 patients ont une échelle complète, 26 patients ont une échelle non remplie (NA aux 17 items)

#A3- corrélation entre items
couplesup0.2 <- get_items_correlation(dl56)
kable(couplesup0.2[order(abs(couplesup0.2[3])), ]) 

#B- structure dimensionnelle
#B1- diagramme des valeurs propres
scree.plot(dl56[,-1], simu=20)
scree.plot(dl56[,-1], type="R", simu=20)

#B2-Analyse factorielle
fit.3 <- factanal(dl56[!is.na(dl56$HAMD1), -1], factors = 3, rotation = "varimax") #retire les NA, si 1 item NA , tous NA comme vu plus haut
print(fit.3, digits=2, cutoff=.1,sort=F)
fit.2 <- factanal(dl56[!is.na(dl56$HAMD1), -1], factors = 2, rotation = "varimax")
print(fit.2,digits=2, cutoff=.1,sort=F) 
fit.1 <-factanal(dl56[!is.na(dl56$HAMD1), -1], factors = 1, rotation = "varimax")
print(fit.1,digits=2, cutoff=.1,sort=F) 

#C- cohérence interne
set.seed(1234) #pour avoir un résultat reproductible
BootCronCi(dl56,100)

#D- validité concourrante et divergente

#calcul du score global de Hamilton
dl56$global <- apply(dl56[,-1],1,sum) #de 0 à 52

#Description des données manquantes pour sc56
sclbis <- read.csv2("data/SCL90.csv")
for (i in colnames(sclbis)[grep("Q",colnames(sclbis))]){
  sclbis[,i] <- as.numeric(as.character(sclbis[ ,i])) #"NAs introduce by coercion" car il ya des ND et des ""
  #Je change les valeurs aberrantes (superieures à 4) en NA
  sclbis[,i] <- ifelse (sclbis[,i] > 4, NA, sclbis[,i])
}
sclbis <- sclbis %>% filter (VISIT=="J56")
table(apply(sclbis[ ,grep("Q",colnames(sclbis))],1,sum),useNA = "a") #112 questionnaires complets
table(apply(apply(sclbis[ ,grep("Q",colnames(sclbis))],2,is.na),1,sum),useNA = "a") #moins de 12 items manquants par patient

#corrélation entre score de hamilton et sous score de scl90:
a <- round(cor(dl56$global,sc56[ ,dimensions],use="complete.obs"),2) #j'ai bien fait attention à ce que les numero de dl56 et sc56 soit dans le meme ordre
kable(data.frame(dimensions = dimensions[order(abs(as.numeric(a)),decreasing = T)], correlation = as.numeric(a)[order(abs(as.numeric(a)),decreasing = T)]))
#le score de hamilton corrèle le plus avec dépression

#ACP focalisée: 
dlsc56 <- merge(dl56[,c("NUMERO","global")], sc56[ ,c("NUMERO",dimensions)],by="NUMERO",all.x=T,all.y=F)
dlsc56$Hamilton <- dlsc56$global
fpca(y="Hamilton",x=dimensions,data=dlsc56)



################
#  QUESION 2   #
################

#1- Imputation par LOCF
table(is.na(h0$tot)) #tous les score sont calculés à h0

#A-LOCF

# utilisation du fichier dwh

# > head(dwh)
# NUMERO GROUPE tot_0 tot_4 tot_7 tot_14 tot_21 tot_28 tot_42 tot_56
#      0      1    29    29    29     29     29     29     29     29
#      3      1    25    27    30     30     30     30     30     30
#      4      1    28    24    21     20     17     12     12      5
#      7      0    27    18    15     13     12      8      5      3
#      8      0    25    21    18     13     12      8      6      4
#      9      0    26    28    26     23     21     17      9      9

#NUMERO = identifiant patient
#tot_0 à 56 : score de hamilton aux temps 0 à 56

#Création de la fonction getLOCF 
#cf script "functions_outils.R"
#creation d'une variable last qui prend la dernière valeur connue du score
dwh$last <- apply(dwh[,grep("tot_",colnames(dwh))],1,getLOCF)
#creation d'une variable qui fait la différence entre le premier score et le dernier connu
dwh$diff_56_0 <- dwh$last - dwh$tot_0 

#B-visualisation graphique
ggplot(dwh, aes(y = diff_56_0, x=GROUPE)) +
  stat_boxplot(geom = "errorbar", width = 0.3, color = "lightsteelblue4") +
  #geom_boxplot(width = 0.5, fill = c("lightsteelblue2", color = "lightsteelblue4") +
  geom_boxplot(width = 0.5, fill = c("#E69F00","#009E73"), color = c("lightsteelblue4","grey")) +
  scale_x_discrete() + xlab("Groupe") + ylab("Différence de score entre J56 et J0") +ggtitle ("Diiférence de score entre J56 et J0 selon les groupes")

#C-COMPARAISON

#méthode 1 : test de student
#Test de student
t.test (dwh[dwh$GROUPE=="0","diff_56_0"],dwh[dwh$GROUPE=="1","diff_56_0"])
#normalité de la différence de score
qplot(dwh$diff_56_0, xlab="Différence de score entre J56 et J0", fill=I("seashell3"), col=I("pink4"),bins=10)
#égalité des variances
by(dwh$diff_56_0,dwh$GROUPE,sd,na.rm=T)

#méthode 2 :régression lineaire (non utilisé dans le manuscrit)
#Régression linéaire
summary(moddiff <- lm(diff_56_0~GROUPE, dwh))
#normalité des résidus
hist(residuals(moddiff))


#2- modele mixte :

# utilisation du fichier dlh
 
# > head(dlh)
# NUMERO GROUPE time tot
#      0      1    0  29
#      3      1    0  25
#      4      1    0  28
#      7      0    0  27
#      8      0    0  25
#      9      0    0  26

#NUMERO: identifiant patient
#time : temps de la visite 0=J0 4=J4 etc... 
#tot : score de Hamilton

#Création du modele mixte
#recherche d'interaction
summary(mod1 <- lm(tot~time*GROUPE+NUMERO,dlh)) #interaction significative
#ecriture du modele mixte et
#lecture la t value de la ligne time:GROUPE1 c'est la différence de pente
summary(mod2 <- lmer(tot~time*GROUPE+(1|NUMERO),dlh))

#Je vérifie la normalité des résidus
qplot(resid(mod2), xlab="résidus du modèle mixte mod2", fill=I("seashell3"), col=I("pink4"), binwidth=2)


#interpretation de la t value
#approximation du nombre de degrés de libertés : fonction get_ddf_Lb du package pbkrtest
df.KR <- get_ddf_Lb(mod2, fixef(mod2)) #require(pbkrtest)
coefs <- data.frame(coef(summary(mod2)))
#lire la pvalue à partir de la distribution de student avec le nombre de degrés de liberté df.KR ddl 
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))
#ou alors lire la pvalue dans la distribution normale, par approximation
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
#tableau final:
coefs[4,] 





################
#  QUESION 3   #
################

#Utilisation de la table dws (cf script data_management.R)

# > head(dws)
# NUMERO GROUPE evt time
#      0      1   0    0
#      3      1   0    7
#      4      1   1   28
#      7      0   1   14
#      8      0   1   21
#      9      0   1   42

#NUMERO= identifiant patient
#evt=diminution d'au moins 50 % du score de hamilton
#time = délai jusqu'à l'évènement ou jusqu'aux dernières nouvelles



#1- Analyses preliminaires 

#Nombre d'évènements par groupe
table(dws$evt,dws$GROUPE,deparse.level = 2)
#nombre d'évènements à chaque visite
table(dws$time[dws$evt==1])

#suivi global
suiv <- survfit(Surv(dws$time,1-dws$evt)~1)
#plot du suivi global (non utilisé)
plot(suiv,xscale=1, yscale= 100, xlab="Durée (jours)",xaxt = "n")
axis(1, at = levels(as.factor(dws$time)), cex.axis = 1) 
#mediane de suivi
med <- min(suiv$time[suiv$surv<=0.5])

dws %>% filter(time==56 & evt==1)#10 evenement à J56

#survie globale
surv <- survfit(Surv(dws$time,dws$evt)~1, conf.int=.95)
#plot de la survie globale (non utilisé)
plot(surv,xscale=1, yscale= 100, xlab="Durée (jours)",xaxt = "n")
axis(1, at = levels(as.factor(dws$time)), cex.axis = 1)
#mediane de survie
med1 <- min(surv$time[surv$surv<=0.5])



#2- Comparaison de la survie entre les 2 groupes

#A - Courbes de survie
KMcurv <- survfit(Surv(time,evt)~GROUPE, data = dws)
g <- ggsurv(KMcurv, surv.col=c("#E69F00", "#009E73"),size.est=1,cens.col="black") + 
  scale_x_continuous(breaks=time) + #permet d'avoir J0 J4 J7 etc... au lieu du nombre de jour de 0 à 50
  scale_y_continuous(labels=percent) + #pour avoir la survie en %
  labs(title =NULL, x = "Temps après premier questionnaire, jours", y = "Patients non répondeurs, %")  +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12))
g

#Abis- Tables de survie
at_risk <- summary(KMcurv)
dfrisk <- data.frame(at_risk[c(2:5,7)])
dfrisk$surv <- paste(round(dfrisk$surv,2)*100,"%")

dfrisk0 <- dfrisk[dfrisk$strata=="GROUPE=0",-5]
dfrisk0tab <- data.frame(t(dfrisk0))[-1,]
colnames(dfrisk0tab) <- paste0("J",dfrisk0$time)
dfrisk0tab$J0 <- c(length(unique(dws$NUMERO[dws$GROUPE==0])), 0, "100%")
dfrisk0tab <- dfrisk0tab[,c(8,1:7)]
kable(dfrisk0tab)

dfrisk1 <- dfrisk[dfrisk$strata=="GROUPE=1",-5]
dfrisk1tab <- data.frame(t(dfrisk1))[-1,]
colnames(dfrisk1tab) <- paste0("J",dfrisk1$time)
dfrisk1tab$J0 <- c(length(unique(dws$NUMERO[dws$GROUPE==1])), 0, "100%")
dfrisk1tab$J4 <- c(length(unique(dws$NUMERO[dws$GROUPE==1])), 0, "100%")
dfrisk1tab <- dfrisk1tab[c(7:8,1:6)]
kable(dfrisk1tab)


#B- Tests
#On peut utiliser un test du log rank qui est non paramétrique et ne fait donc pas d'hypothèse sur la 
#distribution de la survie. Comme la comparaison se fait entre 2 groupes, on pourra calculer également le HR.

#B1-Test du logrank (non présenté dans le manuscrit)
sdf <- survdiff(Surv(dws$time,dws$evt)~dws$GROUPE)
p.val <- round(1 - pchisq(sdf$chisq, length(sdf$n) - 1),5) #2e-05


#B2- modèle de Cox et test du score (équivalent du log rank)

modcox <- coxph(Surv(dws$time,dws$evt)~dws$GROUPE)
#conditions :

#1:risques proportionnels
a <- cox.zph(modcox) 
#pvalue:
a$table[3]
#non significatif mais peut être manque de puissance => je trace une courbe
plot(a,main="GROUPE")#J'ai essayé de transformer en ggplot2 mais je n'y parviens pas.
#la courbe est à peu près horizontale, j'en conclue donc que la condition est vérifiée

#2:Loglinéarité : ?

#comparaison
b<-summary(modcox)
res <- round(as.numeric(b$sctest[3]),3)

#C- Hazard ratio
#à partir de la table du log rank
HR <- round((sdf$obs[2]/sdf$exp[2])/(sdf$obs[1]/sdf$exp[1]),2)
up95 <- round(exp(log(HR) + qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1])),2)
low95 <- round(exp(log(HR) - qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1])),2)
paste0(HR," [",low95,"-",up95,"]")
#à partir du modèle de cox
HR <- round(exp(coef(modcox)),2)
low95 <- round(exp(confint(modcox)),2)[1]
up95 <-round(exp(confint(modcox)),2)[2]
paste0(HR," [",low95,"-",up95,"]")


