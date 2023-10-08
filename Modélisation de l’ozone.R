##### Exercice 11 #####
library(car)
library(leaps)
library(lmtest)

tab <- read.table("Données/ozone.txt", header = T, sep = " ")

tabbis=tab[,2:11]
plot(maxO3~., data = tabbis)

#Analyser le nuage de points et la corr´elation lin´eaire entre maxO3 et 
#chacune des variables quantitatives disponibles

cor=as.data.frame(cor(tab[,2:11],use ="pairwise.complete.obs"))

#Ajuster le mod`ele de regression lineaire expliquant maxO3 
#en fonction de toutes les variables quantitatives

reg = lm(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15, data = tabbis)
summary(reg)
bgtest(reg)

# Calculer les VIF (Variance Inflation Factor) pour chacune 
# des variables explicatives du modele precedent

vif(reg)

# Ajustement des nouveaux modeles

reg2 = lm(maxO3~T9+Ne12+Vx9)
summary(reg2)
vif(reg2)

BIC(reg)
BIC(reg2)


regs=regsubsets(maxO3~.,data=tabbis)
plot(regs,scale="bic")
plot(regs,scale="adjr2")
plot(regs,scale="Cp")

summary(regs)
summary(regs)$bic
summary(regs)$which

n=nrow(na.omit(tab))
step(reg, direction = "both") #both
step(reg, direction = "backward") #backward
step(reg, k = log(n)) #backward BIC

min.model=lm(maxO3~1, data = tabbis)
biggest=formula(reg)
step(min.model, direction = "forward",scope = biggest) #forward
step(min.model, direction = "forward",scope = biggest, k = log(n)) #forward


reg3=lm(maxO3~T12+Ne9+Vx9, data = tabbis)
summary(reg3)
vif(reg3)

BIC(reg2)
BIC(reg3)

AIC(reg2)
AIC(reg3)


# Analyse des résidus
plot(reg3)

# Homoscédasticité
bptest(reg3)

# Non corrélation des résidus (erreurs)
bgtest(reg3)
dwtest(reg3)

# ajout de la valeur de la veille 

tabbis$maxO3v=c(NA,tabbis$maxO3[1:121])
plot(maxO3~maxO3v, data = tabbis)
cor.test(tabbis$maxO3,tabbis$maxO3v)

#Ajustement les nouveaux modeles

regs=regsubsets(maxO3~.,data=tabbis, nvmax = 10)
plot(regs,scale="bic")

summary(regs)
summary(regs)$bic
summary(regs)$which

reg4=lm(maxO3~T12+Ne9+maxO3v, data = tabbis)
summary(reg4)

BIC(reg3)
BIC(reg4)

# Analyse des résidus
plot(reg4)

# Homoscédasticité
bptest(reg4)

# Non corrélation des résidus (erreurs)
bgtest(reg4)
dwtest(reg4)





