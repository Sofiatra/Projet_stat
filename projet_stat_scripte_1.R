data=read.table(file="C:/Users/PC/Downloads/data.txt",
                header=TRUE,stringsAsFactors = TRUE)
data$Age ; data$Genre;data$Bilirubine;data$Alkphos
barplot(height = table(data$Genre),xlab = "genre du patient",ylab = "nombre de patients",main = "Representation par genre des patients", col="maroon",las=1)
sd(data$Bilirubine)
var(data$Bilirubine)
barplot(height=table(data$Statut_Foie), xlab="statut du foie", ylab="Nombre de patients", main="Representation par le statut du fois", col="#1d1d86",las=1)

?barplot
# Données fictives
donnees1 <-table(data$Statut_Foie[data$Genre=="Femme"])
table(data$Genre[data$Statut_Foie=="Sain"])

donnees2 <-table(data$Statut_Foie[data$Genre=="Homme"])
table(data$Genre[data$Statut_Foie=="Malade"])
categories <- c("Femme", "Homme")
table(data$Sgot[data$Genre=="Femme"]&data$Statut_Foie=="Sain")
# Création des bar plots côte à côte
barplot(matrix(c(donnees1, donnees2), nrow = 2, byrow = TRUE),
        beside = TRUE,
        names.arg = categories,
        legend.text = c("Malade", "Sain"),
        args.legend = list(x = "topleft", bty = "n"),
        ylim=c(0,80),
        col = c("#1d1d86", "maroon"),
        xlab = "Genre des patients",
        ylab = "nombre de patients",
        main = "Comparaison de la statut du foie
        par rapport au Genre des patients")
text(x = barplot(matrix(c(donnees1, donnees2), nrow = 2, byrow = TRUE),
                 beside = TRUE,
                 col = c("#1d1d86", "maroon"),
                 names.arg = categories,
                 plot = FALSE),
     y = c(donnees1, donnees2) + 1,   # Ajuste la position verticale des étiquettes
     label = c(donnees1, donnees2),
     pos = 3,           # 3 pour positionner au-dessus de la barre
     cex = 1,         # Ajuste la taille des étiquettes
     col = "black")     # Ajuste la couleur de mes étiquettes







# Boxplot pour visualiser la comparaison
#boxplot(list(Groupe1 = donnees1, Groupe2 = donnees2), col = c("blue", "red"), names = c("Groupe 1", "Groupe 2"))
#hist(data$Sgot[data$Genre=="Femme"&data$Statut_Foie])
#hist(data$Sgot[data$Genre=="Femme"&data$Statut_Foie=="Malade"])
#hist(data$Sgot[data$Genre=="Femme"])
#summary(data$Sgot[data$Genre=="Femme"&data$Statut_Foie=="Sain"])
#summary(data$Sgot[data$Genre=="Femme"&data$Statut_Foie=="Malade"])
#hist(data$Sgot[data$Genre=="Homme"])
hist(x=data$Sgot,ylab="nombre de patients",
     xlab="mesure du taux d'aspartate aminotransferase dans le sang",
     main = "Comparaison des valeurs du Sgot chez des patients Sain")

hist(data$Sgot[data$Genre=="Femme"&data$Statut_Foie=="Sain"],
     ylab="nombre de patients",
        xlab="mesure du taux d'aspartate aminotransferase dans le sang",
     col="cornflowerblue",
     main = "Comparaison des valeurs du Sgot chez des femmes avec un foie Sain")
hist(data$Sgot[data$Genre=="Femme"&data$Statut_Foie=="Malade"],
     ylab="nombre de patients",
     xlab="mesure du taux d'aspartate aminotransferase dans le sang,femme,sain",
     col="pink",
     main = "Comparaison des valeurs du Sgot chez des femmes avec un foie Malade")
hist(x=data$Sgot[data$Genre=="Femme"&data$Statut_Foie=="Malade"],
     ylab="nombre de patients",
     xlab="taux d'aspartate aminotransferase dans le sang",
     main = "Comparaison des valeurs du Sgot chez des patients Malade")
hist(data$Sgot[data$Genre=="Homme"&data$Statut_Foie=="Sain"],
     ylab="nombre de patients",
     xlab="mesure du taux d'aspartate aminotransferase dans le sang",
     col="green",
     main = "Comparaison des valeurs du Sgot chez des Homme avec un foie Sain")
hist(data$Sgot[data$Genre=="Homme"&data$Statut_Foie=="Malade"],
     ylab="nombre de patients",
     xlab="mesure du taux d'aspartate aminotransferase dans le sang",
     col="red",
     main = "Comparaison des valeurs du Sgot chez des Homme avec un foie Malade")
data$Sgot[data$Sgot&data$Statut_Foie=="Sain"]
table(data$Sgot[data$Sgot&data$Statut_Foie=="Sain"])
table(data$Sgot[data$Sgot&data$Statut_Foie=="Malade"])
hist(x=table(data$Sgot[data$Sgot&data$Statut_Foie=="Sain"]),
     xlab="taux d'aspartate aminotransferase dans le sang",
     ylab="nombre de patients",col="cornflowerblue",
     main = "taux d'aspartate aminotransferase dans le sang par des patients avec un foie sain ")
hist(x=data$Proteines[data$Genre],
     xlab="taux total de proteines dans le sang",
     ylab="nombre de patients",
     col="yellow",
     main = "taux total de proteine dans le sang par des patients avec un foie sain")
#hist(data$age[data$Proteines])
hist(data$Age[data$Statut_Foie=="Sain"],
     xlab="Age des patients",
     ylab="nombre de patients",
     col="blueviolet",
     main = "l'age des patients avec un foie Sain")
hist(data$Age[data$Statut_Foie=="Malade"],
     xlab="Age des patients",
     ylab="nombre de patients",
     col="slategray4",
     main = "l'age des patients avec un foie Malade")
boxplot(data$Age~data$Statut_Foie,
        xlab="Age des patients",
        ylab="nombre de patients",
        col=c("cyan","cyan4"),
        main = "repartition de l'age des patients par l'etat du foie")
boxplot(data$Sgot~data$Statut_Foie,
        ylab="nombre de patients",
        xlab="taux d'aspartate aminotransferase dans le sang",
        col=c("chartreuse","chartreuse4"),
        main = "Dosage du taux d'aspartate aminotransferase dans le sang par l'etat du foie")
boxplot(data$Proteines~data$Genre,
        ylab="nombre de patients",
        xlab="taux total de proteines dans le sang",
        col=c("antiquewhite","antiquewhite4"),
        main = "Dosage du taux total de proteines dans 
        le sang selon genre des patients")
summary(data)
library(ggplot2)
# Données testes
don1 <- data$Bilirubine[data$Genre=="Femme"&data$Statut_Foie=="Malade"]
don2 <- data$Alkphos[data$Genre=="Homme"&data$Statut_Foie=="Malade"]
don3 <- data$Sgpt[data$Genre=="Homme"&data$Statut_Foie=="Malade"]
don4 <- data$Sgot[data$Genre=="Homme"&data$Statut_Foie=="Malade"]
don5 <- data$Proteines[data$Genre=="Femme"&data$Statut_Foie=="Malade"]
# Création d'un boxplot
boxplot(list(Sgpt = don3,Sgot=don4),ylim = c(0,600),
        col = c("red", "pink"),
        main = "Comparaison de Sgpt et Sgot
        chez des Homme avec un foie Malade",
        xlab = "les marqueurs dosés",
        ylab = "Valeurs")

do1 <- data$Bilirubine[data$Genre=="Homme"&data$Statut_Foie=="Malade"]
do2 <- data$Alkphos[data$Genre=="Homme"&data$Statut_Foie=="Malade"]
do3 <- data$Sgpt[data$Genre=="Homme"&data$Statut_Foie=="Malade"]
do4 <- data$Sgot[data$Genre=="Homme"&data$Statut_Foie=="Malade"]
do5 <- data$Proteines[data$Genre=="Homme"&data$Statut_Foie=="Malade"]

# Création d'un boxplot
boxplot(list(Alkphos = do2, Sgpt = do3,Sgot=do4),
        col = c("blue", "red", "green","cyan"),
        main = "Comparaison des proteines chez des patients Masculins avec un foie Malade",
        xlab = "Proteines",
        ylab = "Valeurs")

boxplot(list(Sain = data$Bilirubine[data$Genre=="Femme"&data$Statut_Foie=="Sain"], malade = data$Bilirubine[data$Genre=="Femme"&data$Statut_Foie=="Malade"]),
        col = c("yellow","#c72c48"),
        ylim=c(0,20),
        main = "Variation de l'Alkphos chez nos patients femme
        avec distinction de l'état du foie",
        xlab = "statut du foie",
        ylab = "taux de Alkphos")
boxplot(list(Sain = data$Alkphos[data$Genre=="Homme"&data$Statut_Foie=="Sain"], malade = data$Alkphos[data$Genre=="Homme"&data$Statut_Foie=="Malade"]),
        col = c("yellow","#c72c48"),
        main = "Variation de l'Alkphos chez nos patients homme
        avec distinction de l'état du foie",
        xlab = "statut du foie",
        ylab = "taux de Alkphos")

var(data$Bilirubine[data$Genre=="Femme"&data$Statut_Foie=="Sain"])
summary(data$Bilirubine[data$Genre=="Homme"])
sd(data$Bilirubine[data$Genre=="Homme"&data$Statut_Foie=="Sain"])
sd(data$Bilirubine[data$Genre=="Homme"&data$Statut_Foie=="Malade"])
summary(data$Bilirubine[data$Genre=="Homme"&data$Statut_Foie=="Sain"])
summary(data$Bilirubine[data$Genre=="Homme"&data$Statut_Foie=="Malade"])



mb1=mean(data$Bilirubine[data$Age_classe=="10-35"&data$Genre=="Femme"])
mb2=mean(data$Bilirubine[data$Age_classe=="35-60"&data$Genre=="Femme"])
mb3=mean(data$Bilirubine[data$Age_classe=="60-85"&data$Genre=="Femme"])
means <- c(mb1, mb2, mb3)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes, 
        col = "mediumvioletred",
        xlab = "Tranche d'âge", ylab = "Moyenne de la bilirubine",
        main = "Moyenne de la bilirubine par tranche d'âge chez les femmes")
mb4=mean(data$Bilirubine[data$Age_classe=="10-35"&data$Genre=="Homme"])
mb5=mean(data$Bilirubine[data$Age_classe=="35-60"&data$Genre=="Homme"])
mb6=mean(data$Bilirubine[data$Age_classe=="60-85"&data$Genre=="Homme"])
means <- c(mb4, mb5, mb6)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes, 
        col = "navyblue",
        ylim = c(0,5),
        xlab = "Tranche d'âge", ylab = "Moyenne de la bilirubine",
        main = "Moyenne de la bilirubine par tranche d'âge chez les hommes")

mS1=mean(data$Sgpt[data$Age_classe=="10-35"&data$Genre=="Femme"])
mS2=mean(data$Sgpt[data$Age_classe=="35-60"&data$Genre=="Femme"])
mS3=mean(data$Sgpt[data$Age_classe=="60-85"&data$Genre=="Femme"])
means <- c(mS1, mS2, mS3)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes,
        ylim = c(0,60),
        col = "mediumvioletred",
        xlab = "Tranche d'âge", ylab = "Moyenne de Sgpt",
        main = "Moyenne de Sgpt par tranche d'âge chez les femmes")

mS4=mean(data$Sgpt[data$Age_classe=="10-35"&data$Genre=="Homme"])
mS5=mean(data$Sgpt[data$Age_classe=="35-60"&data$Genre=="Homme"])
mS6=mean(data$Sgpt[data$Age_classe=="60-85"&data$Genre=="Homme"])
means <- c(mS4, mS5, mS6)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes,
        ylim = c(0,120),
        col = "navyblue",
        xlab = "Tranche d'âge", ylab = "Moyenne de Sgpt",
        main = "Moyenne de Sgpt par tranche d'âge chez les hommes")

mO1=mean(data$Sgot[data$Age_classe=="10-35"&data$Genre=="Femme"])
mO2=mean(data$Sgot[data$Age_classe=="35-60"&data$Genre=="Femme"])
mO3=mean(data$Sgot[data$Age_classe=="60-85"&data$Genre=="Femme"])
means <- c(mO1, mO2, mO3)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes, 
        ylim = c(0,60),
        col = "mediumvioletred",
        xlab = "Tranche d'âge", ylab = "Moyenne de Sgot",
        main = "Moyenne de Sgot par tranche d'âge chez les femmes")

mO4=mean(data$Sgot[data$Age_classe=="10-35"&data$Genre=="Homme"])
mO5=mean(data$Sgot[data$Age_classe=="35-60"&data$Genre=="Homme"])
mO6=mean(data$Sgot[data$Age_classe=="60-85"&data$Genre=="Homme"])
means <- c(mS4, mS5, mS6)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes,
        ylim = c(0,120),
        col = "navyblue",
        xlab = "Tranche d'âge", ylab = "Moyenne de Sgot",
        main = "Moyenne de Sgot par tranche d'âge chez les hommes")# Calculer les pourcentages par groupe
mP1=mean(data$Proteines[data$Age_classe=="10-35"&data$Genre=="Femme"])
mP2=mean(data$Proteines[data$Age_classe=="35-60"&data$Genre=="Femme"])
mP3=mean(data$Proteines[data$Age_classe=="60-85"&data$Genre=="Femme"])
means <- c(mP1, mP2, mP3)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes,
        ylim = c(0,8),
        col = "mediumvioletred",
        xlab = "Tranche d'âge", ylab = "Moyenne de Proteines",
        main = "Moyenne de Proteines par tranche d'âge chez les femmes")# Calculer les pourcentages par groupe

mP4=mean(data$Proteines[data$Age_classe=="10-35"&data$Genre=="Homme"])
mP5=mean(data$Proteines[data$Age_classe=="35-60"&data$Genre=="Homme"])
mP6=mean(data$Proteines[data$Age_classe=="60-85"&data$Genre=="Homme"])
means <- c(mP4, mP5, mP6)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes, 
        ylim = c(0,8),
        col = "navyblue",
        xlab = "Tranche d'âge", ylab = "Moyenne de Proteines",
        main = "Moyenne de Proteines par tranche d'âge chez les hommes")# Calculer les pourcentages par groupe


mf1=mean(data$Alkphos[data$Age_classe=="10-35"&data$Genre=="Femme"])
mf2=mean(data$Alkphos[data$Age_classe=="35-60"&data$Genre=="Femme"])
mf3=mean(data$Alkphos[data$Age_classe=="60-85"&data$Genre=="Femme"])
means <- c(mf1, mf2, mf3)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes,
        ylim = c(0,300),
        col = "mediumvioletred",
        xlab = "Tranche d'âge", ylab = "Moyenne de l'alkphos",
        main = "Moyenne de l'Alkphos par tranche d'âge chez les femmes")# Calculer les pourcentages par groupe

mH1=mean(data$Alkphos[data$Age_classe=="10-35"&data$Genre=="Homme"])
mH2=mean(data$Alkphos[data$Age_classe=="35-60"&data$Genre=="Homme"])
mH3=mean(data$Alkphos[data$Age_classe=="60-85"&data$Genre=="Homme"])
means <- c(mH1, mH2, mH3)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes, 
        ylim = c(0,400),
        col = "navyblue",
        xlab = "Tranche d'âge", ylab = "Moyenne de l'alkphos",
        main = "Moyenne de l'Alkphos par tranche d'âge chez les hommes")# Calculer les pourcentages par groupe


#                        test
chisq.test(data$Age_classe,data$Genre)

# Effectuer un test t de Student pour chaque tranche d'âge chez les femmes
t.test(data$Proteines[data$Age_classe == "10-35" & data$Genre == "Femme"],
       data$Proteines[data$Age_classe == "10-35" & data$Genre == "Homme"])

# Effectuer un test t de Student pour chaque tranche d'âge chez les hommes
t.test(data$Proteines[data$Age_classe == "35-60" & data$Genre == "Femme"],
       data$Proteines[data$Age_classe == "35-60" & data$Genre == "Homme"])


# Effectuer un test t de Student pour chaque tranche d'âge chez les femmes
t.test(data$Proteines[data$Age_classe == "60-85" & data$Genre == "Femme"],
       data$Proteines[data$Age_classe == "60-85" & data$Genre == "Homme"])



t.test(data$Proteines[data$Age_classe == "10-35" & data$Genre == "Femme"],
       data$Proteines[data$Age_classe == "35-60" & data$Genre == "Femme"])

# Effectuer un test t de Student pour chaque tranche d'âge chez les hommes
t.test(data$Proteines[data$Age_classe == "35-60" & data$Genre == "Femme"],
       data$Proteines[data$Age_classe == "60-85" & data$Genre == "Femme"])

# Effectuer un test t de Student pour chaque tranche d'âge chez les femmes
t.test(data$Proteines[data$Age_classe == "10-35" & data$Genre == "Femme"],
       data$Proteines[data$Age_classe == "60-85" & data$Genre == "Femme"])


t.test(data$Proteines[data$Age_classe == "10-35" & data$Genre == "Homme"],
       data$Proteines[data$Age_classe == "35-60" & data$Genre == "Homme"])

# Effectuer un test t de Student pour chaque tranche d'âge chez les hommes
t.test(data$Proteines[data$Age_classe == "35-60" & data$Genre == "Homme"],
       data$Proteines[data$Age_classe == "60-85" & data$Genre == "Homme"])

# Effectuer un test t de Student pour chaque tranche d'âge chez les femmes
t.test(data$Proteines[data$Age_classe == "10-35" & data$Genre == "Homme"],
       data$Proteines[data$Age_classe == "60-85" & data$Genre == "Homme"])
# l'histogramme de la première valeur
#hist(data$Proteines[data$Statut_Foie=="Malade"&data$Genre=="Homme"],
#main = "taux de proteines total 
#des patients homme ",
#xlab = "age des patients",
#ylab = "Fréquence",ylim = c(0,30),col = "green")

# Ajout de l'histogramme de la deuxième valeur dans le mm graphique
#par(new=TRUE) #fonction pour 2 graphes
#hist(data$Proteines[data$Statut_Foie=="Sain"&data$Genre=="Homme"],
 #    main = "taux de Bilirubine selon 
  #   l'agedes patients homme sains",
   #  xlab = "age des patients",
    # ylab = "Fréquence",ylim = c(0,30),col = "green")
# Ajout d'une légende
#legend("topright", legend = c("SAIN", "MALADE"), fill = c("red", "lightblue"))
#hist(data$Age[data$Statut_Foie])
#hist(x=data$Age[data$Bilirubine&data$Statut_Foie=="Sain"&data$Genre=="Femme"])
#install.packages("ggplot2")

summary(data$Alkphos[data$Statut_Foie=="Sain"&data$Genre=="Femme"])
P48FM=data$Alkphos[data$Statut_Foie=="Malade"&data$Genre=="Femme"&data$Age>48]
M48FM=data$Alkphos[data$Statut_Foie=="Malade"&data$Genre=="Femme"&data$Age<48]
P48FS=data$Alkphos[data$Statut_Foie=="Sain"&data$Genre=="Femme"&data$Age>48]
M48SS=data$Alkphos[data$Statut_Foie=="Sain"&data$Genre=="Femme"&data$Age<48]
P48HM=data$Alkphos[data$Statut_Foie=="Malade"&data$Genre=="Homme"&data$Age>48]
M48HM=data$Alkphos[data$Statut_Foie=="Malade"&data$Genre=="Homme"&data$Age<48]
P48HS=data$Alkphos[data$Statut_Foie=="Sain"&data$Genre=="Homme"&data$Age>48]
M48HSFS=data$Alkphos[data$Statut_Foie=="Sain"&data$Genre=="Homme"&data$Age<48]
categories <- c("-48","-48","+48","+48")
valeurs <- c(median(M48FM),median(M48HM),median(P48FM),median(P48HM))
#barplot(valeurs, names.arg = categories, col = "skyblue", main = "Bar Plot",
 #       xlab = "Categories", ylab = "Valeurs")
median(M48FM)
median(P48FM)
median(M48HM)
barplot(matrix(valeurs, nrow = 2, byrow = TRUE),
        beside = TRUE,
        names.arg = categories,
        legend.text = c("Femme", "Homme"),
        args.legend = list(x = "topleft", bty = "n"),
        ylim=c(0,250),
        col = c("#1d1d86", "maroon"),
        xlab = "Genre des patients",
        ylab = "Taux de Alkphos",
        main = "Comparaison du statut du foie
        par rapport au Genre des patients")

text(x = barplot(matrix(valeurs, nrow = 2, byrow = TRUE),
                 beside = TRUE,
                 col = c("#1d1d86", "maroon"),
                 names.arg = categories,
                 plot = FALSE),
     y = valeurs + 1,   
     label = valeurs,
     pos = 3,
     cex = 1,         
     col = "black")

# Test du chi2 pour le genre et l'état du foie
table_genre_foie <- table(data$Genre, data$Statut_Foie)
chi2 <- chisq.test(table_genre_foie)
chi2
#p-value = 0.708>alpha=0.05 donc on accepte H0."a verfier +tard dans table
# Régression logistique
modele_logistique <- glm(Statut_Foie ~ Genre + Age + Bilirubine + Sgpt + Sgot + Alkphos + Proteines, 
                         data = data, family = binomial)

# Résumé du modèle
summary(modele_logistique)
#ce resumé montre que  seule la variable Alkphos semble avoir un effet significatif sur l'état du foie, tandis que les autres variables n'ont pas d'effet significatif
# Créer un modèle ANOVA pour comparer les moyennes entre les différents états du foie nous verifions ces infos apres
modele_anova <- aov(Age ~ Statut_Foie + Bilirubine + Sgpt + Sgot + Alkphos + Proteines, data = data)

# Afficher un résumé du modèle ANOVA
summary(modele_anova)
#ce resumé montre que seule les proteine ont un effet significative sur l'etat du fois
# Calcul de la corrélation de Spearman entre l'âge et les dosages des enzymes sanguins
cor_spearman <- cor(data$Age, data$Bilirubine, method = "spearman")
cor_spearman
#data$prob_pred <- predict(modele_logistique, type = "response")
#plot(data$prob_pred, data$EtatFoie, xlab = "Probabilité Prédite", ylab = "État du Foie",
    # main = "Plot des Probabilités Prédites vs État du Foie", pch = 19, col = "blue")
#abline(h = 0.5, col = "red", lty = 2)


#plot(data$Age, data$Bilirubine, 
    # xlab = "Âge", ylab = "Bilirubine",
    # main = "Relation entre Âge et Bilirubine")

mean(data$Alkphos[data$Genre=="Homme"&data$Alkphos&data$Statut_Foie=="Malade"])
boxplot(data$Bilirubine~data$Age,
        ylab="nombre de patients",
        xlab="taux d'aspartate aminotransferase dans le sang",
        col=c("chartreuse","chartreuse4"),
        main = "Dosage du taux d'aspartate aminotransferase dans le sang par l'etat du foie")
library(ggplot2)


age <- c(35,60)  # Seuils pour l'Alkphos

# diviser les données d'Alkphos en classes
get_classe_age <- function(valeur) {
  if (valeur < age[1]) {
    return("10-35")
  } else if (valeur <= age[2]) {
    return("35-60")
  }else {
    return("60-85")
  }
}

#  colonne classe d'Alkphos
data$Age_classe <- sapply(data$Age, get_classe_age)

# niveau Alkphos_classe
data$Age_classe <- factor(data$Age_classe, levels = c("10-35", "35-60", "60-85"))

# histogramme ggplot2
ggplot(data, aes(x = Age_classe, fill = Statut_Foie)) +
  geom_bar(position = "dodge", color = "black") +
  facet_grid(~ Genre) +
  labs(title = "Relation entre l'age et la statut du foie selon le genre des patients",
       x = "Classe d'Age", y = "") +
  theme_minimal()
summary(data$Age[data$Genre=="Homme"])
m1=mean(data$Bilirubine[data$Age_classe=="10-35"])
m2=mean(data$Bilirubine[data$Age_classe=="35-60"])
m3=mean(data$Bilirubine[data$Age_classe=="60-85"])
means <- c(m1, m2, m3)
age_classes <- c("10-35", "35-60", "60-85")
barplot(means, names.arg = age_classes, 
        col = "red",
        xlab = "Tranche d'âge", ylab = "Moyenne de la bilirubine",
        main = "Moyenne de la bilirubine par tranche d'âge")

library(dplyr)
library(ggplot2)

# données pour les patients malades
malades <- data %>%
  filter(Statut_Foie == "Malade")

# données pour les patients sains
sains <- data %>%
  filter(Statut_Foie == "Sain")

# pourcentages par classe d'âge 
#pour les patients malades femmes
pourcentages_malades_femmes <- malades %>%
  filter(Genre == "Femme") %>%
  group_by(Age_classe) %>%
  summarize(percentage = n() / nrow(malades) * 100)

# pour les patients malades hommes
pourcentages_malades_hommes <- malades %>%
  filter(Genre == "Homme") %>%
  group_by(Age_classe) %>%
  summarize(percentage = n() / nrow(malades) * 100)

# les patients sains femmes
pourcentages_sains_femmes <- sains %>%
  filter(Genre == "Femme") %>%
  group_by(Age_classe) %>%
  summarize(percentage = n() / nrow(sains) * 100)

# les patients sains hommes
pourcentages_sains_hommes <- sains %>%
  filter(Genre == "Homme") %>%
  group_by(Age_classe) %>%
  summarize(percentage = n() / nrow(sains) * 100)

# afficher dans le même graphique
resultats <- bind_rows(
  mutate(pourcentages_malades_femmes, Statut_Foie = "Malade", Genre = "Femme"),
  mutate(pourcentages_malades_hommes, Statut_Foie = "Malade", Genre = "Homme"),
  mutate(pourcentages_sains_femmes, Statut_Foie = "Sain", Genre = "Femme"),
  mutate(pourcentages_sains_hommes, Statut_Foie = "Sain", Genre = "Homme")
)

# l'histogramme
ggplot(resultats, aes(x = Age_classe, y = percentage, fill = Statut_Foie)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  facet_grid(~ Genre) +
  labs(title = "Relation entre l'âge et le statut du foie selon le genre des patients",
       x = "Classe d'Âge", y = "Pourcentage") +
  theme_minimal()




# Alkphos pour les hommes et les femmes
seuils_alkphos_hommes <- c(25, 50)  # Seuils pour les hommes
seuils_alkphos_femmes <- c(150, 350)  # Seuils pour les femmes

# diviser Alkphos en classes
get_classe_alkphos <- function(valeur, genre) {
  if (genre == "Homme") {
    if (valeur < seuils_alkphos_hommes[1]) {
      return("Faible")
    } else if (valeur < seuils_alkphos_hommes[2]) {
      return("Moyen")
    } else {
      return("Élevé")
    }
  } else {  # Pour les femmes
    if (valeur < seuils_alkphos_femmes[1]) {
      return("Faible")
    } else if (valeur < seuils_alkphos_femmes[2]) {
      return("Moyen")
    } else {
      return("Élevé")
    }
  }
}


data$Alkphos_classe <- mapply(get_classe_alkphos, data$Alkphos, data$Genre)
data$Alkphos_classe <- factor(data$Alkphos_classe, levels = c("Faible", "Moyen", "Élevé"))
#graphe
ggplot(data, aes(x = Alkphos_classe, fill = Statut_Foie)) +
  geom_bar(position = "dodge", color = "black") +
  facet_grid(~ Genre) +
  labs(title = "Histogramme des classes d'Alkphos en fonction de l'état du foie et du genre",
       x = "Classe d'Alkphos", y = "Fréquence") +
  theme_minimal()
summary(data$Alkphos[data$Genre=="Femme"])


seuils <- list(Bilirubine = c(2, 5), Sgpt = c(100, 200), Sgot = c(100, 200), Proteines = c(5, 8))


get_classe <- function(valeur, seuils) {
  if (valeur < seuils[1]) {
    return("Faible")
  } else if (valeur < seuils[2]) {
    return("Moyen")
  } else {
    return("Élevé")
  }
}

# graphe
for (enzyme in names(seuils)) {
  data[[paste0(enzyme, "_classe")]] <- sapply(data[[enzyme]], get_classe, seuils[[enzyme]])
  
ggplot(data, aes(x = !!sym(paste0(enzyme, "_classe")), fill = Statut_Foie)) +
    geom_bar(position = "dodge", color = "black") +
    facet_grid(~ Genre) +
    labs(title = paste("Histogramme des classes de", enzyme, "en fonction de l'état du foie et du genre"),
         x = paste("Classe de", enzyme), y = "Fréquence") +
    theme_minimal()}
 
summary(data$Sgot[data$Genre=="Homme"])
summary(data$Age[data$Alkphos_classe=="Faible"&data$Statut_Foie=="Malade"])
hist(data$Age[data$Alkphos_classe=="Faible"&data$Statut_Foie=="Sain"])

MA=chisq.test(table(data$Statut_Foie, data$Genre))
MA$expected
# Test de corrélation entre l'âge et l'Alkphos
cor.test(data$Age, data$Alkphos)
seuils <- list(Bilirubine = c(2, 6), Alkphos=c(200,400),Sgpt = c(40, 200), Sgot = c(40, 200), Proteines = c(6, 8))

tableau_enzyme <- data.frame(
  Genre=data$Genre,
  Age = data$Age,
  Statut_Foie = data$Statut_Foie,
  Bilirubine_classe = cut(data$Bilirubine, breaks = c(-Inf, seuils$Bilirubine, Inf), labels = c("Faible", "Moyen", "Élevé")),
  Alkphos_classe=cut(data$Alkphos, breaks = c(-Inf, seuils$Alkphos, Inf), labels = c("Faible", "Moyen", "Élevé")),
  Sgpt_classe = cut(data$Sgpt, breaks = c(-Inf, seuils$Sgpt, Inf), labels = c("Faible", "Moyen", "Élevé")),
  Sgot_classe = cut(data$Sgot, breaks = c(-Inf, seuils$Sgot, Inf), labels = c("Faible", "Moyen", "Élevé")),
  Proteines_classe = cut(data$Proteines, breaks = c(-Inf, seuils$Proteines, Inf), labels = c("Faible", "Moyen", "Élevé"))
)

tableau_enzyme
table(tableau_enzyme$Proteines_classe[tableau_enzyme$Statut_Foie=="Sain"&tableau_enzyme$Genre=="Femme"])

enzyFS<-matrix(c(8,6,2,13,3,0,11,5,0,12,3,1,5,8,3),byrow=T,ncol=3)

rownames(enzyFS)<-c("Alkphos", "Bilirubine", "Sgpt","Sgot","Proteines")
colnames(enzyFS)<-c("Faible","Moyen" , "Élevé")
enzyFS_pourcentage <- apply(enzyFS, 1, function(x) (x / sum(x)) * 100)
enzyFS_pourcentage
barplot(height=enzyFS, beside=TRUE,legend.text=rownames(enzyFS), las=1, 
        xlab="taux des enzymes",ylab="Nombre de patients")
# On transpose le tableau
t(enzyFS)
barplot(height=t(enzyFS), beside=TRUE,
        main = "Représentation du taux de différentes enzymes 
        du sang des patients (femmes) sains",
        col = c("green", "red","blue"),
        ylim = c(0,20),
        las=1,xlab="enzymes du sang",ylab="Nombre de patients")
legend("topright", legend = c("Faible","Moyen" , "Élevé"), fill = c("green", "red","blue"))


#                   FM


table(tableau_enzyme$Proteines_classe[tableau_enzyme$Statut_Foie=="Malade"&tableau_enzyme$Genre=="Femme"])

enzyFM<-matrix(c(5,4,0,9,0,0,8,1,0,7,2,0,1,7,1),byrow=T,ncol=3)

rownames(enzyFM)<-c("Alkphos", "Bilirubine", "Sgpt","Sgot","Proteines")
colnames(enzyFM)<-c("Faible","Moyen" , "Élevé")
enzyFM_pourcentage <- apply(enzyFM, 1, function(x) (x / sum(x)) * 100)
enzyFM_pourcentage
fm=chisq.test(enzyFM)
fm$expected
fisher.test(enzyFM)
t(enzyFM)
barplot(height=t(enzyFM), beside=TRUE,
        main = "Représentation du taux de différentes enzymes 
        du sang des patients (femmes) malades",
        col = c("green", "red","blue"),
        ylim = c(0,20),
        las=1,xlab="enzymes du sang",ylab="Nombre de patients")
legend("topright", legend = c("Faible","Moyen" , "Élevé"), fill = c("green", "red","blue"))


#                HM
table(tableau_enzyme$Proteines_classe[tableau_enzyme$Statut_Foie=="Malade"&tableau_enzyme$Genre=="Homme"])

enzyHM<-matrix(c(16,5,1,20,2,0,18,4,0,16,6,0,8,13,1),byrow=T,ncol=3)
enzyHM
rownames(enzyHM)<-c("Alkphos", "Bilirubine", "Sgpt","Sgot","Proteines")
colnames(enzyHM)<-c("Faible","Moyen" , "Élevé")
enzyHM_pourcentage <- apply(enzyHM, 1, function(x) (x / sum(x)) * 100)
enzyHM_pourcentage
# On transpose le tableau
t(enzyHM)
barplot(height=t(enzyHM), beside=TRUE,
        main = "Représentation du taux de différentes enzymes 
        du sang des patients (hommes) malades",
        col = c("green", "red","blue"),
        ylim = c(0,30),
        las=1,xlab="enzymes du sang",ylab="Nombre de patients")
legend("topright", legend = c("Faible","Moyen" , "Élevé"), fill = c("green", "red","blue"))
#                               HS


table(tableau_enzyme$Proteines_classe[tableau_enzyme$Statut_Foie=="Sain"&tableau_enzyme$Genre=="Homme"&tableau_enzyme$Age>40])

enzyHS<-matrix(c(12,29,12,29,12,12,18,25,10,14,26,13,15,36,2),byrow=T,ncol=3)

rownames(enzyHS)<-c("Alkphos", "Bilirubine", "Sgpt","Sgot","Proteines")
colnames(enzyHS)<-c("Faible","Moyen" , "Élevé")

# On transpose le tableau
t(enzyHS)
barplot(height=t(enzyHS), beside=TRUE,
        main = "Représentation du taux de différentes enzymes 
        du sang des patients (hommes) sains",
        col = c("green", "red","blue"),
        ylim = c(0,40),
        las=1,xlab="enzymes du sang",ylab="Nombre de patients")
legend("topright", legend = c("Faible","Moyen" , "Élevé"), fill = c("green", "red","blue"))
table_genre_foi<- table(tableau_enzyme, tableau_enzyme$Statut_Foie)


# on Converti les effectifs en pourcentage pour chaque enzyme
enzyHS_pourcentage <- apply(enzyHS, 1, function(x) (x / sum(x)) * 100)
enzyHS_pourcentage
# Représentation avec les effectifs en pourcentage et étiquettes
barplot(height = enzyHS_pourcentage, beside = TRUE,
        main = "Représentation du taux de différentes enzymes 
        du sang des patients (hommes) sains (en pourcentage)",
        col = c("green", "red", "blue"),
        ylim = c(0, 100),
        las = 1,
        xlab = "Enzymes du sang",
        ylab = "Pourcentage de patients")

# Ajouter les étiquettes des pourcentages au-dessus de chaque barre
text(x = barplot(height = t(enzyHS_pourcentage), beside = TRUE, plot = FALSE), 
     y = enzyHS_pourcentage + 2, 
     labels = sprintf("%.1f%%", enzyHS_pourcentage), 
     pos = 3.5, 
     cex = 0.7)
legend("topright", legend = colnames(enzyHS_pourcentage), fill = c("green", "red", "blue"))

masl<- table(data$Age_classe, data$Genre)
TST=chisq.test(masl)#p-value >0 on accepte H0 Il n'y a pas de relation entre le statut du foie et l'age des patients
#idemm pour le genre et l'age
TST
TST$expected# condition chi-2 respecté 
TST$residuals
mean(data$Alkphos[data$Alkphos<200&data$Genre=="Homme"&data$Statut_Foie=="Malade"])
mean(data$Alkphos[data$Alkphos<200&data$Genre=="Homme"&data$Statut_Foie=="Sain"])
mean(data$Bilirubine[data$Bilirubine<2&data$Genre=="Homme"&data$Statut_Foie=="Malade"])
result<-aov(data$Bilirubine ~ tableau_enzyme$Alkphos_classe)
summary(result)
kruskal.test(data$Bilirubine ~ tableau_enzyme$Alkphos_classe)
plot(x=data$Bilirubine)
#data$Bilirubine>40
#which(data$Bilirubine>40)
#data$Bilirubine[41]<- NA

# Sous-ensemble de données pour les hommes atteints de problèmes hépatiques
hommes_malades <- subset(data, Genre == "Homme" & Statut_Foie == "Malade")
# Sous-ensemble de données pour les hommes sains
hommes_sains <- subset(data, Genre == "Homme" & Statut_Foie == "Sain")

# Test t de Student pour la bilirubine chez les hommes
t_bilirubine_hommes <- t.test(hommes_malades$Bilirubine<2, hommes_sains$Bilirubine<2)
t_bilirubine_hommes

# Test t de Student pour l'Alkphos chez les hommes
t_alkphos_hommes <- t.test(hommes_malades$Alkphos, hommes_sains$Alkphos)
t_alkphos_hommes

# Sous-ensemble de données pour les femmes atteintes de problèmes hépatiques
femmes_malades <- subset(data, Genre == "Femme" & Statut_Foie == "Malade")
# Sous-ensemble de données pour les femmes saines
femmes_saines <- subset(data, Genre == "Femme" & Statut_Foie == "Sain")

# Test t de Student pour la bilirubine chez les femmes
var(data$Alkphos[data$Alkphos<200&data$Genre=="Homme"&data$Statut_Foie=="Sain"])
var(data$Alkphos[data$Alkphos<200&data$Genre=="Homme"&data$Statut_Foie=="Malade"])

t_bilirubine_femmes <- t.test(femmes_malades$Bilirubine, femmes_saines$Bilirubine)
t_bilirubine_femmes



# Test t de Welch pour la bilirubine chez les hommes
t_bilirubine_hommes <- t.test(hommes_malades$Bilirubine, hommes_sains$Bilirubine, var.equal = FALSE)
t_bilirubine_hommes
# Test t de Welch pour l'Alkphos chez les hommes
t_alkphos_hommes <- t.test(hommes_malades$Alkphos, hommes_sains$Alkphos, var.equal = FALSE)
t_alkphos_hommes
# Test t de Welch pour la bilirubine chez les femmes
t_bilirubine_femmes <- t.test(femmes_malades$Bilirubine, femmes_saines$Bilirubine, var.equal = FALSE)
t_bilirubine_femmes

library(ggplot2)

ggplot(data, aes(x = Age, y = Bilirubine, color = Statut_Foie)) +
  geom_point() +
  labs(title = "Relation entre l'âge, la bilirubine et le statut du foie",
       x = "Âge", y = "Bilirubine") +
  scale_color_manual(values = c("Malade" = "red", "Sain" = "blue")) +
  theme_minimal()
anova_result <- aov(Bilirubine ~ Statut_Foie * Genre, data = data)
summary(anova_result)
#p > 0.05 Genre pas d'effet sur niveau de bilirubine
#p > 0.05 statu du fois ne depend du genre 
#pvalue= 0.0683 tres proche de alpha 0.05 on va essayer de faire le teste non parametrique
kruskal.test(Bilirubine ~ Statut_Foie, data = data)
# pvaleur<0.05 il existe une différence notable dans les niveaux de bilirubine entre les individus ayant un foie sain et ceux ayant un foie malade