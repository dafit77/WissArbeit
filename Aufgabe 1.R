#Titanic Datensatz laden
# sep= ";" wird verwendet bei mir, da die Spalten in der Ursprünglichen 
# Datei nicht erkannt werden

titanic <- 
  read.csv("C:/Users/Miche/Desktop/Wissenschaftliches_Arbeiten/titanic.csv", 
           sep=";")

# titanic <- read.csv("~/Documents/R/titanic.csv")
# Read Datei für mein Mac (David und Colin)


# 1. Teilaufgabe (Michel)
# Extrahiert aus dem Namen eine Variable mit der Anrede der Person

# erfasst alles zwischen Komma und Punkt in der Spalte "Name" und packt es in 
# die Spalte "Title",
# also seperiert den Titel aus der Spalte "Name".

titanic$Title <- sub(".*,\\s*(.*?)\\..*", "\\1", titanic$Name) 
# table(titanic$Title) # Anzeigen, welche Titel existieren 
                       # und welche ersetzt werden

# Anreden standardisieren, so dass nurnoch Mr, Mrs, Miss und Master
# verwendet werden

Standart <- c(
  "Ms" = "Mrs",
  "Mlle" = "Mrs",
  "Mme" = "Mrs",
  "Lady" = "Mrs",
  "Master"="Master",
  "Mrs"="Mrs",
  "Mr"="Mr",
  "Miss"="Miss",
  "Dr"="Mr",
  "the Countess"="Mrs",
  "Rev"="Mr",
  "Sir"="Mr",
  "Capt"="Mr",
  "Col"="Mr",
  "Don"="Mr",
  "Jonkheer"="Mr",
  "Major"="Mr"
)

# Aendert die Einträge in der Spalte Titel nach dem Muster von der
# Variabel "Standart"

titanic$Title <- ifelse(titanic$Title %in% names(Standart), 
                        Standart[titanic$Title], 
                        titanic$Title) 

#überprüfen ob alle Titel geändert wurden

titanic$Title  

# 2. Teilaufgabe (David)
# Codiert die Variablen „Survived“, „Sex“, „Embarked“ als factor um.

titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# 3. Teilaufgabe (Michel)
# Überführt die Variable „Pclass“ in einen ordered-factor.

titanic$Pclass<-factor(titanic$Pclass, levels = c(3, 2, 1), ordered = TRUE)

# 4. Teilaufgabe (David)
# Imputiert fehlende Werte in der Variable „Age“ mithilfe der erzeugten
# Variable „Anrede“ über ein Imputationsverfahren eurer Wahl (z.B.
# arithmetisches Mittel, Median, usw.)

med.Mr <- median(titanic$Age[titanic$Title == "Mr"], na.rm = TRUE)
med.Mrs <- median(titanic$Age[titanic$Title == "Mrs"], na.rm = TRUE)
med.Master <- median(titanic$Age[titanic$Title == "Master"], na.rm = TRUE)
med.Miss <- median(titanic$Age[titanic$Title == "Miss"], na.rm = TRUE)
# na.rm funkrioniert vlcht bei Michel nicht, nachher bitte einmal testen

allMr <- titanic$Age[titanic$Title == "Mr"]
FalseVek1 <- is.na(titanic$Age[titanic$Title == "Mr"])
allMr[FalseVek1] <- med.Mr
titanic$Age[titanic$Title == "Mr"] <- allMr

allMrs <- titanic$Age[titanic$Title == "Mrs"]
FalseVek2 <- is.na(titanic$Age[titanic$Title == "Mrs"])
allMrs[FalseVek2] <- med.Mrs
titanic$Age[titanic$Title == "Mrs"] <- allMrs

allMaster <- titanic$Age[titanic$Title == "Master"]
FalseVek3 <- is.na(titanic$Age[titanic$Title == "Master"])
allMaster[FalseVek3] <- med.Master
titanic$Age[titanic$Title == "Master"] <- allMaster

allMiss <- titanic$Age[titanic$Title == "Miss"]
FalseVek3 <- is.na(titanic$Age[titanic$Title == "Miss"])
allMiss[FalseVek3] <- med.Miss
titanic$Age[titanic$Title == "Miss"] <- allMiss 