#Titanic Datensatz laden
 Development
# sep= ";" wird verwendet bei mir, da die Spalten in der Ursprünglichen 
# Datei nicht erkannt werden
titanic <- 
  read.csv("C:/Users/Miche/Desktop/Wissenschaftliches_Arbeiten/titanic.csv", 
           sep=";")


# 1. Teilaufgabe (Michel)
# Extrahiert aus dem Namen eine Variable mit der Anrede der Person

  
  # erfasst alles zwischen Komma und Punkt in der Spalte "Name" und packt es in 
  # die Spalte "Title",
  # also seperiert den Titel aus der Spalte "Name".
  titanic$Title <- sub(".*,\\s*(.*?)\\..*", "\\1", titanic$Name) 
  
  
  # Anreden standardisieren, so dass nurnoch Mr, Mrs, Miss und Master
  # verwendet werden

#sep= ";" wird verwendet bei mir, da die SPalten in der Ursprünglichen Datei nicht erkannt werden
titanic <- read.csv("C:/Users/Miche/Desktop/Wissenschaftliches_Arbeiten/titanic.csv", sep=";")


#1. Teilaufgabe 
#Extrahiert aus dem Namen eine Variable mit der Anrede der Person

  
  #erfasst alles zwischen Komma und Punkt in der Spalte "Name" und packt es in die Spalte "Title",
  #also seperiert den Titel aus der Spalte "Name".
  titanic$Title <- sub(".*,\\s*(.*?)\\..*", "\\1", titanic$Name) 
  
  
  # Anreden standardisieren, so dass nurnoch Mr, Mrs, Miss und Master verwendet werden
Dev.-Michel
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
  
Development
  # Aendert die Einträge in der Spalte Titel nach dem Muster von der
  # Variabel "Standart"

  #Ändert die Einträge in der Spalte Titel nach dem Muster von der Variabel "Standart"
Dev.-Michel
  titanic$Title <- ifelse(titanic$Title %in% names(Standart), 
                          Standart[titanic$Title], 
                          titanic$Title) 
  
 Development
# 2. Teilaufgabe (David)
# Codiert die Variablen „Survived“, „Sex“, „Embarked“ als factor um.
  
  titanic$Survived <- as.factor(titanic$Survived)
  titanic$Sex <- as.factor(titanic$Sex)
  titanic$Embarked <- as.factor(titanic$Embarked)

# 3. Teilaufgabe (Michel)
#Überführt die Variable „Pclass“ in einen ordered-factor.
  titanic$Pclass<-factor(titanic$Pclass, levels = c(3, 2, 1), ordered = TRUE)

    #überprüfen ob alle Titel geändert wurden
    titanic$Title
    
 Dev.-Michel
