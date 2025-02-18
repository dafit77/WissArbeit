#Titanic Datensatz laden
# sep= ";" wird verwendet bei mir, da die SPalten in der Ursprünglichen 
# Datei nicht erkannt werden
titanic <- 
  read.csv("C:/Users/Miche/Desktop/Wissenschaftliches_Arbeiten/titanic.csv", 
           sep=";")


# 1. Teilaufgabe 
# Extrahiert aus dem Namen eine Variable mit der Anrede der Person

  
  # erfasst alles zwischen Komma und Punkt in der Spalte "Name" und packt es in die Spalte "Title",
  # also seperiert den Titel aus der Spalte "Name".
  titanic$Title <- sub(".*,\\s*(.*?)\\..*", "\\1", titanic$Name) 
  
  
  # Anreden standardisieren, so dass nurnoch Mr, Mrs, Miss und Master verwendet werden
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
  
  #Ändert die Einträge in der Spalte Titel nach dem Muster von der Variabel "Standart"
  titanic$Title <- ifelse(titanic$Title %in% names(Standart), 
                          Standart[titanic$Title], 
                          titanic$Title) 
  
# 2. Teilaufgabe (David)
# Codiert die Variablen „Survived“, „Sex“, „Embarked“ als factor um.

  