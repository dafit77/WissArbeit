# Titanic Datensatz laden
# sep= ";" wird verwendet bei mir, da die Spalten in der Urspruenglichen 
# Datei nicht erkannt werden
# titanic <-read.csv("C:/Users/Miche/Desktop/Wissenschaftliches_Arbeiten/titanic.csv", sep=";")

titanic <- read.csv("titanic.csv")
# Read Datei fuer mein Mac (David und Colin)

# 1. Teilaufgabe (Michel)
# Extrahiert aus dem Namen eine Variable mit der Anrede der Person

titanic$Title <- sub(".*,\\s*(.*?)\\..*", "\\1", titanic$Name) 
# erfasst alles zwischen Komma und Punkt in der Spalte "Name" 
# und packt es in die Spalte "Title",
# also separiert den Titel aus der Spalte "Name".

# table(titanic$Title) 
# Anzeigen, welche Titel existieren und welche ersetzt werden

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

titanic$Title <- ifelse(titanic$Title %in% names(Standart), 
                        Standart[titanic$Title], 
                        titanic$Title) 
# Aendert die Eintraege in der Spalte Titel nach dem Muster von der
# Variabel "Standart"

# ueberpruefen ob alle Titel geaendert wurden
# table(titanic$Title)  

# 2. Teilaufgabe (David)
# Codiert die Variablen „Survived“, „Sex“, „Embarked“ als factor um.

titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# Durch die RBase Funktion "as.factor" ist die Umcodierung des Codes 
# sehr simpel und einfach durchgefuehrt

# 3. Teilaufgabe (Michel)
# Ueberfuehrt die Variable „Pclass“ in einen ordered-factor.

titanic$Pclass<-factor(titanic$Pclass, levels = c(3, 2, 1), ordered = TRUE)

# Erneute umsetzung der Faktor-Funktion aus Base R, diesmal mit der
# Ergaenzung des "levels" Attributs, um eine korrekte Sortierung
# zu gewaehrleisten

# 4. Teilaufgabe (David)
# Imputiert fehlende Werte in der Variable „Age“ mithilfe der erzeugten
# Variable „Anrede“ ueber ein Imputationsverfahren eurer Wahl (z.B.
# arithmetisches Mittel, Median, usw.)

med.Mr <- median(titanic$Age[titanic$Title == "Mr"], na.rm = TRUE)
med.Mrs <- median(titanic$Age[titanic$Title == "Mrs"], na.rm = TRUE)
med.Master <- median(titanic$Age[titanic$Title == "Master"], na.rm = TRUE)
med.Miss <- median(titanic$Age[titanic$Title == "Miss"], na.rm = TRUE)

# Schritt 1

# Einigung auf die Verwendung des Medians fuer den Imputierten Wert.
# Dieser wird jeweils auf eine Teilmenge die gesamte Wertemenge der
# variable "Titles" angewand. Jede Anwendung bezieht sich daraufhin
# auf den relevanten Aspekt und wir auf alle vorkommenden Merkmals-
# auspraegungen einzelnd angewand.

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

# Mehrschrittiger Prozess zur Bearbeitung der Wertetabelle der Variable "Age"
# Zunächst werden alle Werte einer Merkmalsauspraegung in einem separaten
# Vektor gespeichert. Auf diesem Vektor wird darauf ein logischer Vektor 
# angewand, welcher den Wert "False" für jeder Position im verkuerzten Vektor
# besitzt, wessen laenge gleich ist zur die Menge der jeweiligen Merkmals-
# auspraegung. Nun kann differenziert werden, welche Werte in diesem Vektor
# die Originalen Werte aus dem Datensatz sind, und welche Werte duch vorherige
# ausarbeitung (Schritt 1) nun ergaenzt werden muessen. Nachdem dies Erfolgt 
# wird der Originale Datensatz mit dem noch "unvollstaendigen" Vektor mit 
# dem gerade erstelltem, "vollstaendigen" Vektor ueberschrieben. Diese 
# Methode wird 4 mal wiederholt fuer jede Merkmalsauspraegung.

# 5. Teilaufgabe (Michel)
# Extrahiert aus der Variable „Cabin“ die folgenden Informationen und erzeugt
# neue Variablen hierfür:

# 5.3
# Eintraege mit unbekannter Kabinennummer, d.h. „“ setzt ihr auf NA.
titanic$Cabin[titanic$Cabin == ""] <- NA # 5.3 zuerst, damit Komplikationen
                                         # im Code bei 5.1 und 5.2 vermieden
                                         # werden

# 5.1
# Backbord oder Steuerbord? Tipp: Kabinen mit einer ungeraden Nummer liegen
# auf Steuerbord, die anderen auf Backbord.
titanic$Side <-  ifelse(
  is.na(titanic$Cabin), NA, ifelse(as.numeric(gsub("[^0-9]", "",
                                                  titanic$Cabin)) %% 2 == 1, 
                                 # Ueberprueft ob Kabinennummer modolu 2 gleich
                                 # 1 ist, also ob Kabinennummer ungerade ist
                                                  "Steuerbord",
                                 # Wenn Ungerade wird Wert in Spalte Side zu
                                 # Steuerbord gesetzt
                                                  "Backbord"
                                 # Ansonsten auf Backbord gesetzt
                                  )
                                )

# 5.2
# Deck: Vorangehender Buchstabe der Kabinennummer
titanic$Deck<-ifelse(
  is.na(titanic$Cabin), NA,      # Wenn Kabinennummer unbekannt ist so
                                 # ist auch Decknummer unbekannt
  substr(titanic$Cabin, 1, 1)    # Ansonsten nimmt es den ersten Wert 
                                 # der Kabinennummer
)

# 6. Teilaufgabe (Michel)
# Entfernt am Ende die Variablen „PassengerID“, „Name“,
# „Ticket“ und „Cabin“ aus dem Datensatz

titanic$PassengerId <- NULL
titanic$Name <- NULL
titanic$Ticket <- NULL
titanic$Cabin <- NULL

# Die zu ueberschreibenden Variablen werden mit einer Nullmenge ersetzt, welche 
# in der Darstellung danach komplett ausgelassen werden.

# 7. Teilaufgabe (DAvid)
# Abspeichern

write.csv(titanic, file = "titanic_Berichtigt.csv", row.names = FALSE)

# Die Datei wird in der Directory der Repository gespeichert.

# rm(list=ls())
rm(allMaster, allMiss, allMr, allMrs, FalseVek1, FalseVek2, FalseVek3, med.Master,med.Miss,med.Mr,med.Mrs,Standart)

# Speichern der bearbeiteten Tabelle (optional, kann auskommentiert werden)
