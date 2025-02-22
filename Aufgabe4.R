# Aufgabenstellung von Aufgabe 4
# Diejenigen Gruppenmitglieder, die nicht an (1.) gearbeitet haben,
# sollen mit Hilfe der Funktionen den aufgeräumten Datensatz aus (1.) analysieren
# (Deskription und Visualisierung). 

# Funktionen aus den Teilskripten laden

source("Aufgabe_2_Skript_1.R")
source("R_Skript1.R")
source("Skript1_Aufgabe_2a_iii_iv.R")
# Sourcing schlägt fehl
file.exists("Skript1_Aufgabe_2a_iii_iv.R")
source(list.files(pattern = "Skript1_Aufgabe_2a_iii_iv.R$", full.names = TRUE))
source("Skript1_Aufgabe_2a_iii_iv.R", encoding = "UTF-8")

# Analyse des titanic_Bereinigt.csv

df <- read.csv("titanic.csv", header = TRUE, stringsAsFactors = FALSE)
df


# Wie ist das Alter der Gäste auf der Titanic verteilt gewesen ? 
func_spread_summary(df$Age)
# Unterscheidet sich das Alter von Überlebenden und Verstorbenen ? 
# These: Alte Menschen und Kinder 

func_spread_summary(df[df$Survived == 0,]$Age)
func_spread_summary(df[df$Survived == 1,]$Age)
# Wie ist sind mehr Frauen oder mehr Männer auf dem Schiff gewesen ? 
func_categorial_summary(df$Sex)
# Mehr Männer 
# Wie sieht die Aufteilung bei den Überlebenden aus ? 
func_categorial_summary(df[df$Survived == 1,]$Sex)




