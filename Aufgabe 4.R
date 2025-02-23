# Aufgabenstellung von Aufgabe 4
# Diejenigen Gruppenmitglieder, die nicht an (1.) gearbeitet haben,
# sollen mit Hilfe der Funktionen den aufgeräumten Datensatz aus (1.) analysieren
# (Deskription und Visualisierung). 

# Funktionen aus den Teilskripten laden

# renv für saubere Projekt Library
renv::activate()
renv::restore(prompt = FALSE) 

source("Aufgabe_2_Skript_1_fertig.R"

df <- read.csv("titanic_Berichtigt.csv", header = TRUE, stringsAsFactors = FALSE)
df


# Wie ist das Alter der Gäste auf der Titanic verteilt gewesen ? 
func_spread_summary(df$Age)
# Unterscheidet sich das Alter von Überlebenden und Verstorbenen ? 
# These: Alte Menschen und Kinder können nicht so gut schwimmen/ erfrieren schnell
# Im Eiswasser, auch wenn auf Ältere und Kinder bei der Rettung besondere Rücksicht 
# genommen wird sterben 

func_spread_summary(df[df$Survived == 0,]$Age)
func_spread_summary(df[df$Survived == 1,]$Age)
# sind mehr Frauen oder mehr Männer auf dem Schiff gewesen ? 
func_categorial_summary(df$Sex)
# Mehr Männer 
# Wie sieht die Aufteilung bei den Überlebenden aus ? 
func_categorial_summary(df[df$Survived == 1,]$Sex)
# Mehr Frauen



# Wieviele Tickets jeder Klasse wurden gekauft ? 
# Zu Geschlecht und Survived wurden ja schon deskriptive Statistiken berechnet
Visualisierung(df, "Sex", "Survived", "Pclass")


bivariate_stats(df, "Survived", "Sex")
# Hypothese: Vor allem die Gäste aus den günstigeren Klassen sind verstorben
bivariate_statsfkt(df, "Survived", "Pclass")
# Indiz für ja, da negative Korrelation zwischen Survived und Pclass
# Frage: Wer hatte eine bessere Überlebenschance Junge oder Alte Menschen ? 
bivariate_statsfkt(df, "Survived", "Age")
# Junge Menschen haben eine bessere Chance gehabt 
