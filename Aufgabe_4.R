# Aufgabenstellung von Aufgabe 4
# Diejenigen Gruppenmitglieder, die nicht an (1.) gearbeitet haben,
# sollen mit Hilfe der Funktionen den aufgeräumten Datensatz aus (1.) analysieren
# (Deskription und Visualisierung). 

# Funktionen aus den Teilskripten laden

# renv für saubere Projekt Library
renv::activate()
renv::restore(prompt = FALSE) 

source("Aufgabe_1.R")
source("Aufgabe_2_Skript_1.R")

df <- read.csv("titanic_Berichtigt.csv", header = TRUE, stringsAsFactors = FALSE)
df


# Wie ist das Alter der Gäste auf der Titanic verteilt gewesen ? 
func_spread_summary(df$Age)
# Range sagt uns das sowohl sehr junge, als auch ältere Menschen mit der Titanic gereist sind
# Der Variationskoeffizient (Varianz) sagen uns das im Allgemeinen das Alter breit gestreut war
# Da die InterquartilsRange "nur" bei 14 liegt bedeutet das, dass viele Werte mittig liegen
# Also viele Menschen eine Altersgruppe sind ~15 Jahre Unterschied

# Unterscheidet sich das Alter von Überlebenden und Verstorbenen ? 
# These: Alte Menschen und Kinder können nicht so gut schwimmen/ erfrieren schnell
# Im Eiswasser, auch wenn auf Ältere und Kinder bei der Rettung besondere Rücksicht 
# genommen wird sterben 

func_spread_summary(df[df$Survived == 0,]$Age)
func_spread_summary(df[df$Survived == 1,]$Age)
# Die Streuung der Verstorbenen / Überlebenden ist ähnlich, wie die Streuung von allen Gästen

# sind mehr Frauen oder mehr Männer auf dem Schiff gewesen ? 
func_categorial_summary(df$Sex)
# Mehr Männer 
# Wie sieht die Aufteilung bei den Überlebenden aus ? 
func_categorial_summary(df[df$Survived == 1,]$Sex)
# Mehr Frauen haben überlebt -> Frauen und Kinder zuerst 



# Wieviele Tickets jeder Klasse wurden gekauft ? 
# Wieviele Menschen haben überlebt/ Wieviel % sind verstorben
# Zu Geschlecht und Survived wurden ja schon deskriptive Statistiken berechnet
Visualisierung(df, "Sex", "Survived", "Pclass")
# 62% der Gäste der Titanic haben das Schiffsunglück leider nicht überlebt
# Über 50% der Gäste hatten dritte Klasse gebucht 

bivariate_stats(df, "Survived", "Sex")
# Hypothese: Vor allem die Gäste aus den günstigeren Klassen sind verstorben
bivariate_statsfkt(df, "Survived", "Pclass")
# Indiz für ja, da negative Korrelation zwischen Survived und Pclass
# 3 Klasse war Tiefer, ein längerer Weg aufs Deck zu den Rettungsbooten
# Außerdem wurden Schleusen geschlossen um ein weiteres Voranschreiten des Wassers
# im Boot zu verhindern, viele Menschen wurden wahrscheinlich im sinkenden Schiff eingesperrt

# Frage: Wer hatte eine bessere Überlebenschance Junge oder Alte Menschen ? 
bivariate_statsfkt(df, "Survived", "Age")
# Junge Menschen haben eine bessere Chance gehabt 

# Gibt es einen Unterschied zwischen der Korrelation zwischen "Male" und "Female"
bivariate_statsfkt(df[df$Sex == "male",], "Survived", "Age")
bivariate_statsfkt(df[df$Sex == "female",], "Survived", "Age")
# Ja es gibt einen Unterschied, der Wert geht "in die andere Richtung"
