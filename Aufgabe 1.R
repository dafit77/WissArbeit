#1. Teilaufgabe 
#Extrahiert aus dem Namen eine Variable mit der Anrede der Person

titanic$Title <- sub(".*,\\s*(.*?)\\..*", "\\1", titanic$Name) 
#erfasst alles zwischen Komma und Punkt in der Spalte "Name" und packt es in die Spalte "Title"
