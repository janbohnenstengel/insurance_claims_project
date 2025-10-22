Dieses Projekt war Teil des Kurses „Business Analytics“ meines Masters. In einem fiktiven Szenario sollten wir als beratende Datenanalyst:innen die Prozesse eines Versicherungsunternehmens in Bezug auf Betrugserkennung  überprüfen.
Dazu erhielten wir einen Übungsdatensatz mit 100.000 Versicherungsfällen, wobei 80.000 als betrügerisch oder echt gelabelt waren. Bei den restlichen 20.000 fehlte dieses Label. Unsere Aufgabe bestand darin, die Daten deskriptiv zu analysieren, auf Muster zu untersuchen und dann einen Bericht für unseren fiktionalen Auftraggeber (den Professor) zu verfassen.
In einem zweiten Schritt sollten wir ein Modell entwickeln, um für die 20.000 nicht gekennzeichneten Fälle vorherzusagen, ob diese Betrugsfälle waren oder nicht. Ich habe sowohl ein Random-Forest- als auch ein XGBoost-Modell getestet. Letzteres habe ich am Ende für die Vorhersagen genutzt. Zusätzlich enthält die Datei zwei Benchmark-Modelle zum Vergleich der Ergebnisse.
Eingereicht wurde nur der PDF-Bericht, kein Code.

Insurance_EDA enthält die Datenmanipulation sowie deskriptive Analysen. 

Insurance_modelling beinhaltet die Modelle, Vorhersagen und einen SHAP-Graphen.

Bohnenstengel_Jan_Business_Report ist der Bericht, der am Ende eingereicht wurde.

Simple_descriptive_dashboard ist ein kleines, einfaches, deskriptives Shiny Dashboard.
