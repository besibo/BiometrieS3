# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------  Séance 1  ------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

# Explication du principe de fonctionnement des TP et TEA.
# Les étudiants commencent la lecture et la découverte des tutoriels data camp.
# Ils commencent à faire les exercices qu'ils devront déposer sur l'ENT.

# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------  Séance 2  ------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

# Installation du package ggplot2
# install.packages(ggplot2)

# Mise en mémoire du package
library(ggplot2)

# Chargement du jeu de données diamonds
data(diamonds)

# Nombre de lignes de diamonds
diamonds
nrow(diamonds)
str(diamonds)

# Diamands dont le prix est supérieur à 15000 USD
## Première méthode :
diamants_chers <- diamonds[diamonds$price > 15000, ]
## Deuxième méthode :
diamants_chers <- subset(diamonds, diamonds$price > 15000)
## Troisième méthode
library(dplyr)
diamants_chers <- diamonds %>%
  filter(price > 15000)

# Nombre de diamants dont le prix dépasse 15000 USD
sum(diamonds$price > 15000)

# Proportion de diamants dont le prix dépasse 15000 USD
mean(diamonds$price > 15000)

# Tri du tableau par ordre de prix décroissants
## Première méthode
diamants_chers_tri <- diamants_chers[order(diamants_chers$price, decreasing = TRUE), ]
## Seconde méthode
diamants_chers_tri <- diamants_chers %>%
  arrange(desc(price))

# Affichage des 20 diamants les plus chers
head(diamants_chers_tri, n = 20)

# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------  Séance 3  ------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

# Mise en mémoire des packages utiles
library(dplyr)
library(nycflights13)

# Affichage du tableau flights dans un onglet de RStudio
View(flights)


# Exercices 3.3.1

  ## chacune des lignes du tableau corrspond aux données d'un vol (réponse B)
  ## Nombre de vols ayant décollé de JFK le 12 février 2013 : 282. On le détermine en ajustant les sliders, les filtres, etc.


# Exercices 3.3.2

  ## `tailnum` (numéro d'identification de l'avion), `origin` (aéroport de départ de l'avion), et `carrier` (compagnie aérienne) sont des variables catégorielles. Les valeurs sont des chaînes de caractères et non des chiffres.


# Exercices 3.4

  ## Consultez l’aide du jeu de données diamonds du package ggplot2.
  library(ggplot2)
  ?diamonds

    ## Quel est le code de la couleur la plus prisée ? D
    ## Quel est le code de la moins bonne clarté ? I1
    ## À quoi correspond la variable z ? La profondeur des diamands en millimètres
    ## En quoi la variable depth est-elle différente de la variable z ? Il s'agit d'un ratio entre z et la moyenne des 2 autres dimensions du diamands

  ## Consultez l’aide du package nycflights13 en tapant help(package="nycflights13").
  help(package="nycflights13")

  ## Consultez l’aide des 5 jeux de données de ce package.
  ?airlines
  ?airports
  ?flights
  ?planes
  ?weather

  ## À quoi correspond la variable visib ? Visibilité en miles
  ## Dans quel tableau se trouve-t’elle ? Weather
  ## Combien de lignes possède ce tableau ? 26115
  nrow(weather)


# Exercices 4.3

  ## alaska_flights contient les mêmes variables que flights, mais beaucoup moins de lignes : seulement 714.
  alaska_flights <- flights %>%
    filter(carrier == "AS")
  alaska_flights


# Exercices 4.3.3

  ## Donnez une raison pratique expliquant pourquoi les variables `dep_delay` et `arr_delay` ont une relation positive
    # Le retard au départ se répercute forcément sur le retard à l'arrivée. Sauf s'il est possible pour un avion de rattraper son retard en vol, ce qui semble peu probable.

  ## Quelles variables (pas nécessairement dans le tableau alaska_flights) pourraient avoir une corrélation négative (relation négative) avec dep_delay ? Pourquoi ? Rappelez-vous que nous étudions ici des variables numériques.
    ## On peut supposer que la température et le rayonnement solaire sont corrélés négativement à `dep_delay`. Quand les conditions météos se dégradent (moins de soleil, température plus faible) les retards augmentent car les avions et les pistes doivent être (re)mis en état plus souvent. Cela demande plus de maintenance.

  ## Selon vous, pourquoi tant de points sont-il regroupés près de (0, 0) ? La majorité des vols décollent et aterrisent à l'heure ou avec peu d'avance ou de retard
  ## À quoi le point (0,0) correspond-il pour les vols d’Alaska Airlines ? Cela correspond à la situation idéale de décollage et d'aterrissage à l'heure.
  ## Citez les éléments de ce graphique/de ces données qui vous sautent le plus aux yeux ? La relation est linéaire et positive. La majorité des vols est à peu près à l'heure, mais il y a une forte variabilité avec des vols accumulant plus de 3h de retard.
  ## Créez un nouveau nuage de points en utilisant d’autres variables du jeu de données alaska_flights
  ggplot(data = alaska_flights, mapping = aes(x = month, y = arr_delay)) +
    geom_point()


# Exercices 4.3.6

set.seed(4532) # Afin que tout le monde récupère les mêmes lignes
diams <- diamonds %>%
  sample_n(5000)

ggplot(diams, aes(x = carat, y = price, color = clarity)) +
  geom_point(shape = 4, alpha = 0.6)

# Les bandes verticales sont le résultats d'approximations humaines. On a tendance à arrondir au carat "rond" immédiatement supérieur (2 au lieu de 1.98, 1.7 au lieu de 1.69, etc).

# Exercices 4.4.2

small_weather <- weather %>%
  filter(origin == "EWR",
         month == 1,
         day <= 15)

small_weather

  ## Expliquez pourquoi la variable `time_hour` identifie de manière unique le moment ou chaque mesure a été réalisée alors que ce n’est pas le cas de la variable `hour`.
    ## `hour` est une variable entière qui prend les valeurs 0 à 23 chaque jour de l'année. Indiquer qu'une mesure a été faite à 13h n'est pas suffisant car elle a pu être faite n'importe que jour. La variable `time-hour` contient la date et l'heure de chaque mesure.



# Exercices 4.6.3

  ## Examinez la figure 4.37.

  ## Quels éléments nouveaux ce graphiques nous apprend-il par rapport au graphique 4.34 ci-dessus ? La nature bimodale de la distribution de la première figure cache un fait une distribution mensuelle des température unimodale tout à fait classique, et ce, pour les 3 aéroports
  ## Comment le “faceting” nous aide-t’il à visualiser les relations entre 2 (ou 3) variables ? En facilitant les comparaisons en lignes ou en colonnes
  ## À quoi correspondent les numéros 1 à 12 ? Les mois de l'année
  ## À quoi correspondent les chiffres 25, 50, 75, 100 ? Les températures en degrés farenheit
  ## À quoi correspondent les chiffres 0, 100, 200, 300 ? Au nombre d'observations pour chaque catégorie
  ## Observez les échelles des axes x et y pour chaque sous graphique. Qu’on-t’elles de particulier ? En quoi est-ce utile ? Elles sont toutes identiques. Elles facilitent ainsi les comparaisons entre sous-graphiques
  ## La variabilité des températures est-elle plus importante entre les aéroports, entre les mois, ou au sein des mois ? Expliquez votre réflexion. La variabilité des températures entre aéroports est très faible : pour un mois donné, les températures observées sont distribuées de façon très similaire dans les 3 aéroports. La variabilité entre les mois est plus importante : au sein d'un mois, la distribution des températures couvre rarement plus de 30º Farenheit alors qu'elle atteint presque 50 degrés Farenheit entre les mois d'été et les mois d'hiver.



# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------  Séance 4  ------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

# Exercices 4.8.3

  ## 1. Quelle est la différence entre un histogramme et un diagramme bâtons ?

    ## Un histogramme permet de visualiser la distribution d'une variable continue.
    ## Un diagramme bâtons permet de visualiser la distribution d'une variable catégorielle

  ## 2. Pourquoi les histogrammes sont-ils inadaptés pour visualiser des données catégorielles ?

    ## Parce que les barres sont collées les unes aux autres ce qui donne une impression visuelle de continuité.
    ## Par ailleurs, les barres ne peuvent pas être ré-ordonnées, ce qui est souvent nécessaire pour une variable catégorielle.

  ## 3. Quel est le nom de la companie pour laquelle le plus grand nombre de vols ont quitté New York en 2013 (je veux connaître son nom, pas juste son code) ? Où se trouve cette information ?

    ## On examine la figure 4.47 (par exemple) et on constate que la compagnie en question a le code UA
    airlines # Ce tableau fait le lien entre codes et nom des compagnies aériennes.
    ## La compagnie qui a affrété le plus grand nombre de vols au départ de New York en 2013 est United Air Lines Inc.

  ## 4. Quel est le nom de la companie pour laquelle le plus petit nombre de vols ont quitté New York en 2013 (je veux connaître son nom, pas juste son code) ? Où se trouve cette information ?

    ## Même démarche : il s'agit de la compagnie Skywest Airlines Inc. (code OO).

# ------------------------------------------------------------------------------------------------------------

# Exercices 4.8.4

  ## La figure 4.47 et la tableau carrier_table permettent de répondre aux questions suivantes

  ## Comparez les compagnies ExpressJet Airlines (EV) et US Airways (US). De combien de fois la part de EV est-elle supérieure à celle d’US ? (2 fois, 3 fois, 1.2 fois ?…)

    ## Un peu plus de 2,5

  ## Quelle est la troisième compagnie aérienne la plus importante en terme de nombre de vols au départ de New York en 2013 ?

    ## ExpressJet Airlines (EV)

  ## Combien de companies aériennes ont moins de vols que United Airlines (UA) ?

    ## Toutes, soit 15 au total

# ------------------------------------------------------------------------------------------------------------

# Exercices 4.10
## Mise en mémoire des packages nécessaires
library(tidyverse)
library(nycflights13)

## On fixe le générateur de nombres aléatoires pour choisir les mêmes lignes que dans l'exemple du livre
set.seed(1234)

## Création de small_flights
small_flights <- flights %>%
  sample_n(1000) %>%
  filter(!is.na(arr_delay),
         distance < 3000)

## Création du premier graphique
ggplot(small_flights, aes(x = distance, y = air_time, color = origin, shape = origin)) +
  geom_point(alpha = 0.8) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Distance parcourue en vol (miles)",
       y = "Temps de vol (minutes)",
       title = "Relation entre le temps de vol et la distance parcourue",
       subtitle = "Seuls les vols au départ de JFK et Newark parcourent plus de 1600 miles",
       color = "Aéroport de\nNew York",
       shape = "Aéroport de\nNew York",
       caption = "Données : small_flights") +
  theme_bw()

## Création du second graphique
ggplot(small_flights, aes(x = factor(month), fill = origin)) +
  geom_bar() +
  facet_wrap(~fct_infreq(carrier), nrow=4) +
  scale_fill_brewer(palette = "Accent") +
  labs(x = "Mois de l'année 2013",
       y = "Nombre de vols",
       title = "Évolution mensuelle du trafic aérien New Yorkais en 2013",
       subtitle = "Seules 9 compagnies aériennes sur 14 ont déservi New York toute l'année",
       fill = "Aéroport de\nNew York",
       caption = "Données : small_flights") +
  theme_bw()

# ------------------------------------------------------------------------------------------------------------

# Exercice 5.2.4

## Examinez les tableaux rates, storms et population du package EDAWR.

## Ces tableaux sont-ils des “tableaux rangés” (tidy data) ?
##  Si oui, quelles sont les variables représentées ?
##  Si non, transformez-les en “tableaux rangés”.

library(EDAWR)
rates  # Rates est un tableau rangé avec une variable par colonne (pays, année, nombre de cas de tuberculose, population globale, et taux de malades) et une ligne par observation

storms # Storms est un tableau rangé avec une variable par colonne (nom de la tempête, vitesse du vent en mph, pression en millibars et date à laquelle la plus forte vitesse de vent a été enregistrée) et une ligne par observation

population # Population n'est pas un tableau rangé. Les variables devraient être pays, année et population
gather(population, key = year, value = pop, `1995`:`2013`)

# ------------------------------------------------------------------------------------------------------------

# Exercice 5.3.4

  ## 1. L’objet dauphin est-il “tidy” (autrement dit, s’agit-il de “données rangées”) ? Justifiez.
  ## Oui : une variable par colonne, une observation par ligne. La variable ID pourrait être supprimées

  ## 2. Produisez le graphique ci-dessous

ggplot(dauphin <- aes(x = Age, y = Hg, color = Sexe)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~Organe, nrow=2, scales = "free_y") +
  labs(x = "Âge (années)",
       y = "Concentration en mercure (mg/kg)",
       title = "Évolution de la concentration en Mercure age l'âge chez Delphinus delphis",
       color = "Sexe",
       caption = "Données : dauphin.xls") +
  theme_bw()







# ------------------------------------------------------------------------------------------------------------
# -----------------------------------  Correction chapitre 6  ------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

# Calcul du nombre de vols en retard (+ de 30 min à l'arrivée) pour chaque compagnie et chaque mois
delayed <- flights %>%
  filter(arr_delay > 30) %>%
  group_by(carrier, month) %>%
  summarize(n_delayed = n()) %>%
  select(carrier, month, n_delayed)

# Calcul du nombre total de vols non annulés pour chaque compagnie et chaque mois
total <- flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(carrier, month) %>%
  summarize(n_total = n())

# Création du tableau de synthèse (2 left_join())
carrier_stats <- total %>%
  left_join(delayed) %>%
  mutate(n_delayed = replace_na(n_delayed, 0),
         rate = n_delayed / n_total) %>%
  left_join(airlines)
carrier_stats

# Création du tableau synthétique. Il faut dégrouper le tableau avec ungroup() pour ne pas avoir la variable carrier. J'arrondis aussi à 3 chiffres significatifs.
carrier_stats %>%
  ungroup() %>%
  select(name, month, rate) %>%
  mutate(rate = round(rate, 3)) %>%
  spread(month, rate)

# Création de graphiques de synthèse
carrier_stats %>%
  ggplot(aes(x = factor(month), y = rate, group = 1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~carrier, ncol=4)

# La compagnie OO (SkyWest Airlines Inc.) a un comportement très atypique dû au très faible nombre de vols affrétés (1 seul en janvier, 2 en juin, 4 en août, 17 en septembre et 5 en novembre). Elle n'est d'ailleurs présente que quelques mois de l'année dans les aéroports de New York
carrier_stats %>%
  filter(carrier == "OO")

# Pour les les plus grosses compagnies, les retards les plus fréquents sont observés l'été, et les moins fréquents en automne. Cela correspond aux périodes de très fortes affluences en été, et au moins fortes affluences à la rentrée de septembre. C'est du moins le cas pour UA et B6. Moins pour EV.

carrier_stats %>%
  filter(carrier %in% c("UA", "B6", "EV")) %>%
  select(carrier, month, n_total) %>%
  spread(carrier, n_total)

carrier_stats %>%
  filter(carrier %in% c("UA", "B6", "EV")) %>%
  ggplot(aes(x = factor(month), y = n_total, group = carrier, color = carrier)) +
    geom_line()

# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------




# ------------------------------------------------------------------------------------------------------------
# Misc.

library(tidyverse)
who <- read_csv("data/whoTB.csv")
who2 <- gather(who, key=categ, value = cases, new_sp_m014:new_rel_f65, na.rm = TRUE)
who3 <- separate(who2, col = categ, into = c("new_old", "type", "sex_age"))

who3 %>% count(type)
who3 %>% count(new_old)

who4 <- separate(who3, col = sex_age, into = c("sexe", "age"), sep = 1)
who5 <- select(who4, -iso2, -iso3, -new_old)

who5 %>%
  filter(country == "France") %>%
  ggplot(aes(year, cases, color = sexe)) +
    geom_point() +
    geom_smooth(method = "loess", size = 0.5, se = FALSE) +
    facet_wrap(~age, ncol = 2)


blah <- left_join(all_flights_origin, cancelled_origin, by = c("carrier", "origin")) %>%
  mutate(pc = n.y * 100 / n.x) %>%
  mutate(carrier = fct_reorder(carrier, pc, .desc = TRUE))

blah$carrier
blah %>% arrange(carrier)

blah %>%
  ggplot(aes(x = carrier, y = pc, fill = origin)) +
  geom_col(color = "black") +
  facet_wrap(~origin, ncol = 1) +
  labs(x = "Compagnie aérienne",
     y = "Proportion de vols annulés après décollage (%)",
     fill = "Aéroport de\nNew York") +
  scale_fill_brewer(palette = "Set1")


us <- map_data("state")
world <- map_data("world")
airp <- airports %>% filter(tz %in% (-8):(-5), lon < -50, lat < 50)

ggplot(data = us, aes(long, lat)) +
  geom_polygon(aes(group = group), fill = grey(.8), color = grey(.35)) +
  geom_point(data = airp, aes(lon, lat, fill = factor(tz)), shape = 21, color = grey(0.4), alpha = 0.5) +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Time zone",
       title = "Airport locations in the continental USA",
       subtitle = "Time zones are color coded")



flights_sml <- flights %>%
  select(year:day,
         ends_with("delay"),
         distance,
         air_time)

gain_speed <- flights_sml %>%
  transmute(gain = dep_delay - arr_delay,
            distance = distance * 1.60934,
            speed = (distance / air_time) * 60)

gain_speed %>%
  ggplot(aes(x = distance, y = gain)) +
    geom_point(alpha = 0.3)

gain_speed %>%
  ggplot(aes(x = speed, y = gain)) +
    geom_point(alpha = 0.3)

gain_speed %>%
  ggplot(aes(x = gain)) +
    geom_histogram(breaks = seq(-200, 100, 10), color="white")

# test --------------------------------------------------------------------
library(reprex)




### Autres exercices possibles ?
all_flights <- flights %>%
  count(carrier)
all_flights

left_join(all_flights, cancelled, by = "carrier")
inner_join(all_flights, cancelled, by = "carrier")
inner_join(all_flights, cancelled, by = "carrier") %>%
  mutate(pc = n.y * 100 / n.x) %>%
  arrange(desc(pc))

left_join(all_flights, cancelled, by = "carrier") %>%
  mutate(pc = n.y * 100 / n.x) %>%
  arrange(desc(pc))

left_join(all_flights, cancelled, by = "carrier") %>%
  mutate(pc = n.y * 100 / n.x) %>%
  ggplot(aes(x = fct_reorder(carrier, pc, .desc = TRUE), y = pc)) +
  geom_col() +
  labs(x = "Compagnie aérienne",
       y = "Proportion de vols annulés après décollage (%)")


cancelled_origin <- flights %>%
  filter(!is.na(dep_time),
         is.na(arr_time)) %>%
  count(carrier, origin)
cancelled_origin

all_flights_origin <- flights %>%
  count(carrier, origin)
all_flights_origin

left_join(all_flights_origin, cancelled_origin, by = c("carrier", "origin")) %>%
  mutate(pc = n.y * 100 / n.x) %>%
  ggplot(aes(x = fct_reorder(carrier, pc, .desc = TRUE), y = pc, fill = origin)) +
  geom_col(color = "black") +
  facet_wrap(~origin, ncol = 1) +
  labs(x = "Compagnie aérienne",
       y = "Proportion de vols annulés après décollage (%)",
       fill = "Aéroport de\nNew York") +
  scale_fill_brewer(palette = "Set1")



# ---------------------------------------------------------------
# Pieuvres
library(tidyverse)
library(magrittr)
squid <- read_delim("Corrections/squid.txt", "\t",
                    escape_double = FALSE, col_types = cols(Sex = col_integer()),
                    trim_ws = TRUE)

squid %<>%
  mutate(Sex = factor(Sex, labels=c("Male", "Female")),
         Location = factor(Location))


squid %>%
  group_by(Location, Month, Sex) %>%
  count() %>%
  spread(Location, n, fill = 0) %>%
  filter(Sex == "Male")

squid %>%
  group_by(Location, Month, Sex) %>%
  count() %>%
  spread(Location, n, fill = 0) %>%
  filter(Sex == "Female")



squid %>%
  group_by(Month, Sex) %>%
  count() %>%
  ggplot(aes(x = factor(Month), y = n, color = Sex, group = Sex)) +
    geom_line() +
    geom_point() +
    labs(x = "Mois de l'année",
         y = "Nombre de pieuvres collectées",
         color = "Sexe")


# ---------------------------------------------------------------

# Dauphins
library(readxl)
dauphin <- read_excel("data/dauphin.xls", na = "*", skip = 9) %>%   # Importation, puis
  rename(ID = `N°`,                          # On donne des noms courts, puis
         Statut = `Statut reproducteur`,
         Taille = `Taille en cm`,
         Age = `Age en années`,
         Cd = `Cd (mg.kg-1)`,
         Cu = `Cu (mg.kg-1)`,
         Hg = `Hg (mg.kg-1)`) %>%
  mutate_if(is.character, as.factor) %>%    # On transforme en facteurs les variables <chr>, puis
  mutate(Sexe = fct_recode(Sexe,            # On modifie les niveau de `Sexe` et l'ordre des niveaux de Statut
                           "Female" = "f",
                           "Male" = "m"),
         Statut = fct_relevel(Statut, "imm", "mat", "pnl", "l", "pl", "repos"))

dauphin

dauphin %>%
  count(Statut, Sexe) %>%
  spread(Sexe, n)

dauphin %>%
  filter(Sexe == "Female") %>%
  ggplot(aes(x = Statut, y = Hg)) +
    geom_boxplot()

# ---------------------------------------------------------------

# BCI and apply alternative
library(purrr)
library(vegan)
data(BCI)
BCI <- as_tibble(BCI)
BCI

- sum(BCI[1,] / sum(BCI[1,]) * log(BCI[1,] / sum(BCI[1,])), na.rm=TRUE)


BCI %>%
  mutate(Shannon = pmap(., ~ - sum(c(...) / sum(c(...)) * log(c(...) / sum(c(...))), na.rm=TRUE))) %>%
  select(Shannon) %>%
  unnest()

apply(BCI, 1, function(x) - sum(x / sum(x) * log(x / sum(x)), na.rm=TRUE))


BCI2 <- as_tibble(t(BCI))

BCI2 %>%
  summarize_all(~ -sum(. / sum(.) * log(. / sum(.)), na.rm=TRUE)) %>%
  gather(key = Parcelle, value = Shannon, 1:50, convert = TRUE)

BCI2 %>%
  summarize_all(~sum(.!=0))




