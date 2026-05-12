####################
## Memoire RH     ##
####################

#1. Librairies et chemins
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")      
install.packages("stats")      

library(readxl)
library(dplyr)
library(ggplot2)
library(psych)
library(stats)

# rm(list = ls())

isol <- read_excel("C:/Users/emmac/Desktop/alt_isol.xlsx", skip = 1)
pilot <- read_excel("C:/Users/emmac/Desktop/alt_pilot.xlsx", skip = 1)

#2. Créeation des scores composites pour chaque répondant car pas sur 100
# Pour les isolés
isol <- isol %>%
  mutate(
    attractivite = rowSums(select(., 2:16), na.rm = TRUE) / 105 * 100,
    engagement_affectif = `Total Engagement affectif/56` / 56 * 100,
    engagement_calcule = `Total EC/56` / 56 * 100,
    engagement_total = (engagement_affectif + engagement_calcule) / 2
  )

# Pour les pilotés
pilot <- pilot %>%
  mutate(
    attractivite = rowSums(select(., 2:16), na.rm = TRUE) / 105 * 100,
    engagement_affectif = `Engagement affectif score total sur 56` / 56 * 100,
    engagement_calcule = `Engagement calculé sur 56` / 56 * 100,
    engagement_total = (engagement_affectif + engagement_calcule) / 2
  )

#3. H1 : Corrélation entre attractivité et engagement
#Pour les isoles
cor.test(isol$attractivite, isol$engagement_total, method = "pearson")

#H0 (hypothèse nulle) : il n’y a pas de corrélation entre attractivité perçue et engagement → ρ = 0
#H1 (hypothèse alternative) : il existe une corrélation significative entre attractivité perçue et engagement → ρ ≠ 0


cor.test(pilot$attractivite, pilot$engagement_total, method = "pearson")


#4. H2 : Corrélation entre attractivité et engagement
isol$groupe <- "isolé"
pilot$groupe <- "piloté"

#fusion 2 BDD
merged_data <- bind_rows(isol, pilot)

#ANOVA - t-test de Welch (t-test à 2 groupes avec variances inégales autorisées)
t.test(attractivite ~ groupe, data = merged_data) #pour attractivite t= -1.62 et pval = 	0.1344 

t.test(engagement_total ~ groupe, data = merged_data) #pour engagement
# t= -0.56 et p-value	0.5867 -->  p > 0.05 → H0 non rejetée --> Pas de différence significative d’engagement entre les deux groupes.

# pour attractivite
ggplot(merged_data, aes(x = groupe, y = attractivite, fill = groupe)) +
  geom_boxplot() +
  labs(title = "Comparaison de l’attractivité perçue",
       x = "Groupe d'alternants",
       y = "Score d'attractivité (sur 100)") +
  theme_minimal()

#pour engagement
ggplot(merged_data, aes(x = groupe, y = engagement_total, fill = groupe)) +
  geom_boxplot() +
  labs(title = "Comparaison de l’engagement total",
       x = "Groupe d'alternants",
       y = "Score d’engagement (sur 100)") +
  theme_minimal()

#Moyenne d’engagement total par groupe

summary_data <- merged_data %>%
  group_by(groupe) %>%
  summarise(mean_eng = mean(engagement_total, na.rm = TRUE),
            sd_eng = sd(engagement_total, na.rm = TRUE),
            n = n(),
            se_eng = sd_eng / sqrt(n))

ggplot(summary_data, aes(x = groupe, y = mean_eng, fill = groupe)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
  geom_errorbar(aes(ymin = mean_eng - se_eng, ymax = mean_eng + se_eng),
                width = 0.2, position = position_dodge(0.6)) +
  labs(title = "Moyenne d’engagement total par groupe",
       x = "Groupe",
       y = "Engagement (moyenne ± SE)") +
  theme_minimal()


#5. Comparaison des moyennes d’engagement entre groupes avec des correlations mais je dois renommer les catégories
isol <-isol %>%
  mutate(
    valeur_attrait = rowMeans(select(., matches("^Valeur d[’']?attrait", ignore.case = TRUE)), na.rm = TRUE),
    valeur_sociale = rowMeans(select(., matches("^Valeur social", ignore.case = TRUE)), na.rm = TRUE),
    valeur_economique = rowMeans(select(., matches("^Valeur économique", ignore.case = TRUE)), na.rm = TRUE),
    valeur_developpement = rowMeans(select(., matches("^Valeur de développement", ignore.case = TRUE)), na.rm = TRUE),
    valeur_application = rowMeans(select(., matches("^Valeur application des connaissances", ignore.case = TRUE)), na.rm = TRUE),
    
    engagement_affectif_detail = rowMeans(select(., matches("^Engagement affectif\\.", ignore.case = TRUE)), na.rm = TRUE),
    engagement_calcule_detail = rowMeans(select(., matches("^Engagement calculé\\.", ignore.case = TRUE)), na.rm = TRUE)
  )

pilot <- pilot %>%
  mutate(
    valeur_attrait = rowMeans(select(., matches("^Valeur d[’']?attrait", ignore.case = TRUE)), na.rm = TRUE),
    valeur_sociale = rowMeans(select(., matches("^Valeur social", ignore.case = TRUE)), na.rm = TRUE),
    valeur_economique = rowMeans(select(., matches("^Valeur économique", ignore.case = TRUE)), na.rm = TRUE),
    valeur_developpement = rowMeans(select(., matches("^Valeur de développement", ignore.case = TRUE)), na.rm = TRUE),
    valeur_application = rowMeans(select(., matches("^Valeur application des connaissances", ignore.case = TRUE)), na.rm = TRUE),
    
    engagement_affectif_detail = rowMeans(select(., matches("^Engagement affectif\\.", ignore.case = TRUE)), na.rm = TRUE),
    engagement_calcule_detail = rowMeans(select(., matches("^Engagement calculé\\.", ignore.case = TRUE)), na.rm = TRUE)
  )

isol$groupe <- "isolé"
pilot$groupe <- "piloté"
merged_data <- bind_rows(isol, pilot)

#Correlation entre chaque valeur et son engagement
cor.test(merged_data$valeur_attrait, merged_data$engagement_total) #0.3374055 
cor.test(merged_data$valeur_sociale, merged_data$engagement_total)  #0.5763685
cor.test(merged_data$valeur_economique, merged_data$engagement_total) #0.7414111
cor.test(merged_data$valeur_developpement, merged_data$engagement_total) #0.4293748 
cor.test(merged_data$valeur_application, merged_data$engagement_total) #0.5145187


#Représentation visuelle de valeur économique et engagement total car correlation positive
ggplot(merged_data, aes(x = valeur_economique, y = engagement_total)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Lien entre la valeur économique perçue et l’engagement total",
    x = "Valeur économique perçue",
    y = "Engagement total (sur 100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12)
  )

#Régression multiple pour voir quelles valeurs expliquent le plus l’engagement
modele <- lm(engagement_total ~ valeur_attrait + valeur_sociale + valeur_economique + 
               valeur_developpement + valeur_application,
             data = merged_data)
summary(modele)

# Barplot des coefficients de la régression
library(broom)
library(ggplot2)

tidy(modele) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Impact des valeurs perçues sur l'engagement",
       x = "Dimensions de l'attractivité perçue",
       y = "Coefficient de régression (β)") +
  theme_minimal()

# 6. Corrélations croisées (matrice)
# Sélection des variables
cor_mat <- merged_data %>%
  select(valeur_attrait, valeur_sociale, valeur_economique,
         valeur_developpement, valeur_application,
         engagement_total, engagement_affectif_detail, engagement_calcule_detail)

# Matrice de corrélation
correlation_results <- corr.test(cor_mat, method = "pearson")
print(correlation_results$r)    # Matrice des corrélations
print(correlation_results$p)    # Matrice des p-values


install.packages("corrplot")
library(corrplot)

# Créer la matrice de corrélation (sur les bonnes colonnes)
cor_matrix <- cor(merged_data[, c("valeur_attrait", "valeur_sociale", "valeur_economique", 
                                  "valeur_developpement", "valeur_application",
                                  "engagement_total", "engagement_affectif_detail", "engagement_calcule_detail")],
                  use = "complete.obs")

# Affichage avec couleurs
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.9,
         addCoef.col = "black", # pour ajouter les coefficients
         col = colorRampPalette(c("red", "white", "blue"))(200),
         number.cex = 0.7)

# Corrélations avec l’engagement affectif
corr_affectif <- c(
  cor(merged_data$valeur_attrait, merged_data$engagement_affectif_detail, use = "complete.obs"),
  cor(merged_data$valeur_sociale, merged_data$engagement_affectif_detail, use = "complete.obs"),
  cor(merged_data$valeur_economique, merged_data$engagement_affectif_detail, use = "complete.obs"),
  cor(merged_data$valeur_developpement, merged_data$engagement_affectif_detail, use = "complete.obs"),
  cor(merged_data$valeur_application, merged_data$engagement_affectif_detail, use = "complete.obs")
)
labels_valeurs <- c("valeur_attrait", "valeur_sociale", "valeur_economique", "valeur_developpement", "valeur_application")
df_affectif <- data.frame(
  valeur = labels_valeurs,
  correlation = corr_affectif
)

ggplot(df_affectif, aes(x = reorder(valeur, correlation), y = correlation, fill = correlation > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "turquoise", "FALSE" = "salmon")) +
  labs(title = "Corrélations : Dimensions perçues et Engagement affectif",
       x = "Dimensions de l’attractivité perçue",
       y = "Corrélation (r de Pearson)") +
  theme_minimal()


# Corrélations avec engagement calculé
corr_calcule <- c(
  cor(merged_data$valeur_attrait, merged_data$engagement_calcule_detail, use = "complete.obs"),
  cor(merged_data$valeur_sociale, merged_data$engagement_calcule_detail, use = "complete.obs"),
  cor(merged_data$valeur_economique, merged_data$engagement_calcule_detail, use = "complete.obs"),
  cor(merged_data$valeur_developpement, merged_data$engagement_calcule_detail, use = "complete.obs"),
  cor(merged_data$valeur_application, merged_data$engagement_calcule_detail, use = "complete.obs")
)

df_calcule <- data.frame(
  valeur = labels_valeurs,
  correlation = corr_calcule
)

# Graphique
ggplot(df_calcule, aes(x = reorder(valeur, correlation), y = correlation, fill = correlation > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "turquoise", "FALSE" = "salmon")) +
  labs(title = "Corrélations : Dimensions perçues et Engagement calculé",
       x = "Dimensions de l’attractivité perçue",
       y = "Corrélation (r de Pearson)") +
  theme_minimal()


#Interaction entre l’attractivité perçue et le groupe (isolé vs piloté), sur l’engagement total :

library(ggplot2)

# Graphe d'interaction : attractivité × groupe → engagement
ggplot(merged_data, aes(x = attractivite, y = engagement_total, color = groupe)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, fullrange = TRUE) +
  labs(
    title = "Interaction entre attractivité perçue et groupe sur l’engagement",
    x = "Attractivité perçue (score sur 100)",
    y = "Engagement total (score sur 100)",
    color = "Groupe"
  ) +
  theme_minimal()



