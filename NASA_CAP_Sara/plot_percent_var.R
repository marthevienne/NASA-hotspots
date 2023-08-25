plot_percent_var <- function(pca, pc){
  # Calcule du pourcentage de variance
  percent_var_explained <- (pca$sdev^2 / sum(pca$sdev^2))*100
  # Préparation d'un tableau avec le numéro des composantes principales 
  # et le pourcentage de variance qui lui est associé
  percent_var_explained <- data.frame(
    PC=1:length(percent_var_explained),
    percent_Var=percent_var_explained
  )
  # Récupérer uniquement le nombre de PC indiqué en argument
  sub_percent_var_explained <- percent_var_explained[1:pc,]
  # Génère le graphique
  p <- ggplot(sub_percent_var_explained, aes(x=PC, y=percent_Var)) + 
    # Génère un barplot
    geom_col()+
    # Utilise le thème "black and white"
    theme_bw() +
    # Renomme l'axe des abscisses
    xlab("PCs") +
    # Renomme l'axe des ordonnées
    ylab("% Variance") +
    # Titre du graphique
    ggtitle("Screeplot")+
    # Option de taille des éléments textuels
    theme(
      axis.text=element_text(size=16),
      axis.title=element_text(size=16),
      legend.text = element_text(size =16),
      legend.title = element_text(size =16 ,face="bold"),
      plot.title = element_text(size=18, face="bold", hjust = 0.5),
      # Astuce pour garder un graphique carré
      aspect.ratio=1
    )
  # Affiche le graphique
  print(p)
}
