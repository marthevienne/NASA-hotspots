plot_pca <- function(pca=pca, pc=pc, 
                     conditions=conditions, 
                     colours=colours,
                     size = size,
                     labels = labels, 
                     title = title){
  # Transforme le nombre de PC en argument en nom de PC 
  PCs <- paste("PC",1:pc, sep="")
  # Calcule le pourcentage de variance par PC
  percent_var_explained <- (pca$sdev^2 / sum(pca$sdev^2))*100
  # Transforme le vecteur de conditions en un facteur
  cond <- factor(conditions)
  # Crée un autre facteur avec les conditions
  col <- factor(conditions)
  # Change les niveaux du facteur avec la palette de couleur pour attribuer
  # à chaque condition une couleur
  levels(col) <- colours
  # Re-transforme le facteur en vecteur
  col <- as.vector(col)
  # Récupère les scores pour le graphique
  scores <- as.data.frame(pca$x)
  # Génère toutes les combinaisons possibles de PC 
  PCs.combinations <- combn(PCs,2)
  # Génère un graphique pour chaque combinaison
  # avec une boucle apply
  g <- apply(
    PCs.combinations,
    2,
    function(combination)
    {
      p1 <- ggplot(scores, aes_string(x=combination[1], y=combination[2])) +
        # Dessine des points avec une bordure de 0.5 remplis avec une couleur
        geom_point(shape = 21, size = size, stroke=0.5, aes(fill=cond)) +
        # Utilise le thème "black and white"
        theme_bw() +
        # Spécifie la palette de couleur et donne un titre vide à la légende
        scale_fill_manual(
          values=colours,
          name="",
          labels = labels
        ) +
        # Renomme le titre des axes des abscisses et des ordonnées en "PCx (pourcentage de variance)" avec 3 chiffres après la virgule
        xlab(paste(combination[1], " (",round(percent_var_explained[as.numeric(gsub("PC", "", combination[1]))], digit=3),"%)", sep=""))+
        ylab(paste(combination[2], " (",round(percent_var_explained[as.numeric(gsub("PC", "", combination[2]))], digit=3),"%)", sep=""))+
        # Titre du graphique
        ggtitle(title)+
        # Option de taille des éléments texte
        theme(
          axis.text=element_text(size=16),
          axis.title=element_text(size=16),
          legend.text = element_text(size =16),
          legend.title = element_text(size =16 ,face="bold"),
          plot.title = element_text(size=18, face="bold", hjust = 0.5),
          # Astuce pour garder un graphique carré
          aspect.ratio=1
        ) +
        guides(fill = guide_legend(override.aes = list(size = 4)))
      # Affiche le graphique
      print(p1)
    }
  )
}
