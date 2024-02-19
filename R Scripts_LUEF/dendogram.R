tab <- read.csv('tabela_2.csv', sep=";", h=T)

tab <- tab |> dplyr::select(LU_br,Ind) |> dplyr::group_by(LU_br) |> dplyr::summarise(I=mean(Ind))


tab_pad <- vegan::decostand(tab[-1],method="standardize",na.rm=T)


tab_pad_eu <- vegan::vegdist(tab_pad, "euclidean")

ward <- hclust(tab_pad_eu, method="ward.D")
rotulo= tab$LU_br



plot(ward,
     ylab = "Distancia euclidiana",
     xlab=" Classe",
     main='Dendograma',
     hang = -1,
     col="blue", las=1,
     cex=.6,lwd=1.5,
     labels = rotulo)


