install.packages("stringi")
install.packages("missForest")
#install.packages("stringr")
install.packages("mltools")
install.packages("tidymodels")
install.packages("clValid")
install.packages("NbClust")
BiocManager::install("M3C")
install.packages("treeClust")
install.packages("kamila")
install.packages("clustMixType") # for kprototype
install.packages("mclust") # for the adjusted rand index
install.packages("arsenal")
install.packages("fmsb") # for radar plot
install.packages("colormap")
install.packages("GGally")
install.packages("ggthemes")
install.packages("hrbrthemes")
install.packages("randomForest")
# utilities

install.packages("factoextra") # for the fviz functions
devtools::install_github("ranibasna/NumericalTransformation")

# reproduciblity
install.packages("renv")

# we need to install BicoMnagere since diceR needs the NMF package that depends on the Biobase that only exits with BiocManager
install.packages("BiocManager")
BiocManager::install("Biobase")
install.packages('NMF')
# install.packages("diceR", dependencies = TRUE)
install.packages("diceR")


BiocManager::install("M3C")
# install.packages("devtools")
# devtools::install_github("AlineTalhouk/diceR")



