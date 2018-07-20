# FRIDAYLAB
options(scipen = 999)

# Nettoyage du fichier
nettoyer_fichier = function(fichier, annee){
  fichier$Année = annee
  colnames_an_passe = colnames(fichier)[grepl(annee-1, colnames(fichier))]
  fichier = subset(fichier,select = setdiff(colnames(fichier), colnames_an_passe))
  colnames(fichier) = gsub('Catégorie', 'Catégorie_code', colnames(fichier))
  colnames(fichier) = gsub('Libellé.catégorie', 'Catégorie', colnames(fichier))
  fichier[, 'Catégorie'] = gsub('^0$', '', fichier[, c('Catégorie')])
  
  for (c in c('AEPLF', 'CPPLF')){
    fichier[, c] = gsub('\\s+', '', fichier[, c])
    fichier[, c] = as.numeric(fichier[, c])
  }
  return(fichier)
}


### PLF 2018
plf = read.csv2("PLF2018-BG-Msn-Nat.csv", stringsAsFactors = FALSE, skip = 3)
plf$Type_budget = "Budget général"
plf = nettoyer_fichier(plf, 2018)

### CS et CF
cf = read.csv2("PLF2018-CF-Msn-Nat.csv", stringsAsFactors = FALSE, skip = 3)
cf = nettoyer_fichier(cf, 2018)
cf$Type_budget = "Comptes financiers"

cs = read.csv2("PLF2018-CS-Msn-Nat.csv", stringsAsFactors = FALSE, skip = 3)
cs = nettoyer_fichier(cs, 2018)
cs$Type_budget = "Comptes spéciaux"

budgets = rbind(plf, cf, cs)

write.csv2(budgets, 'budgets_retraites.csv', row.names = F)
