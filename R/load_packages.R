
load_packages <-  function(packages){
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}else{
  print("All packages installed")
}

invisible(lapply(packages, library, character.only = TRUE)) #load packages
}