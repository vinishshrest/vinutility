# Install packages not yet installed
fun_pack <- function(packages) {
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  else{
    print("installed")
  }
}
