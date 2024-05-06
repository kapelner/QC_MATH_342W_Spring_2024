.onLoad = function(libname, pkgname){
  packageStartupMessage(paste(
	"\nWelcome to QCbasementGBM v", utils::packageVersion("QCbasementGBM"), ".\n", 
	sep = ""
  ))
}
