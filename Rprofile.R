cat("Good job on using the GitHub RProfile! :)\n")

gitSource <- function(myRepo,myFile){
  myURL <- "https://raw.githubusercontent.com/paul-goodall/myRepo/master/myFile"
  myURL <- gsub("myRepo",myRepo,myURL)
  myURL <- gsub("myFile",myFile,myURL)
  source(myURL)
}
