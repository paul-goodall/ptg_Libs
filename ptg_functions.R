## ============================================
"&" = function(x,y) {
    return(paste0(x,y))
}
## ============================================
Exe <- function(myCom,x=1){
  if(x == 0){
    cat("\n======\n",myCom,"\n======\n")
  }
  if(x == 1){
    system(myCom)
  }
  if(x == 2){
    cat("\n======\n",myCom,"\n======\n")
    system(myCom)
  }
}
## ============================================
LEFT <- function(myS,N){
  substr(myS,1,N)
}
## ============================================
RIGHT <- function(myS,N){
  n2 <- nchar(myS)
  n1 <- n2-N+1
  substr(myS,n1,n2)
}
## ============================================
SPLIT <- function(myS,myX){
  strsplit(myS,myX,fixed = TRUE)[[1]]
}
## ============================================
padZeros <- function(nn,numZ){
  sprintf(paste0("%0",numZ,"d"), nn)
}
## ============================================
file2string <- function(myFile){
  myFile <- as.character(read.csv(myFile,sep="\n")[,1])
  myFile <- paste0(myFile,collapse="\n")
  myFile
}
## ============================================
string2file <- function(myS,myF){
  cat(myS,file=myF)
}
## ============================================
tStamp <- function(){
  format(Sys.time(), "%H%M%S")
}
## ============================================
dStamp <- function(){
  format(Sys.time(), "%Y-%m-%d")
}
## ============================================
dtStamp <- function(){
  format(Sys.time(), "%Y-%m-%d_%H%M%S")
}
## ============================================
dates2mjd <- function(dates, dformat = "%Y-%m-%d")
{
  dates <- as.Date(dates,format=dformat)
  unclass(dates)
}
## ============================================
dates2int <- function(dates, dformat = "%Y-%m-%d")
{
  dates  <- as.Date(dates,format=dformat)
  years  <- as.numeric(format(dates, "%Y"))
  months <- as.numeric(format(dates, "%m"))
  days   <- as.numeric(format(dates, "%d"))
  dates  <- years*10000+months*100+days
}
## ============================================
wget <- function(url,fname) {
  if(!file.exists(fname)){
    download.file(url,destfile=fname,method="curl")
  }
}
## ============================================
gitInit <- function(localPath,myRepoName){
  myGithubURLbase <- "https://github.com/pgoodall1984"
  localPath <- localPath & "/myRepoName"
  myComment <- "Initial commit at [" & dtStamp() & "]"    
  com <- ""
  com <- com & "mkdir localPath;"
  com <- com & "echo '# New Repo: myRepoName' >> localPath/README.md;" 
  com <- com & "git -C localPath init;"  
  com <- com & "git -C localPath add README.md;"  
  com <- com & "git -C localPath commit -m 'myComment';"  
  com <- com & "git -C localPath remote add origin myGithubURLbase/myRepoName.git;"   
  com <- com & "git -C localPath push -u origin master;"  
  com <- gsub(";",";\n",com)
  com <- gsub("localPath",localPath,com)
  com <- gsub("myComment",myComment,com)
  com <- gsub("myRepoName",myRepoName,com)
  com <- gsub("myGithubURLbase",myGithubURLbase,com)
  Exe(com,x=2)  
}
## ============================================
gitPush <- function(localPath,myComment=NULL){
  if(is.null(myComment)){
    myComment = "Update at [" & dtStamp & "]"    
  }
  com <- ""
  com <- com & "git -C localPath add -A;" 
  com <- com & "git -C localPath commit -m 'myComment';"
  com <- com & "git -C localPath push;"    
  com <- gsub("localPath",localPath,com)
  com <- gsub("myComment",myComment,com)
  system(com)
}
## ============================================
gitPull <- function(localPath){
  com <- ""
  com <- com & "git -C localPath pull;"    
  com <- gsub("localPath",localPath,com)
  system(com)
}
## ============================================



## ============================================
## ============================================
## ============================================
## ============================================
## ============================================
## ============================================
## ============================================
## ============================================
## ============================================
## ============================================

































