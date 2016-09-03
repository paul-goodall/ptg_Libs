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
NormalDist <- function(x, mu, sig, height="auto"){
  x1 <- min(x)
  x2 <- max(x)
  dx1 <- sqrt((x1-mu)^2)
  dx2 <- sqrt((x2-mu)^2)
  dx <- max(dx1,dx2)
  x <- (0:1000-500)/500
  x <- x*dx
  ind <- (x-mu)^2
  denom <- 2*sig^2
  y <- exp(-ind/denom)/sqrt(denom*pi)
  if(height != "auto"){
    y <- y*height/max(y)
  }
  d <- data.frame(x,y)
}
## ============================================
ScatterPlot <- function(myOptions){
  myOptions <- SetOptions(myOptions)
  
  g <- ggplot(myOptions$data,(aes(x = x, y = y)))
  if(is.null(myOptions$MSizeLabels)){
  g <- g + geom_point(aes(colour = colours), size = myOptions$MSize)
  } else {
  g <- g + geom_point(aes(colour = colours, size = msize))
  }
  if(!(is.null(myOptions$SeriesColours))){
  g <- g + scale_color_manual(breaks=levels(myOptions$data$colours),values=myOptions$SeriesColours)
  }
  g <- g + labs(x=xt,y=yt,title=tt)

  return (g)
}   
## ============================================
SetOptions <- function(myOptions){
  if(is.null(myOptions$data)) stop("data is required.\n")
  myDF <- myOptions$data
  myDataNames <- names(myDF)
  tt <- myOptions$ttitle
  xt <- myOptions$xtitle
  yt <- myOptions$ytitle
  if(is.null(tt)) tt <- ""
  if(is.null(xt)) xt <- myDataNames[1]
  if(is.null(yt)) yt <- myDataNames[2]

  names(myDF)[1] <- "x"
  names(myDF)[2] <- "y"

  if(is.null(myOptions$SeriesLabels)){
    myDF$colours <- "all"
  } else {
    myDF$colours <- as.factor(myDF[myOptions$SeriesLabels][[1]])
  }
  
  if(is.null(myOptions$FacetLabels)){
    myDF$facets  <- ""
  } else {
    myDF$facets <- as.factor(myDF[myOptions$FacetLabels][[1]])
  }
  
  if(is.null(myOptions$FacetLabels)){
    myDF$facets  <- ""
  } else {
    myDF$facets <- as.factor(myDF[myOptions$FacetLabels][[1]])
  }
  
  if(is.null(myOptions$MSize)){
    myOptions$MSize <- 1
  }
  
  if(!is.null(myOptions$MSizeLabels)){
    myDF$msize <- as.numeric(myDF[myOptions$MSizeLabels][[1]])
    myDF$msize <- myDF$msize-min(myDF$msize)/(max(myDF$msize)-min(myDF$msize))
  }  
  
  myOptions$data <- myDF
  return (myOptions)
}  
## ============================================
BarPlot <- function(myOptions){
  myOptions <- SetOptions(myOptions)
  g <- ggplot(myOptions$data,(aes(x = x, y = y)))
  g <- g + geom_bar(aes(colour = colours))
  g <- g + labs(x=xt,y=yt,title=tt)
  return (g)
}    
## ============================================
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
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

































