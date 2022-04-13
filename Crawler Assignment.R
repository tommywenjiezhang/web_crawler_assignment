#Write a topical crawler using the information provided below:
list.of.packages <- c("xml", "RCurl", "stringr", "httr", "xml2", "httr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


##Start your code with these libraries:
library(RCurl)
library(XML)
library(stringr)
library(httr)
library(xml2)
library(stringi)


htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}




invalid_utf8_ <- function(x){
  
  !is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8"))
  
}

detect_invalid_utf8 <- function(string, seperator){
  
  stringSplit <- unlist(strsplit(string, seperator))
  
  invalidIndex <- unlist(lapply(stringSplit, invalid_utf8_))
  
  data.frame(
    word = stringSplit[invalidIndex],
    stringIndex = which(invalidIndex == TRUE)
  )
  
}



###Run the function code for htmlToText()(Be sure this function is listed in your Environment)

###Load the first element in the frontier to an "exploredlink" variable


frontier <- c("https://www.cnn.com","https://www.kdnuggets.com/","http://news.google.com")



topicwords<-c("technology","school","web","mining","news")

num <- 50 #total number of items to crawl
result <- c()
j <- 0  #number of items in the repository


while (j < num){
  

  if(length(frontier)<1){
    break
  }
  
  #grab the first item in the frontier and place in the "exploredlink" variable
  exploredlink<-frontier[1]
  frontier<-frontier[-1]
  
  if(str_detect(exploredlink,"\\.jpg$"))
  {
    next
  }

  
  #fill in your code here
  #How to get HTML
  doc <- tryCatch(getURL(exploredlink,.encoding = 'UTF-8'),error=function(cond){return("")})
  
  if(invalid_utf8_(doc)){
    print("invalid page ")
    next
  }
  
  
  if(tryCatch(str_length(doc), error=function(cond){return(0)})<10){
    print("<10")
    next
  }
  
  doc <- htmlParse(doc)
  
  domain<-str_extract(exploredlink,pattern = ".*\\.com")
  
  if(is.null(domain)){
    next
  }
  
  #How to get a title
  titleText <- tryCatch(xmlToDataFrame(nodes = getNodeSet(doc, "//title")),error=function(cond){return("")})
  
  if(is.null(titleText) ){
    next
  }
  titleText <- as.vector(titleText$text)
  titleText <- unique(titleText)
  titleText <- paste(str_trim(str_replace_all(titleText, "[^[:alnum:]]", " ")), collapse = '')
  
  #How to get body text 
  
  bodyText<- tryCatch(stri_enc_toutf8(htmlToText(content(GET(exploredlink),type="text/html",as="text"))),error=function(cond){return("")})
  bodyText<-str_split(tolower(str_replace_all((str_replace_all(bodyText,"(\\t|\\n|\\r)"," ")),"\\s{2,}"," "))," ")[[1]]
  ###  write to file
  for (val in topicwords)
  {
    if(val %in% bodyText){
      path <- paste(titleText,".html")
      write.table(bodyText, 
                  file= path, 
                  quote = FALSE,
                  col.names = FALSE,
                  row.names = FALSE)
      write(c(exploredlink,titleText), file="repositary.txt",append=TRUE,sep = "\t")
      next
    }
  }
  
  print(exploredlink)
  
  
  
  #How to get links from a page
  anchor <- getNodeSet(doc, "//a")
  anchor <- sapply(anchor, function(x) xmlGetAttr(x, "href"))
  
  if(length(anchor)>0){
    temp <- c()
    for(i in 1:length(anchor)){
      if(is.null(anchor[[i]])){
        next
      }
      if(!str_detect(anchor[[i]][1],"^http")){
        next
      }
      if(str_detect(anchor[[i]][1],domain)){
        next
      }
      temp <- append(temp,str_trim(anchor[[i]][1]))
    }
    anchor <- temp
    rm(temp)
    
    frontier<-append(frontier,anchor)
    frontier <- unique(frontier)
  }
  
  

}





# show number of html pages 
library(system)
system("echo 'number of html pages: ' $(ls -l *.html | wc -l) ")

############   USEFUL CODE SNIPPETS ########

#How to get HTML
doc <- tryCatch(getURL(exploredlink),error=function(cond){return("")})


if(str_length(doc)<10){
  next
}

doc <- htmlParse(doc)

domain<-str_extract(exploredlink,pattern = ".*\\.com")

if(is.na(domain)){
  next
}

###


#How to get a title
titleText <- tryCatch(xmlToDataFrame(nodes = getNodeSet(doc, "//title")),error=function(cond){return("")})
if(titleText==""){
  next
}
titleText <- as.vector(titleText$text)
titleText <- unique(titleText)

###



#How to get body text  
bodyText<- tryCatch(htmlToText(content(GET(exploredlink),type="text/html",as="text")),error=function(cond){return("")})

bodyText<-str_split(tolower(str_replace_all((str_replace_all(bodyText,"(\\t|\\n|\\r)"," ")),"\\s{2,}"," "))," ")[[1]]

###



#How to get links from a page
anchor <- getNodeSet(doc, "//a")
anchor <- sapply(anchor, function(x) xmlGetAttr(x, "href"))

if(length(anchor)>0){
  temp <- c()
  for(i in 1:length(anchor)){
    if(is.null(anchor[[i]])){
      next
    }
    if(!str_detect(anchor[[i]][1],"^http")){
      next
    }
    if(str_detect(anchor[[i]][1],domain)){
      next
    }
    temp <- append(temp,str_trim(anchor[[i]][1]))
  }
  anchor <- temp
  rm(temp)
  
  frontier<-append(frontier,anchor)
  frontier <- unique(frontier)
}

###


