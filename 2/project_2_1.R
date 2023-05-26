library(stringr)




load_page_data = function(){
  "
  Description
  Function for downloading pomagam.pl page  
  Usage
  load_page_data()
  Arguments
  -
  Value
  HTML file of pomagam.pl
  "
  
  url = "https://pomagam.pl/"
  download.file (url = url, destfile = "page.html")
  f <- readLines("quote.html")
  
}

#Wyszukiwanie projektów
get_promoted_projects = function(f){
  "
  Description
  Function for getting titles of promoted projects from the pomagam.pl page
  Usage
  get_promoted_projects(f)
  Arguments
  f - html file of pomagam.pl
  Value
  Character vector of titles 
  "
  paragraph = str_match_all(f,'class=\"project-card-wrapper\"')
  imp = c()   
  paragraph_l = length(paragraph)
  for (x in 1:paragraph_l)  {
    if (!is.na(paragraph[[x]][,][1])) {
      imp = c(imp,x)
    }
  }
  titles = c()
  for (i in 1:length(imp)){
    title = str_match(f[imp[i]], '(title=\")(.*)(\")')
    t = str_split(title[3][1],'\"')
    t = t[[1]][1]
    if (!is.na(t)){
      titles = c(titles,t)
    }
    
  }
  titles
}

show_projects = function(titles){
  "
  Description
  Function for logging into the console list of titles 
  Usage
  show_projects(titles)
  Arguments
  titles - character vector of titles
  Value
  -
  "
  print("Promoted projects on pomagam.pl are:")
  print(titles)
}





