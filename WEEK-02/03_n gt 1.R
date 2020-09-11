library(stringr)
library(rvest)

grabFilmInfoFromFilmsPage = function(page,pagecount)
{
  # 50 elements
  for(i in 1:pagecount)
  {
    # title = id = rank = year = rating = minutes = genre = votes = metascore = desc = millions
  }
  
  result
  
}

grabNameFromFilmsPage = function(page)
{
  name = page %>%
    html_node(".header") %>%
    html_text()
  
    name = gsub("Most Rated Feature Films With","",name,fixed=T)
    name = str_trim(name)
    name
}

# grabFilmCountFromFilmsPage = function(page)
# {
#   count = page %>%
#     html_node(".desc") %>%
#     html_text()
#   
#     temp = strsplit(count,"of",fixed=T)
#     temp2 = strsplit(temp[[1]][2], "titles", fixed=T)
#     count = str_trim(temp2[[1]][1])
#     count = as.numeric(count)
# }

grabFilmCountFromFilmsPage = function(page)
{
  totalcount = page %>%
    html_node(".desc") %>%
    html_text()
  
    temp = strsplit(totalcount,"of",fixed=T)
    temp2 = strsplit(temp[[1]][2], "titles", fixed=T)
    totalcount = str_trim(temp2[[1]][1])
    totalcount = as.numeric(totalcount)
    
    temp2 = strsplit(temp[[1]][1],"to",fixed=T)
    pagecount = str_trim(temp2[[1]][2])
    pagecount = as.numeric(pagecount)
  
  result = list()
  result$totalcount = totalcount
  result$pagecount = pagecount
  
  result
  
}

nmid <- "nm0000226"

grabFilmsForPerson = function(nmid)
{
  url <- paste("https://www.imdb.com/filmosearch/?role=",nmid,"&explore=title_type&sort=num_votes,desc&mode=detail&page=1&title_type=movie&ref_=filmo_ref_typ", sep="")
  
  page1 <- read_html(url)
  
  result <- list()
    
  result$name = grabNameFromFilmsPage(page1)
  result$countfilms=grabFilmCountFromFilmsPage(page1)
  result$movies.50 = grabFilmInfoFromFilmsPage(page1,result$pagecount)
  
  ## Parallel format...
  ranks <- page1 %>%
    html_nodes(".lister-item-index") %>%
    html_text() %>%
    as.numeric()
  
  years <- page1 %>%
    html_nodes(".lister-item-year") %>%
    html_text()
  
  years <- gsub('(','',years,fixed=T)
  years <- gsub(')','',years,fixed=T)
  years <- gsub('I','',years,fixed=T)
  years <- as.numeric(years)
  
  titles = page1 %>%
    html_nodes(".lister-item-header a") %>%
    html_text()
  
  table = as.data.frame(cbind(ranks,years,titles))
  table$ranks = as.numeric(table$ranks)
  table$years = as.numeric(table$years)
  colnames(table) = c("rank","year","title")
  
  result
}

grabFilmsForPerson(nmid)
