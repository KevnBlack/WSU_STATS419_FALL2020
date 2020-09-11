myMatrix = matrix(c(
  1, 0, 2,
  0, 3, 0,
  4, 0, 5
), nrow=3, byrow=T);
myMatrix

transposeMatrix = function(mat)
{
  t(mat);	
}


rotateMatrix90 = function(mat)
{
  t(apply(mat, 2, rev)); # Rotation by reversing the cols, then transpose
}

rotateMatrix180 = function(mat)
{
  rotateMatrix90(rotateMatrix90(mat)); # Repeat two 90 deg rotations
}

rotateMatrix270 = function(mat)
{
  rotateMatrix180(rotateMatrix90(mat)); # Rotate a 90 deg rotation with a 180 deg rotation
}

myMatrix
rotateMatrix90(myMatrix)
rotateMatrix180(myMatrix)
rotateMatrix270(myMatrix)

##############################################

pairs(x = iris[,1:4], # Supply just the sepal and petal lengths/widths
      pch = 21, # Plotting symbol 21, a filled circle
      bg = c("red", "green", "blue")[iris$Species], # Color points by species
      main = "Iris Data (red=setosa,green=versicolor,blue=virginica)") # Title

##############################################

df <- read.table("personality-raw.txt", header=T, sep="|", dec=".")[,-3] # Import data, omit V00
YEAR <- c() # Create empty vectors for YEAR and WEEK
WEEK <- c()

for (i in 1:length(df$date_test)){ # Loop through date_test col of df
  curr <- unlist(strsplit(as.character(df$date_test[i]), " "))[1] # Grab date as a character
  curr.form <- as.Date(curr, "%m/%d/%Y") # Convert character to formatted date
  YEAR[i] <- format(curr.form,"%Y") # Add year to YEAR vector
  WEEK[i] <- format(curr.form,"%V") # Add week number to WEEK vector
}

df$YEAR <- YEAR # Add YEAR and WEEK cols to original df
df$WEEK <- WEEK
df <- df[order(df$YEAR,df$WEEK, decreasing=TRUE),] # Sort dataframe by YEAR and WEEK

df <- df[!duplicated(df$md5_email),] # Keep unique rows based on df$md5_email
write.table(df, file = "personality-clean.txt", sep="|") # Write cleaned df to .txt file

##############################################

df <- read.table("personality-clean.txt", header=T, sep="|", dec=".") # Read clean dataset
monte.record <- df[1,] # Record for monte.shaffer@gmail.com
doSummary(monte.record) # Various statistics for the monte record

doSummary = function(x)
{
  z.scores <- c()
  naive = doSampleVariance(as.numeric(x[,3:62]),'naive') # Calculate naive var
  two.pass = doSampleVariance(as.numeric(x[,3:62]),'two-pass') # Calculate two-pass var
  mean = mean(as.numeric(x[,3:62])) # Calculate mean
  stdev = sd(as.numeric(x[,3:62])) # Calculate stdev
  x = as.numeric(x[,3:62]) # Convert vector to numerical values
  
  print(paste("Length:",length(x))) # Length of x
  print(paste("# of NAs:",sum(is.na(x)))) # Number of NAs
  print(paste("Mean:",mean)) # Mean of x
  print(paste("Median:",median(x))) # Median of x
  print(paste("Mode:",doMode(x))) # Mode of x
  cat("",sep="\n")
  print(paste("Variance (Base):",var(x))) # Base R var
  cat("",sep="\n")
  print(paste("Variance (Naive, Sum):",naive[1])) # Sum when calculating naive var
  print(paste("Variance (Naive, SumSq):",naive[2])) # SumSq when calculating naive var
  print(paste("Variance (Naive, Var):",naive[3])) # Naive var
  cat("",sep="\n")
  print(paste("Variance (Two-pass, sum1):",two.pass[1])) # sum1 when calculating two-pass var
  print(paste("Variance (Two-pass, sum2):",two.pass[2])) # sum2 when calculating two-pass var
  print(paste("Variance (Two-pass, Var):",two.pass[3])) # Two-pass var
  cat("",sep="\n")
  print(paste("Standard Deviation (Base):",stdev)) # Base R stdev
  print(paste("Standard Deviation (Naive):",sqrt(as.numeric(naive[3])))) # Stdev using naive var
  print(paste("Standard Deviation (Two-pass):",sqrt(as.numeric(two.pass[3])))) # Stdev using two-pass var
  
  for (i in 1:length(x)) # Compute z-scores for specified record
  {
    z = (x[i]-mean)/stdev # z-score formula
    z.scores <- append(z.scores,z) # Collection of z-scores
  }
  
  plot(x,z.scores) # Plot raw scores against z-scores
  unique(x)
}

doSampleVariance = function(x,method) # Can perform two variance calculations
{
  if(method=='naive') # Algorithm from https://bit.ly/31QSe2I
  {
    n = Sum = SumSq = 0
    for (i in 1:length(x))
    {
      n = n+1
      Sum = Sum + x[i]
      SumSq = SumSq+(x[i]*x[i])
    }
    Var = (SumSq - (Sum*Sum)/n)/(n-1)
    stats <- list(Sum,SumSq,Var)
    return(stats)
  }
  else # Algorithm from https://bit.ly/32R77kS
  {
    n = sum1 = sum2 = 0
    for (i in 1:length(x))
    {
      n = n+1
      sum1 = sum1 + x[i]
    }
    mean = sum1 / n
    for (i in 1:length(x))
    {
      sum2 = sum2 + (x[i]-mean)^2
    }
    Var = sum2 / (n-1)
    stats <- list(sum1,sum2,Var)
    return(stats)
  }
}

doMode = function(x) # Perform mode calculation
{
  uniq <- unique(x) # Isolate unique values
  matches <- match(x,uniq) # Indexes for every x value in uniq
  freq <- tabulate(matches) # Frequencies for each x value
  return(uniq[which.max(freq)]) # Index for max frequency in uniq
}

##############################################

## functions

education = function(one)
{
  result = list();
  result$who 		= one;
  result$think 	= c("intensitively", "critically");
  result$goal 	= "intelligences + character";
  result;	
}

me = education("monte");

library(stringr);	
library(rvest);	

# Research question:  who is a better actor?  Will Smith?  Denzel Washington?

## actor ... person_id
##			movie_id ... details
##			name ... count movies

# https://rvest.tidyverse.org/index.html

## Denzel Washington [nm0000243] vs Will Smith [nm0000226]

## https://www.imdb.com/filmosearch/?explore=title_type&role=nm0000243&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=1&title_type=movie


grabFilmInfoFromFilmsPage = function(page)
{
  # 50 elements
  # title = id = rank = year = rating = minutes = genre = votes = metascore = desc = millions
  
  movies = page %>%
    html_nodes(".mode-detail");
  
  pagecount = length(movies);
  
  result = data.frame( 			matrix(ncol = 11,nrow = pagecount) );
  # a matrix-type form with lots of NA values ...
  
  colnames(result) = c("rank", "title", "ttid", "year", "rated", "minutes", "genre", "ratings", "metacritic", "votes", "millions"); 
  
  
  for(i in 1:pagecount)
  {
    movie = movies[i];
    
    rank = movie %>%
      html_node(".lister-item-index") %>%
      html_text() %>%
      as.numeric();
    result$rank[i] = rank;
    
    title = movie %>%
      html_node(".lister-item-header a") %>%
      html_text();
    result$title[i] = title;
    
    ttid = movie %>%
      html_node(".lister-item-header a") %>%
      html_attr("href");
    
    temp = strsplit(ttid,"/",fixed=T);
    ttid = temp[[1]][3];
    result$ttid[i] = ttid;
    
    year = movie %>%
      html_node(".lister-item-year") %>%
      html_text();
    year = cleanupYear(year);
    result$year[i] = year;
    
    rated = movie %>%
      html_node(".certificate") %>%
      html_text();
    result$rated[i] = rated;
    
    minutes = movie %>%
      html_node(".runtime") %>%
      html_text();
    minutes = cleanupMinutes(minutes);
    result$minutes[i] = minutes;		
    
    genre = movie %>%
      html_node(".genre") %>%
      html_text();
    genre = str_trim(genre);
    result$genre[i] = genre;
    
    ratings = movie %>%
      html_node("div .rating-list") %>%
      html_attr("title");
    temp = strsplit(ratings,"/",fixed=T);
    temp = gsub("Users rated this","",temp[[1]][1],fixed=T);	
    temp = str_trim(temp);
    ratings = as.numeric(temp);
    result$ratings[i] = ratings;
    
    metacritic = movie %>%
      html_node(".ratings-metascore span") %>%
      html_text();
    metacritic = as.numeric(str_trim(metacritic));
    result$metacritic[i] = metacritic;
    
    # para ... +5 EASTER EGG ...
    
    info = movie %>%
      html_nodes(".lister-item-content p span") %>%
      html_text();
    
    votes = as.numeric(gsub(",","",info[8],fixed=T));
    result$votes[i] = votes;
    
    millions = cleanupMillions(info[11]);
    result$millions[i] = millions;			
  }
  
  #str(result);
  
  result;
}

cleanupMillions = function(millions)
{
  millions = gsub('$','',millions, fixed=T);
  millions = gsub('M','',millions, fixed=T);
  
  millions = as.numeric(millions);
  millions;
}

cleanupMinutes = function(minutes)
{
  minutes = gsub('min','',minutes, fixed=T);
  
  minutes = as.numeric(minutes);
  minutes;
}

cleanupYear = function(year)
{
  year = gsub('(','',year, fixed=T);
  year = gsub(')','',year, fixed=T);
  year = gsub('I','',year, fixed=T);
  year = as.numeric(year);
  year;
}

grabNameFromFilmsPage = function(page)
{
  name = page %>%
    html_node(".header") %>%
    html_text();
  
  name = gsub("Most Rated Feature Films With","",name,fixed=T);
  name = str_trim(name);
  
  name;
}

grabFilmCountFromFilmsPage = function(page)
{
  totalcount = page %>%
    html_nodes(".desc") %>%
    html_text();
  
  temp = strsplit(totalcount,"of",fixed=T);
  temp2 = strsplit(temp[[1]][2],"titles", fixed=T);
  
  totalcount = str_trim(temp2[[1]][1]);
  totalcount = as.numeric(totalcount);
  
  temp2 = strsplit(temp[[1]][1],"to", fixed=T);
  
  pagecount = str_trim(temp2[[1]][2]);
  pagecount = as.numeric(pagecount);
  
  result = list();
  
  result$totalcount = totalcount;
  result$pagecount = pagecount;
  
  result;
}

#   nmid = "nm0000226";
# 	will = grabFilmsForPerson(nmid);
# 	plot(will$movies.50[,c(1,6,7:10)]);
#  	boxplot(will$movies.50$millions);

#   nmid = "nm0000243";
# 	denzel = grabFilmsForPerson(nmid);
# 	plot(denzel$movies.50[,c(1,6,7:10)]);
#  	boxplot(denzel$movies.50$millions);


# https://www.imdb.com/title/tt0466839/?ref_=filmo_li_tt ... get box office budget/gross if NA ... on millions. ..

grabFilmsForPerson = function(nmid)
{
  url = paste("https://www.imdb.com/filmosearch/?explore=title_type&role=",nmid,"&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=1&title_type=movie", sep="");
  
  page1 = read_html(url);
  result = list();
  ## useful for other data purposes
  result$nmid = nmid;
  
  ## name of person
  result$name = grabNameFromFilmsPage(page1);
  result$countfilms = grabFilmCountFromFilmsPage(page1);
  
  result$movies.50 = grabFilmInfoFromFilmsPage(page1);
  
  ##  parallel format ...
  # ranks = page1 %>%
  # html_nodes(".lister-item-index") %>%
  # html_text() %>%
  # as.numeric();	
  
  # ranks;
  
  # years = page1 %>%
  # html_nodes(".lister-item-year") %>%
  # html_text();
  
  # years = gsub('(','',years, fixed=T);
  # years = gsub(')','',years, fixed=T);
  # years = gsub('I','',years, fixed=T);
  # years = as.numeric(years);
  
  # titles = page1 %>%	
  # html_nodes(".lister-item-header a") %>%
  # html_text();
  
  # titles;
  
  result;
}

nmid = "nm0000226";
will = grabFilmsForPerson(nmid);
plot(will$movies.50[,c(1,6,7:10)]);
boxplot(will$movies.50$millions);
widx =  which.max(will$movies.50$millions);
will$movies.50[widx,];
summary(will$movies.50$year); # bad boys for life ... did data change?

nmid = "nm0000243";
denzel = grabFilmsForPerson(nmid);
plot(denzel$movies.50[,c(1,6,7:10)]);
boxplot(denzel$movies.50$millions);
didx =  which.max(denzel$movies.50$millions);
denzel$movies.50[didx,];
summary(denzel$movies.50$year);

par(mfrow=c(1,2));
boxplot(will$movies.50$millions, main=will$name, ylim=c(0,360), ylab="Raw Millions" );
boxplot(denzel$movies.50$millions, main=denzel$name, ylim=c(0,360), ylab="Raw Millions" );
par(mfrow=c(1,1));

# data from 1920 to 2020 ... 101 years ...

# bigger dollar gives a more accurate percent ...

# read the values in "year"/"dollars" using rvest ...
library(rvest);	

inflation = function(infl)
{
  infl_vals <- c() # Inflated values
  
  infl.html = read_html(infl);
  
  infl.table = infl.html %>%
    html_node(".expand-table-parent") %>%
    html_node(".table-striped") %>%
    html_node("tbody") %>%
    html_nodes("tr");
  
  result = data.frame( matrix(nrow=length(infl.table), ncol=3));
  colnames(result) = c("year","dollar","inflation");
  
  for(i in 1:length(infl.table) )
  {
    infl.row = infl.table[i]	%>% 
      html_nodes("td") %>%
      html_text();
    
    year = as.numeric(infl.row[1]);
    temp = gsub('$','',infl.row[2],fixed=T);
    temp = gsub(',','',temp,fixed=T);
    dollar = as.numeric(temp);
    temp = gsub('%','',infl.row[3],fixed=T);
    inflation = as.numeric(temp);	
    
    result$year[i] = year;
    result$dollar[i] = dollar;
    result$inflation[i] = inflation;
  }
  infl_vals <- append(infl_vals,result[length(result[,2]),2]);
  return(infl_vals)
}


## Will Smith movie millions adjusted for inflation
Will.Year.Mil <- will$movies.50[,c(4,11)] # Extract just year and millions
Will.Year.Mil <- Will.Year.Mil[order(Will.Year.Mil$year),] # Sort by year
Will.Year.Mil$millions <- Will.Year.Mil$millions*1000000 # Actual money amount

for(j in 1:dim(Will.Year.Mil)[1])
{
  if (is.na(Will.Year.Mil[j,2]))
  {
    Will.Year.Mil$millions.2000[j] <- NA
    next
  }
  if (Will.Year.Mil[j,1] == 2019) # Hard coded values since 2019-2020 don't work with website 
  {
    Will.Year.Mil$millions.2000[46] <- 360349105.97 
    Will.Year.Mil$millions.2000[47] <- 20826791.90
    Will.Year.Mil$millions.2000[48] <- NA
    Will.Year.Mil$millions.2000[49] <- 790505.97
    Will.Year.Mil$millions.2000[50] <- 204420000.00
    break
  }
  infl <- paste("https://www.officialdata.org/us/inflation/",Will.Year.Mil[j,1],"?endYear=2020&amount=",Will.Year.Mil[j,2],sep="")
  result <- inflation(infl) # Millions value with 2020 inflation
  Will.Year.Mil$millions.2000[j] <- result[1]; # Add to millions.2000 column
}

## Denzel Washington movie millions adjusted for inflation
Denzel.Year.Mil <- denzel$movies.50[,c(4,11)]
Denzel.Year.Mil <- Denzel.Year.Mil[order(Denzel.Year.Mil$year),] # Sort by year
Denzel.Year.Mil$millions <- Denzel.Year.Mil$millions*1000000 # Actual money amount

for(j in 1:dim(Denzel.Year.Mil)[1])
{
  if (is.na(Denzel.Year.Mil[j,2]))
  {
    Denzel.Year.Mil$millions.2000[j] <- NA
    next
  }
  if (Denzel.Year.Mil[j,1] == 2020)
  {
    break
  }
  infl = paste("https://www.officialdata.org/us/inflation/",Denzel.Year.Mil[j,1],"?endYear=2020&amount=",Denzel.Year.Mil[j,2],sep="")
  result <- inflation(infl) # Millions value with 2020 inflation
  Denzel.Year.Mil$millions.2000[j] <- result[1]; # Add to millions.2000 column
}

##############################################

# Comparing millions
par(mfrow=c(1,2));
boxplot(will$movies.50$millions, main=will$name, ylim=c(0,360), ylab="Raw Millions" );
boxplot(denzel$movies.50$millions, main=denzel$name, ylim=c(0,360), ylab="Raw Millions" );
par(mfrow=c(1,1));

# Comparing millions.2000
par(mfrow=c(1,2));
boxplot(Will.Year.Mil$millions.2000, main=will$name, ylim=c(0,5e+08), ylab="Adjusted Millions" );
boxplot(Denzel.Year.Mil$millions.2000, main=denzel$name, ylim=c(0,5e+08), ylab="Adjusted Millions" );
par(mfrow=c(1,1));

# Comparing ratings
par(mfrow=c(1,2));
boxplot(will$movies.50$ratings, main=will$name, ylim=c(4.5,8.5), ylab="Ratings" );
boxplot(denzel$movies.50$ratings, main=denzel$name, ylim=c(4.5,8.5), ylab="Ratings" );
par(mfrow=c(1,1));

# Comparing metacritic
par(mfrow=c(1,2));
boxplot(will$movies.50$metacritic, main=will$name, ylim=c(20,80), ylab="Metacritic" );
boxplot(denzel$movies.50$metacritic, main=denzel$name, ylim=c(20,80), ylab="Metacritic" );
par(mfrow=c(1,1));

# Comparing votes
par(mfrow=c(1,2));
boxplot(will$movies.50$votes, main=will$name, ylim=c(0,5.5e+05), ylab="Votes" );
boxplot(denzel$movies.50$votes, main=denzel$name, ylim=c(0,5.5e+05), ylab="Votes" );
par(mfrow=c(1,1));

# setwd("mypath"); # set where you want to store the file ...
#write.table(Denzel.Year.Mil,file="inflation.txt",sep="|",row.names=F);

# this data starts with 1920 at $1,000,000 dollars
# how can you restructure it so it is in $1,000,000 dollars in the year 2000... I would suggest using the dollars to build the inflation rate, as it will be more accurate ... 
# e.g., around 1973 ... 
# 2220000/2090000 - 1;  #  0.06220096  # vs ... 6.22%
#  options(digits=22);  # the computer is storing even more accuracy under the hood ...


# then you can use it to answer your questions for will vs denzel.
