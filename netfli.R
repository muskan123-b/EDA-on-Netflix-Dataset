#Setting the working directory
setwd("C:/Users/muska/Desktop/Netflix")

#Importing the dataset
df1 <- read.csv("C:/Users/muska/Desktop/Netflix/netflix_titles.csv")
View(df1)

#Importing Libraries
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(caTools)
library(DataExplorer)
library(DescTools)
library(data.table)
library(reshape)
library(stringr)
library(EnvStats)
library(forcats)
library(viridis)
#Data Exploration
nrow(df1) #No. of Rows
ncol(df1) #No. of Columns

head(df1)

str(df1)

summary(df1)

colSums(is.na(df1))

is.null(df1)

df1[df1 == ""] <- NA

plot_missing(df1,group = list("Very less missing values" = 0.06,"A lot of missing values" = 1))

which(is.na(df1$rating))
df1$rating[5990]

#There are missing values in column director,cast,country and date_added.
#We can't randomly fill the missing values in columns of director and cast, so we can drop them.
#For minimal number of missing values in country and date_added,rating, we can fill them using mode(most common value) and mean.

require(dplyr)
df1<-dplyr::mutate(df1, director=NULL, cast = NULL)

df1$country = ifelse(is.na(df1$country),Mode(df1$country,na.rm = T),df1$country)

df1$date_added = ifelse(is.na(df1$date_added),Mode(df1$date_added,na.rm = T),df1$date_added)

df1$rating = ifelse(is.na(df1$rating),Mode(df1$rating,na.rm = T),df1$rating)

df1<-df1[!is.na(df1$duration), ]


#All the missing values in the dataset have either been removed or filled. There are no missing values left.

#-> Cleaning the data
#Adding some new columns:
  
 # listed_in - Genre
#Year Added - year_add
#Month Added - month_add
#Principal Country - country_main


names(df1)[names(df1) == "listed_in"] <- "genre"
library(dplyr)
library(tidyr)


df1$genre<-sapply(strsplit(df1$genre, ","), "[", 1)
df1$country<-sapply(strsplit(df1$country, ","), "[", 1)

df1$date_added <- trimws(df1$date_added, which = c("left"))
df1$date_added<-as.Date(df1$date_added, format='%B %d, %y')
df1$Month_added<-lubridate::month(df1$date_added)

movie_df = df1[df1['type'] == 'Movie']
tv_df = df1[df1['type'] == 'TV Show']

df1.type<-df1%>%group_by(type)%>%count(type)
setnames(df1.type, old="n",new="count")
ggplot(df1.type) +geom_bar(aes(x = type, y = count, fill=factor(count)), position = position_dodge(),stat="identity",color="black")
#There are more no.s of movies than tv shows

df1.rating<-df1%>%group_by(rating)%>%count(rating)
setnames(df1.rating, old="n",new="count")
ggplot(df1.rating) +geom_bar(aes(x = reorder(rating,-count), y = count, fill=factor(count)), position = position_dodge(),stat="identity",color="black")                           
#The largest count of movies are made with the 'TV-MA' rating."TV-MA" is a rating assigned by the TV Parental Guidelines to a television program that was designed for mature audiences only.
#Second largest is the 'TV-14' stands for content that may be inappropriate for children younger than 14 years of age.
#Third largest is the very popular 'R' rating.An R-rated film is a film that has been assessed as having material which may be unsuitable for children under the age of 17.

tv_df<-df1%>%filter(type=="TV Show")%>%group_by(rating)%>%count(rating)
setnames(tv_df, old="n",new="count")
ggplot(tv_df) +geom_bar(aes(x = reorder(rating,-count), y = count, fill=factor(count)), position = position_dodge(),stat="identity",color="black")   
#The largest count of Tv shows are made with the 'TV-MA' rating, followed by 'TV-14' and 'TV-PG'.

release.year<-df1%>%group_by(release_year,type)%>%count(type)
setnames(release.year, old="n",new="Count")
cast_data <- cast(release.year, release_year~type, mean) 
cast_data$Movie = ifelse(is.na(cast_data$Movie),Mode(cast_data$Movie,na.rm = T),cast_data$Movie)
cast_data$`TV Show` = ifelse(is.na(cast_data$`TV Show`),Mode(cast_data$`TV Show`,na.rm = T),cast_data$`TV Show`)
plot(cast_data$release_year, cast_data$Movie, type="o", col="blue", pch="o", xlab="Year",ylab="Count of Movies/TV shows", lty=1)
points(cast_data$release_year, cast_data$`TV Show`, col="red", pch="*")
lines(cast_data$release_year, cast_data$`TV Show`, col="red",lty=2)
#We can see that Netflix released most number of content in year 2017.
#Noticeable growth in releasing content can be seen from the year 2015.

df1.duration.movie<-df1%>%filter(type=="Movie")
df1.duration.movie$duration<-str_replace(df1.duration.movie$duration,"min","")
df1.duration.movie$duration<-as.numeric(df1.duration.movie$duration)
epdfPlot(df1.duration.movie$duration, epdf.col = "red",main="",xlab = "Time Duration(in min)",ylab = "Density")
#So, a good amount of movies on Netflix are among the duration of 75-120 mins.

df1.duration.tvshow<-df1%>%filter(type=="TV Show")
df1.duration.tvshow$duration<-str_replace(df1.duration.tvshow$duration,"Season","")
df1.duration.tvshow$duration<-str_replace(df1.duration.tvshow$duration,"s","")
df1.duration.tvshow$duration<-as.numeric(df1.duration.tvshow$duration)

TVShows.Seasons<-df1.duration.tvshow[c("title","duration")]
TVShows.Seasons<-TVShows.Seasons %>% arrange(desc(duration))
Top20Shows<-head(TVShows.Seasons,20)
ggplot(Top20Shows) +geom_bar(aes(x =reorder(title,-duration), y = duration,fill=factor(duration)), position = position_dodge(),stat="identity",color="black")+ theme(axis.text.x = element_text(angle = 90))
#Grey's Anatomy has the highest numbers of seasons

tv.duration<-df1.duration.tvshow[c("title","duration")]
tv.duration<-tv.duration %>% group_by(duration)%>%count(duration)
setnames(tv.duration, old="n",new="count")
tv.duration<-tv.duration %>% arrange(desc(count))
Top5duration<-head(tv.duration,5)
pie_percent<- paste("Season",Top5duration$duration,":",round(100*Top5duration$count/sum(Top5duration$count), 2), "%")
pie(Top5duration$count, labels = pie_percent,col = rainbow(length(Top5duration$count)),main="Seasons available on netflix of tv shows")
#69.58% of the shows on netflix has only 1 season.


Movie.Countries<-df1.duration.movie[c("title","country")]
Movie.Countries<-Movie.Countries %>% group_by(country)%>%count(country)
setnames(Movie.Countries, old="n",new="count")
Movie.Countries<-Movie.Countries %>% arrange(desc(count))
Top20countries<-head(Movie.Countries,20)
ggplot(Top20countries) +geom_bar(aes(x = reorder(country,-count), y = count,fill=factor(count)), position = position_dodge(),stat="identity",color="black")+ theme(axis.text.x = element_text(angle = 90))+xlab("Countries")
#Highest no. of movies are produced in United States.

tvshow.Countries<-df1.duration.tvshow[c("title","country")]
tvshow.Countries<-tvshow.Countries %>% group_by(country)%>%count(country)
setnames(tvshow.Countries, old="n",new="count")
tvshow.Countries<-tvshow.Countries %>% arrange(desc(count))
Top20countriesTV<-head(tvshow.Countries,20)
ggplot(Top20countriesTV) +geom_bar(aes(x = reorder(country,-count), y = count,fill=factor(count)), position = position_dodge(),stat="identity",color="black")+ theme(axis.text.x = element_text(angle = 90))+xlab("Countries")
#Highest no. of TV shows are produced in United States as well.

country.genre<-df1[c("country","genre")]
country.genre<-country.genre %>% group_by(genre)%>%count(genre)
setnames(country.genre, old="n",new="count")
country.genre<-country.genre %>% arrange(desc(count))
Top10genre<-head(country.genre,10)
ggplot(Top10genre) +geom_bar(aes(x = reorder(genre,-count), y = count,fill=factor(count)), position = position_dodge(),stat="identity",color="black")+ theme(axis.text.x = element_text(angle = 90))+xlab("Genres")
#There are about 1600 Dramas followed by 1210 comedies.

year.content<-df1[c("title","release_year","Month_added")]
year.content<-year.content %>% group_by(release_year,Month_added)%>%count(Month_added)
setnames(year.content, old="n",new="count")
TopContent<-tail(year.content,260)
ggplot(TopContent, aes(x = release_year, y = Month_added, fill = count))+geom_tile() + scale_fill_viridis(discrete = FALSE)
#Heat Map showing the release of content over the years.

