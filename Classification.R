# Decision Tree Classification using the Rpart package
pkgs <- c("rpart", "rpart.plot", "party", "randomForest", "e1071")
library(pkgconfig)
library(rpart)

# CUSTOMIZE DATA FILE: Define data file
ds <- "tmdb_5000_movies.csv"
# movies <- read.csv(ds,sep=",")
movies <- read.csv(ds,sep=",", colClasses=c(NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, "NULL", "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL"))

movies <- na.omit(movies)
str(movies)
# Define columns 
# names(movies) <- c("budget", "genres", "homepage", "id", "keywords", "original_language", "original_title", "overview", "popularity", "production_companies", "production_countries", "release_date", "revenue", "runtime", "spoken_languages", "status", "tagline", "title", "vote_average", "vote_count")
# names(movies) <- c("budget", "revenue")
movies$profit <-movies$revenue-movies$budget
str(movies)
head(movies)

#0 = losing money 1 = profitable
movies$target <- ifelse(movies$profit > 0, 1, 0)
# CUSTOMIZE DATA FILE: The categorical dependent variable is called target
#movies$target_profit <- factor(df$target_profit, levels=c(0,1),
#                     labels=c("losing money", "profitable"))
#Convert movies to numeric
movies1<-sapply(movies,function(x) as.numeric(as.character(x)))
movies1=as.data.frame(movies1)
#Drop revenue, vote_average, and profit 
movies1 <- movies[-4]
#Create buckets for budget, language, status, popularity, vote average, and profit 
library(dplyr)
movies1 <- mutate(movies1, budget_group = ifelse(budget>=0 & budget<1000000, "Less than $1mil",
                                        ifelse(budget>=1000000 & budget < 10000000,"$1mil-$10 mil",
                                               ifelse(budget>=10000000 & budget<100000000,"$10mil-$100mil",
                                                      ifelse(budget>=100000000,"Greater than $100mil", "N/A")))))
movies1 <- mutate(movies1, popularity_group = ifelse(popularity>=0 & popularity<10, "Popularity less than 10",
                                                 ifelse(popularity>=10 & popularity < 20,"Popularity between 10 and 20",
                                                        ifelse(popularity>=20 & popularity<100,"Popularity between 20 and 100",
                                                               ifelse(popularity>=100,"Popularity greater than 100", "N/A")))))
movies1 <- mutate(movies1, vote_group = ifelse(vote_average>=0 & vote_average<4, "Vote average less than 4",
                                                     ifelse(vote_average>=4 & vote_average < 6,"Vote average between 4 and 6",
                                                            ifelse(vote_average>=6 & vote_average<8,"Vote average between 6 and 8",
                                                                   ifelse(vote_average>=8,"Vote average greater than 8", "N/A")))))

#Drop excess columns
movies1 <- movies1[-c(1:5)]
head(movies1)
#Drop popularity just to check
movies1 <- movies1[-3]
head(movies1)
# Define seed for random number generator
set.seed(1234)
# sample takes a random sample of the specified size from the elements of x
train <- sample(nrow(movies1), 0.7*nrow(movies1))
head(train)

# Define training data frame using random sample of observations
movies1.train <- movies1[train,]
# Define validation data frame using all observations not in training data frame
movies1.validate <- movies1[-train,]

# CUSTOMIZE DATA: Table counts the observations for each categorical value of target
# CUSTOMIZE DATA: The categorical dependent variable is called target
table(movies1.train$target)
table(movies1.validate$target)

# Decision Tree
library(rpart)
set.seed(1234)

dtree <- rpart(target ~ ., data=movies1.train, method="class",
               parms=list(split="information"))
summary(dtree)

# Display decision tree.  The true values follow the left branches.
plot(dtree);text(dtree)
dtree$cptable
plotcp(dtree)
opt <- which.min(dtree$cptable[,"xerror"])
cp <- dtree$cptable[opt, "CP"]
dtree.pruned <- prune(dtree, cp)
plot(dtree.pruned);text(dtree.pruned)
class(dtree$cptable)
names(dtree)
library(rpart.plot)
table(movies1.train$target)/nrow(movies1.train)
table(movies1.train$target)/nrow(movies1.train)
prp(dtree.pruned, type=2, extra=104, fallen.leaves=TRUE, main="Decision Tree")






