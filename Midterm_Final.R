#Data Load
imdb_data = read.csv("/Users/manocha/Documents/R_Labs/Midterm/data_final.csv")
attach(imdb_data)

#Loading libraries
library(car)
require(lmtest)
require(plm)
library(boot)
library(splines)
require(psych)
library(stargazer)

#Data Cleanup
myvars = imdb_data[, c(
  3,
  7,
  9,
  10,
  11,
  14,
  16,
  34,
  35,
  38,
  40,
  41,
  42,
  43,
  44,
  45,
  46,
  47,
  48,
  49,
  50,
  51,
  52,
  53,
  54,
  55,
  56,
  57,
  58
)]
myvars_correct <- myvars[myvars$language == 'English', ]
myvars_correct <-
  myvars_correct[rowSums(is.na(myvars_correct)) == 0, ]
myvars_correct <-
  myvars_correct[myvars_correct$cast_total_facebook_likes <= 1e+05, ]

attach(myvars_correct)
View(myvars_correct)
myvars_correct$action = as.factor(myvars_correct$action)
myvars_correct$adventure = as.factor(myvars_correct$adventure)
myvars_correct$scifi = as.factor(myvars_correct$scifi)
myvars_correct$thriller = as.factor(myvars_correct$thriller)
myvars_correct$musical = as.factor(myvars_correct$musical)
myvars_correct$romance = as.factor(myvars_correct$romance)
myvars_correct$western = as.factor(myvars_correct$western)
myvars_correct$sport = as.factor(myvars_correct$sport)
myvars_correct$horror = as.factor(myvars_correct$horror)
myvars_correct$drama = as.factor(myvars_correct$drama)
myvars_correct$war = as.factor(myvars_correct$war)
myvars_correct$animation = as.factor(myvars_correct$animation)
myvars_correct$crime = as.factor(myvars_correct$crime)
myvars_correct$Biography = as.factor(myvars_correct$Biography)
myvars_correct$Comedy = as.factor(myvars_correct$Comedy)
myvars_correct$Family = as.factor(myvars_correct$Family)
myvars_correct$Mystery = as.factor(myvars_correct$Mystery)
myvars_correct$Fantasy = as.factor(myvars_correct$Fantasy)
myvars_correct$History = as.factor(myvars_correct$History)
attach(myvars_correct)

# Visualization -----------------------------------------------------------
par(mfrow = c(3, 2))

#dependent variable distribution
hist(
  myvars_correct$imdb_score,
  col = "green",
  main = "Histogram for imdb_score",
  xlab = "IMDB Score"
)

#duration_mins
boxplot(duration_mins)
plot(
  myvars_correct$duration_mins,
  myvars_correct$imdb_score,
  col = "gray",
  xlab = "Duration in Mins",
  ylab = "IMDB Score"
)
reg1 = lm(imdb_score ~ poly(duration_mins, degree = 5), data = myvars_correct)
lines(sort(myvars_correct$duration_mins), predict(reg1)[order(myvars_correct$duration_mins)],
      col = "red")
summary(reg1)

#residual plot
plot(predict(reg1), residuals(reg1), col = "red")
abline(0, 0, lty = 2)

#heteroskedasticity test
ncvTest(reg1) # p< .05 hence model contains heteroskedasticity

#Testing after removing heteroskedasticity
coeftest(reg1, vcov = vcovHC(reg1, type = "HC1")) #still significant

# test for outlier
outlierTest(reg1)
myvars_correct <- myvars_correct[-c(2479), ] #remove outliers
reg1 = lm(imdb_score ~ poly(duration_mins, degree = 1), data = myvars_correct) # run reggression
summary(reg1)# check for r2 -> still significant
lines(sort(myvars_correct$duration_mins), predict(reg1)[order(myvars_correct$duration_mins)],
      col = "red")
#Observations -> relationship exists, should be kept

#budget
boxplot(movie_budget)
plot(
  myvars_correct$movie_budget,
  myvars_correct$imdb_score,
  col = "gray",
  xlab = "Budget",
  ylab = "IMDB Score"
)
reg2 = lm(imdb_score ~ poly(movie_budget, degree = 3), data = myvars_correct)
lines(sort(myvars_correct$movie_budget), predict(reg2)[order(myvars_correct$movie_budget)],
      col = "red")
summary(reg2)
outlierTest(reg2) # test for outlier
lines(sort(myvars_correct$movie_budget), predict(reg2)[order(myvars_correct$movie_budget)],
      col = "red")

#Observations -> no apparent relationship even after removing outliers

#news_article
boxplot(number_news_articles)
plot(
  myvars_correct$number_news_articles,
  myvars_correct$imdb_score,
  col = "gray",
  xlab = "Number of News Articles",
  ylab = "IMDB Score"
)
reg3 = lm(imdb_score ~ poly(number_news_articles, 8), data = myvars_correct)
lines(sort(myvars_correct$number_news_articles),
      predict(reg3)[order(myvars_correct$number_news_articles)],
      col = "red")
summary(reg3)
plot(predict(reg3), residuals(reg3), col = "red")
abline(0, 0, lty = 2)
ncvTest(reg3) # p< .05 hence model contains heteroskedasticity
coeftest(reg3, vcov = vcovHC(reg3, type = "HC1")) #still significant
outlierTest(reg3) # test for outlier
myvars_correct <- myvars_correct[-c(2479, 1516), ] #remove outliers
attach(myvars_correct)
reg3 = lm(imdb_score ~ poly(number_news_articles, degree = 1), data = myvars_correct)
summary(reg3)
lines(sort(myvars_correct$number_news_articles),
      predict(reg3)[order(myvars_correct$number_news_articles)],
      col = "red")

#Observations ->  positive relationship, can be kept

#director facebook likes
boxplot(director_facebook_likes)
plot(
  myvars_correct$director_facebook_likes,
  myvars_correct$imdb_score,
  col = "gray",
  xlab = "Director Facebook Likes",
  ylab = "IMDB Score"
)
quantile(myvars_correct$director_facebook_likes, c(.25, .90))
reg4 = lm(imdb_score ~ bs(
  director_facebook_likes,
  knots = c(5000, 9000),
  degree = 1
),
data = myvars_correct)
lines(sort(myvars_correct$director_facebook_likes),
      predict(reg4)[order(myvars_correct$director_facebook_likes)],
      col = "red")
summary(reg4)
outlierTest(reg4) # test for outlier
#Observations -> slight positive


#cast_total_facebook_likes
data_likes = myvars_correct[myvars_correct$cast_total_facebook_likes <= 1e+05, ]
boxplot(data_likes$cast_total_facebook_likes)
plot(
  data_likes$cast_total_facebook_likes,
  data_likes$imdb_score,
  col = "gray",
  xlab = "Total Cast Facebook Likes",
  ylab = "IMDB Score"
)
quantile(data_likes$cast_total_facebook_likes, c(.5, .75))
reg5 = lm(imdb_score ~ bs(cast_total_facebook_likes, degree = 4),
          data = data_likes)
lines(sort(data_likes$cast_total_facebook_likes),
      predict(reg5)[order(data_likes$cast_total_facebook_likes)],
      col = "red")
summary(reg5)
#Observations -> relationship exists but r2 is low

##Interactions
reg5 = lm(
  imdb_score ~ director_facebook_likes + cast_total_facebook_likes + cast_total_facebook_likes *
    director_facebook_likes,
  data = data_likes
)

summary(reg5)

#check for collinearity
par(mfrow = c(1, 1))
names(myvars_correct)
quantvars = myvars_correct[, c(2, 5, 6, 7, 8, 9)]
pairs.panels(quantvars)

# Even splines to have a better fit
quantile(myvars_correct$movie_budget, c(.25, .75))
quantile(myvars_correct$director_facebook_likes, c(.25, .75))
quantile(myvars_correct$cast_total_facebook_likes, c(.25, .75))
quantile(myvars_correct$number_news_articles, c(.25, .75))
quantile(myvars_correct$duration_mins, c(.25, .75))


#### K-Fold with splines #####

timestamp(stamp = date())
dega = NULL
degb = NULL
degc = NULL
degd = NULL
dege = NULL
MSE = NULL
d = NULL
for (a in 1:6)
{
  for (b in 1:6)
  {
    for (c in 1:6)
    {
      for (d in 1:6)
      {
        for (e in 1:6)
        {
          dega = append(dega, a)
          degb = append(degb, b)
          degc = append(degc, c)
          degd = append(degd, d)
          dege = append(dege, e)
          glm.fit = glm(
            imdb_score ~
              +bs(
                duration_mins,
                knots = c(96, 121),
                degree = a
              )
            + bs(
              movie_budget,
              knots = c(9.7e+06, 5.0e+07),
              degree = b
            )
            + bs(
              director_facebook_likes,
              knots = c(10, 258),
              degree = c
            )
            + bs(
              cast_total_facebook_likes,
              knots = c(1911, 16701),
              degree = d
            )
            + bs(
              number_news_articles,
              knots = c(81, 1003),
              degree = e
            )
            + action
            + thriller
            + musical
            + romance
            + horror
            + drama
            + animation
            + crime
            + Biography
            + Comedy
            + Family
            ,
            data = myvars_correct
          )
          MSE = append(MSE, cv.glm(myvars_correct, glm.fit, K = 10)$delta[1])
        }
      }
    }
  }
}

d = data.frame(dega, degb, degc, degd, dege, MSE)
d[which.min(d$MSE),]
timestamp(stamp = date())


#Run the model with above polynomials
fit = glm(
  imdb_score ~
    +bs(duration_mins, knots = c(96, 121), degree = 4)
  + bs(
    movie_budget,
    knots = c(9.7e+06, 5.0e+07),
    degree = 2
  )
  + bs(
    director_facebook_likes,
    knots = c(10, 258),
    degree = 3
  )
  + bs(
    cast_total_facebook_likes,
    knots = c(1911, 16701),
    degree = 1
  )
  + bs(
    number_news_articles,
    knots = c(81, 1003),
    degree = 1
  )
  + action
  + thriller
  + musical
  + romance
  + horror
  + drama
  + animation
  + crime
  + Biography
  + Comedy
  + Family
  #+country
  ,
  data = myvars_correct
)
summary(fit)

stargazer(
  fit,
  title = "Final Model",
  align = TRUE,
  type = 'html',
  out = '/Users/manocha/Documents/R_Labs/Midterm/final.htm'
)

#Test the final model for outliers
outlierTest(fit)

#Remove outliers

myvars_correct_o <- myvars_correct[-c(2297, 1466, 2479, 1516, 2120, 278, 1393, 1179, 1355, 854), ]


#Run the model again without outliers

fit = glm(
  imdb_score ~
    +bs(duration_mins, knots = c(96, 121), degree = 4)
  + bs(
    movie_budget,
    knots = c(9.7e+06, 5.0e+07),
    degree = 2
  )
  + bs(
    director_facebook_likes,
    knots = c(10, 258),
    degree = 3
  )
  + bs(
    cast_total_facebook_likes,
    knots = c(1911, 16701),
    degree = 1
  )
  + bs(
    number_news_articles,
    knots = c(81, 1003),
    degree = 1
  )
  + action
  + thriller
  + musical
  + romance
  + horror
  + drama
  + animation
  + crime
  + Biography
  + Comedy
  + Family
  #+country
  ,
  data = myvars_correct
)
summary(fit)

cv.error = cv.glm(myvars_correct_o, fit, K = 10)$delta[1]
print(cv.error)

##find polynomial degree again, This time without budget as budget information is not presnet for all movies yet##

timestamp(stamp = date())
dega = NULL
degb = NULL
degc = NULL
degd = NULL
dege = NULL
MSE = NULL
d = NULL
for (a in 1:6)
{
  for (b in 1:6)
  {
    for (c in 1:6)
    {
      for (d in 1:6)
      {
        for (e in 1:6)
        {
          dega = append(dega, a)
          degb = append(degb, b)
          degc = append(degc, c)
          degd = append(degd, d)
          dege = append(dege, e)
          glm.fit = glm(
            imdb_score ~
              +bs(
                duration_mins,
                knots = c(96, 121),
                degree = a
              )
            + bs(
              director_facebook_likes,
              knots = c(10, 258),
              degree = c
            )
            + bs(
              cast_total_facebook_likes,
              knots = c(1911, 16701),
              degree = d
            )
            + bs(
              number_news_articles,
              knots = c(81, 1003),
              degree = e
            )
            + action
            + thriller
            + musical
            + romance
            + horror
            + drama
            + animation
            + crime
            + Biography
            + Comedy
            + Family
            ,
            data = myvars_correct_o
          )
          MSE = append(MSE, cv.glm(myvars_correct_o, glm.fit, K = 10)$delta[1])
        }
      }
    }
  }
}

d = data.frame(dega, degb, degc, degd, dege, MSE)
d[which.min(d$MSE),]
timestamp(stamp = date())



fit = glm(
  imdb_score ~
    +bs(
      duration_mins,
      knots = c(96, 121),
      degree = 4
    )
  + bs(
    director_facebook_likes,
    knots = c(10, 258),
    degree = 6
  )
  + bs(
    cast_total_facebook_likes,
    knots = c(1911, 16701),
    degree = 4
  )
  + bs(
    number_news_articles,
    knots = c(81, 1003),
    degree = 4
  )
  + action
  + thriller
  + musical
  + romance
  + horror
  + drama
  + animation
  + crime
  + Biography
  + Comedy
  + Family
  ,
  data = myvars_correct_o
)

cv=0
for (i in 1:100)
{
  cv = cv + cv.glm(myvars_correct_o, fit, K = 10)$delta[1]
}

avgMSE = cv/100

pred_d = read.csv("/Users/manocha/Downloads/12Movies.csv")
names(pred_d)
