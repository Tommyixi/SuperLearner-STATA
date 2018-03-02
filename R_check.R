library("SuperLearner")

# Not run: 
cars <- read.csv("cars.csv")
cars_altered <- read.csv("cars_altered.csv")

cars_without_mpg <- cars[ , (names(cars) %in% c("weight", "trunk", "price", "length"))]

custom_a<- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {}
  if(family$family=='gaussian'){
    fit.lm<- lm(Y~ weight + trunk + price, data=X, family=family)
    pred <- predict(fit.lm, newdata=newX, type='response')
    fit<- list(object=fit.lm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('custom_a')
  return(out)
}
custom_b<- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {}
  if(family$family=='gaussian'){
    fit.lm<- lm(Y~ weight + trunk, data=X, family=family)
    pred <- predict(fit.lm, newdata=newX, type='response')
    fit<- list(object=fit.lm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('custom_b')
  return(out)
}
custom_c<- function(Y, X, newX, family, ...) {
  if(family$family=='binomial') {}
  if(family$family=='gaussian'){
    fit.lm<- lm(Y~ weight + length, data=X, family=family)
    pred <- predict(fit.lm, newdata=newX, type='response')
    fit<- list(object=fit.lm)
  }
  
  out <- list(pred=pred, fit=fit)
  class(out$fit) <- c('custom_c')
  return(out)
}


# generate Library and run Super Learner
cars$binom <- rbinom(nrow(cars), 1, .5)
SL.library <- c("custom_a", "custom_b", "custom_c")
SL.library2 <- c("SL.mean", "SL.mean")
test <- SuperLearner(Y = cars$mpg, X = cars_without_mpg, SL.library = SL.library,
                     verbose = FALSE, family = "gaussian")
a <- CV.SuperLearner(Y = cars$mpg, X = cars_without_mpg, SL.library = SL.library,
                     verbose = FALSE, family = "gaussian")

b <- SuperLearner(Y = cars$binom, X = cars_without_mpg, SL.library = SL.library,
                  verbose = FALSE, family = "gaussian")

b_2 <- SuperLearner(Y = cars$binom, X = cars_without_mpg, SL.library = SL.library,
                  verbose = FALSE, family = "binomial")
