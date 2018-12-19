# https://www.cyclismo.org/tutorial/R/tables.html



smoke <- matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
colnames(smoke) <- c("High","Low","Middle")
rownames(smoke) <- c("current","former","never")
smoke <- as.table(smoke)
smoke

barplot(smoke,legend=T,beside=T,main='Smoking Status by SES')
plot(smoke,main="Smoking Status By Socioeconomic Status")

margin.table(smoke)
margin.table(smoke,1)
margin.table(smoke,2)

smoke/margin.table(smoke)
margin.table(smoke,1)/margin.table(smoke)
margin.table(smoke,2)/margin.table(smoke)


prop.table(smoke)
prop.table(smoke,1)
prop.table(smoke,2)

summary(smoke)

expected <- as.array(margin.table(smoke,1)) %*% t(as.array(margin.table(smoke,2))) / margin.table(smoke)
expected


chi <- sum((expected - as.array(smoke))^2/expected)
chi


mosaicplot(smoke)
help(mosaicplot)

mosaicplot(smoke,main="Smokers",xlab="Status",ylab="Economic Class")


mosaicplot(smoke,main="Smokers",xlab="Status",ylab="Economic Class")
mosaicplot(smoke,sort=c(2,1))


mosaicplot(smoke,main="Smokers",xlab="Status",ylab="Economic Class")
mosaicplot(smoke,dir=c("v","h"))


# http://tinyheero.github.io/2016/03/20/basic-prob.html
library("ggplot2")
library("dplyr")
library("reshape2")
library("knitr")

diamonds.color.cut.df <-
  diamonds %>%
  group_by(color, cut) %>%
  summarize(n = n())

diamonds.color.cut.df %>%
  dcast(color ~ cut, value.nar = "n") %>%
  kable(align = "l", format = "pandoc")

diamonds.color.cut.df %>%
  dcast(color ~ cut, value.nar = "n") %>%
  kable(align = "l", format = "rst")

diamonds.color.cut.df %>%
  dcast(color ~ cut, value.nar = "n") %>%
  kable(align = "l", format = "markdown")


diamonds.color.cut.prop.df <- 
  diamonds.color.cut.df %>%
  ungroup() %>%
  mutate(prop = n / sum(n))

diamonds.color.cut.prop.df %>%
  dcast(color ~ cut, value.var = "prop") %>%
  kable(align = "l", format = "rst")



color.marginal.df <- 
  diamonds.color.cut.prop.df %>%
  group_by(color) %>%
  summarize(marginal = sum(prop))

cut.marginal.df <- 
  diamonds.color.cut.prop.df %>%
  group_by(cut) %>%
  summarize(marginal = sum(prop))

diamonds.color.cut.prop.df %>%
  dcast(color ~ cut, value.var = "prop") %>%
  left_join(color.marginal.df, by = "color") %>%
  bind_rows(
    cut.marginal.df %>%
      mutate(color = "marginal") %>%
      dcast(color ~ cut, value.var = "marginal")
  ) %>%
  kable(align = "l", format = "rst")




joint.prob <- 
  diamonds.color.cut.prop.df %>%
  filter(color == "G", cut == "Ideal") %>%
  .$prop

marg.prob <- 
  cut.marginal.df %>%
  filter(cut == "Ideal") %>%
  .$marginal

cond.prob <- joint.prob / marg.prob
cond.prob



#https://rstudio-pubs-static.s3.amazonaws.com/209289_9f9ba331cccc4e8f8aabdb9273cc76af.html

p <- matrix(c(.02,.04,.01,.06,.15,.15,.02,.20,.14,.10,.10,.01),ncol=4) ## this line creates matrix p
p  
colnames(p) <- c("Y=0","Y=5","Y=10", "Y=15")
rownames(p) <- c("X=0","X=5","X=10")
p

test1 <- c(1,2,3,4)  ## this line creates the object called "test1"
test1  
test2 <- matrix(c(1,2,3,4), ncol=1)  ## this line creates the object called "test2"
test2
test3 <- matrix(c(1,2,3,4),ncol=2)   ## this line creates the object called "test3"
test3
test4 <- matrix(c(1,2,3,4,5,6), ncol=2)  ## this line creates the object called "test4"
test4
test5 <- matrix(c(1,2,3,4,5,6), ncol=3)  ## this line creates the object called "test5"
test5
test6 <- matrix(c(1,2,3,4,5,6,7,8,9), ncol=3)  ## this line creates the object called "test5"
test6
test7 <- matrix(c(1,2,3,4,5,6,7,8), ncol=2)  ## this line creates the object called "test5"
test7
p[2,3]
p[3,2]
sum(p)
px <- apply(p,1,sum) ## create marginal probabilities for X  
px                   ## display these marginal probabilities
py <- apply(p,2,sum) ## create marginal probabilities for X  
py                   ## display these marginal probabilities

p[2,2]
py[2]
p_x5_y5 <- p[2,2]/py[2]  ## computes conditional probability P(X=5|Y=5)
p_x5_y5  

p_x_y5 <- c(p_x0_y5,p_x5_y5,p_x10_y5)
x<- c(0,5,10)
y<- c(0,5,10,15)
EX  <- sum(px*x)   ## expectation of X
EX
EX2 <- sum(px*x^2) ## expectation of X^2
EX2
EX_Y5 <- sum(p_x_y5*x)
