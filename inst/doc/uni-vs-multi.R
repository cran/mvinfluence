## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  comment = "#>",
  fig.width =  7,
  fig.height = 6
)

options(digits = 4)

## ----packages-----------------------------------------------------------------
library(tibble)      # Simple Data Frames
library(ggplot2)     # Create Elegant Data Visualisations Using the Grammar of Graphics
library(car)         # Companion to Applied Regression
library(mvinfluence) # Influence Measures and Diagnostic Plots for Multivariate Linear Models
library(patchwork)   # The Composer of Plots
library(rgl)         # 3D Visualization Using OpenGL
rgl::setupKnitr(autoprint = TRUE)

## ----toy-data-----------------------------------------------------------------
Toy <- tibble(
   case = 1:9,
   x =  c(1,    1,    2,    2,    3,    3,    4,    4,    10),
   y1 = c(0.10, 1.90, 1.00, 2.95, 2.10, 4.00, 2.95, 4.95, 10.00),
   y2 = c(0.10, 1.80, 1.00, 2.93, 2.00, 4.10, 3.05, 4.93, 10.00)
)

## ----scatmat------------------------------------------------------------------
car::scatterplotMatrix(~y1 + y2 + x, data=Toy, cex=2,
        col = "blue", pch = 16,
        id = list(n=1, cex=2), 
        regLine = list(lwd = 2, col="red"),
        smooth = FALSE)

## -----------------------------------------------------------------------------
car::scatter3d(y1 ~ y2 + x, data=Toy,
               ellipsoid = TRUE,              # show the data ellipsoid
               radius = c(rep(1,8), 2),       # make case 9 larger
               grid.col = "pink", grid.lines = 10, 
               fill = FALSE,
               id = list(n=1), offset=2
               )
rgl::rglwidget()

## ----models-------------------------------------------------------------------
Toy.lm1 <- lm(y1 ~ x, data=Toy)
Toy.lm2 <- lm(y2 ~ x, data=Toy)
Toy.mlm <- lm(cbind(y1, y2) ~ x, data=Toy)

## ----coefs--------------------------------------------------------------------
coef(Toy.lm1)
coef(Toy.lm2)
coef(Toy.mlm)

## ----anovas-------------------------------------------------------------------
car::Anova(Toy.lm1)
car::Anova(Toy.lm2)
car::Anova(Toy.mlm)

## ----cooks-distance-----------------------------------------------------------
df <- Toy
df$D1  <- cooks.distance(Toy.lm1)
df$D2  <- cooks.distance(Toy.lm2)
df$D12 <- cooks.distance(Toy.mlm)

df

## ----inflplots, fig.show='hold', out.width="45%", out.height="50%", collapse=FALSE----
ip1 <- car::influencePlot(Toy.lm1, id=list(cex=1.5))
ip2 <- car::influencePlot(Toy.lm2, id=list(cex=1.5))

## ----inflplot-mlm, fig.width=5, fig.height=4, fig.align='center'--------------
par(mar = c(4,4,1,1)+.1)
influencePlot(Toy.mlm, id.n=2)

## ----inflplot-mlm-lr, fig.width=5, fig.height=4, fig.align='center', results='hide'----
par(mar = c(4,4,1,1)+.1)
influencePlot(Toy.mlm, id.n=2, type = 'LR')

## ----dfbetas-plot, fig.width=9, fig.height=4.5--------------------------------
db1 <- as.data.frame(dfbetas(Toy.lm1))
gg1 <- ggplot(data = db1, aes(x=`(Intercept)`, y=x, label=rownames(db1))) +
  geom_point(size=1.5) +
  geom_label(size=6, fill="pink") +
  xlab(expression(paste("Deletion Intercept  ", b[0]))) +
  ylab(expression(paste("Deletion Slope  ", b[1]))) +
  ggtitle("dfbetas for y1") +
  theme_bw(base_size = 16)

db2 <- as.data.frame(dfbetas(Toy.lm2))
gg2 <- ggplot(data = db2, aes(x=`(Intercept)`, y=x, label=rownames(db2))) +
  geom_point(size=1.5) +
  geom_label(size=6, fill="pink") +
  xlab(expression(paste("Deletion Intercept  ", b[0]))) +
  ylab(expression(paste("Deletion Slope  ", b[1]))) +
  ggtitle("dfbetas for y2") +
  theme_bw(base_size = 16)

gg1 + gg2

