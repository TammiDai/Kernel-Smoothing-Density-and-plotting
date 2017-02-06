#epanechnikov kernel density function
epanechnikov <- function(a,x=0,h=1) {
u=(a-x)/h
  ifelse (abs(u)>1,0,0.75*(1-u^2)/h)
}


#plot all the ingredients that we need

curve(epanechnikov(a,x=0,h=1), xname="a", -3, 3,xlim=c(-3,3),ylim=c(0,2),col="violetred",
xlab="a",ylab="Density",lwd=2,frame.plot=FALSE)
curve(epanechnikov(a,x=0,h=0.5), xname="a",from = -3, to = 3, add = TRUE,xlim=c(-3,3),ylim=c(0,2),col="cadetblue3",lwd=2)
curve(epanechnikov(a,x=0,h=2), xname="a",from = -3, to = 3, add = TRUE,xlim=c(-3,3),ylim=c(0,2),col="slateblue2",lwd=2)
curve(epanechnikov(a,x=1,h=0.75),xname="a", from = -3, to = 3,add = TRUE,xlim=c(-3,3),ylim=c(0,2),col="darkgoldenrod1",lwd=2)
text(-2.2,1.7,"Epanechnikov")
expr1=expression(K[1](a))
expr2=expression(K[.5](a))
expr3=expression(K[2](a))
expr4=expression(K[.75](a-1))
legend(1.1, 2, legend = c(expr1, expr2,expr3,expr4),
       bty = "n", y.intersp = 1.25,lwd=2,lty=c(1,1,1,1),
       col = c("violetred", "cadetblue3","slateblue2","darkgoldenrod1"))




#connect dots and plot the smoothing kernel density
a.grid <- (seq(0,4,length.out = 500))

x.vec <- c(1, 1.2, 1.5, 2.8, 3) 

sand.piles.l<-lapply(x.vec, function(x) {(1/5) * (epanechnikov(a.grid, x, 0.75))})

sand.piles<-matrix(unlist(sand.piles.l), ncol = 5)

# Vectorized operations for plots 

make.curve <- function(idx) {
  ff <- function(x) { sand.piles[,idx] }
  curve(ff, from = 0, to = 4, n = 500, add = (idx > 1),ylim=c(0,0.6), lty = 2, col="blue3",
        xlab = "a", ylab = "Density",bty="n") 
  
}  

the.plots<-lapply(c(1:5), make.curve)
points(x.vec,rep(0,5),pch=16,col="red")
segments(x.vec,rep(0,5),x.vec,rep(1/5,5),lty=3,col="black")


piled.sand<-rowSums(sand.piles)
lines(a.grid,piled.sand,col="blue3",type="l",lty=1,lwd=1.8)






