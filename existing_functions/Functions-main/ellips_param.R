

eDir <-function(VecVal)
{
  ellipDir <- ifelse(VecVal$values[1]>VecVal$values[2],
                     atan2( VecVal$vectors[1,1],VecVal$vectors[2,1])*180/pi,
                     atan2( VecVal$vectors[1,2],VecVal$vectors[2,2])*180/pi)
  ellipDir <- ifelse(ellipDir>0,ellipDir,ellipDir+180)
  return(ellipDir)
}

ellipsDir <- function(VARX,VARY,COVXY)
{
  x <- cbind(VARX, COVXY, COVXY,VARY)
  listOfMat <- lapply(seq_len(nrow(x)), function(i) x[i,])
  listOfMat <- lapply(listOfMat,matrix,ncol=2)
  listOfVecVal <- lapply(listOfMat,eigen)
  return(unlist(lapply(listOfVecVal,eDir)))

}

medlag5 <- function(x)
{
  xx <- cbind(lag(x,1),lag(x,2),lag(x,3),lag(x,4),lag(x,5))
  listOfLags <- lapply(seq_len(nrow(xx)), function(i) xx[i,])
  return(unlist(lapply(listOfLags,median,na.rm=T)))
}


# ellipsDir(1,5,1)
# 
# Q <- RawLoc1[1:10,]
# 
# Q <- Q %>%  mutate(val1=sqrt(((VARX+VARY)-sqrt(VARX^2+VARY^2-2*VARX*VARY+4*COVXY^2))/2),
#                   val2=sqrt(((VARX+VARY)+sqrt(VARX^2+VARY^2-2*VARX*VARY+4*COVXY^2))/2),
#                   ellipsDir=ellipsDir(VARX,VARY,COVXY)) %>% 
#             group_by(TAG) %>% 
#             mutate(dX=X-medlag5(X), #X-median(c(lag(X,1),lag(X,2),lag(X,3),lag(X,4),lag(X,5))),
#                    dY=Y-medlag5(Y), #median(c(lag(Y,1),lag(Y,2),lag(Y,3),lag(Y,4),lag(Y,5))),
#                    moveAngle= atan2(dX,dY)*180/pi,
#                    moveAngle= ifelse(moveAngle>0,moveAngle,moveAngle+360),
#                    projMovStdaxis=abs(cos((moveAngle-ellipsDir)*pi/180)),
#                    #filter options: val2*projMovStdaxis>12, distance*projMovStdaxis>50 or different combinations of them
#             ) %>% 
#             ungroup()

# RawLoc1 <- RawLoc0 %>% mutate(val1=sqrt(((VARX+VARY)-sqrt(VARX^2+VARY^2-2*VARX*VARY+4*COVXY^2))/2),
#                               val2=sqrt(((VARX+VARY)+sqrt(VARX^2+VARY^2-2*VARX*VARY+4*COVXY^2))/2),
#                               ellipsDir=ellipsDir(VARX,VARY,COVXY))
# 
# require('ellipse')

# plot(RawLoc1$X,RawLoc1$Y)
# Index=3
# points(RawLoc1$X[Index],RawLoc1$Y[Index],col='blue')
# lines( ellipse(matrix(cbind(RawLoc1$VARX[Index], RawLoc1$COVXY[Index], RawLoc1$COVXY[Index],RawLoc1$VARY[Index]),2,2),
#                centre = c(RawLoc1$X[Index],RawLoc1$Y[Index])),col='red')
# 
# mu <- c(RawLoc1$X[Index],RawLoc1$Y[Index])
# # Get the correlation matrix
# P <- matrix(cbind(RawLoc1$VARX[Index], RawLoc1$COVXY[Index], RawLoc1$COVXY[Index],RawLoc1$VARY[Index]),2,2)
# 
# ValVec <- eigen(P)
# # Angles of a circle
# a <- seq(0, 2*pi, len=100)
# # Get the distances
# xT <- sqrt(ValVec$values[1]) * cos(a)
# yT <- sqrt(ValVec$values[2]) * sin(a)
# M <- cbind(xT, yT)
# # Covert the coordinates
# transM <- t(evecs %*% t(M))
# 
# lines(transM[,1] + mu[1],transM[,2] + mu[2],col='yellow')