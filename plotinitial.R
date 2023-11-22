x=1

A6x<-0
B6x<-2.5
C6x<-5
D6x<-7.5
A6y<-10
B6y<-10
C6y<-10
D6y<-10

G6x<-c(A6x,B6x,C6x,D6x)
G6y<-c(A6y,B6y,C6y,D6y)

A5x<-0
B5x<-2.5
C5x<-5
D5x<-7.5
A5y<--10
B5y<--10
C5y<--10
D5y<--10

G5x<-c(A5x,B5x,C5x,D5x)
G5y<-c(A5y,B5y,C5y,D5y)

A4x<--25
B4x<--22.5
C4x<--20
D4x<--17.5
A4y<-15
B4y<-15
C4y<-15
D4y<-15

G4x<-c(A4x,B4x,C4x,D4x)
G4y<-c(A4y,B4y,C4y,D4y)

A3x<--20
B3x<--17.5
C3x<--15
D3x<--12.5
A3y<-5
B3y<-5
C3y<-5
D3y<-5

G3x<-c(A3x,B3x,C3x,D3x)
G3y<-c(A3y,B3y,C3y,D3y)

A2x<--20
B2x<--17.5
C2x<--15
D2x<--12.5
A2y<--5
B2y<--5
C2y<--5
D2y<--5

G2x<-c(A2x,B2x,C2x,D2x)
G2y<-c(A2y,B2y,C2y,D2y)

A1x<--30
B1x<--30
C1x<--30
D1x<--30
A1y<--10
B1y<--12.5
C1y<--15
D1y<--17.5

G1x<-c(A1x,B1x,C1x,D1x)
G1y<-c(A1y,B1y,C1y,D1y)

##Plot command

plot(x,x,asp=1,xlim=c(-50,50),ylim=c(-50,50),type="n")
abline(h=seq(-50,50,2.5),lty=2)
abline(v=seq(-50,50,2.5),lty=2)
points(G1x,G1y,pch=20,col=c("brown"),type="l",lwd=2)
points(G2x,G2y,pch=20,col=c("black"),type="l",lwd=2)
points(G3x,G3y,pch=20,col=c("red"),type="l",lwd=2)
points(G4x,G4y,pch=20,col=c("cyan"),type="l",lwd=2)
points(G5x,G5y,pch=20,col=c("violet"),type="l",lwd=2)
points(G6x,G6y,pch=20,col=c("green"),type="l",lwd=2)
points(G1x,G1y,pch=20,col=c("Blue","Green","orange","red"))
points(G2x,G2y,pch=20,col=c("Blue","Green","orange","red"))
points(G3x,G3y,pch=20,col=c("Blue","Green","orange","red"))
points(G4x,G4y,pch=20,col=c("Blue","Green","orange","red"))
points(G5x,G5y,pch=20,col=c("Blue","Green","orange","red"))
points(G6x,G6y,pch=20,col=c("Blue","Green","orange","red"))


