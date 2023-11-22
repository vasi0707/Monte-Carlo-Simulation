vasanthcombined<-function() {
  
 
 
  Group1x<-G1x
  Group2x<-G2x
  Group3x<-G3x
  Group4x<-G4x
  Group5x<-G5x
  Group6x<-G6x
  Group1y<-G1y
  Group2y<-G2y
  Group3y<-G3y
  Group4y<-G4y
  Group5y<-G5y
  Group6y<-G6y
  Einit=0
  
  for(i in 1:1000000) {
    s1=sample(1:8,1)
    if(s1==1){
      
      modgroup1x=Group1x-2.5
      modgroup1y=Group1y
    }
    else if (s1==2) {
      modgroup1x=Group1x+2.5
      modgroup1y=Group1y
      
    }
    else if (s1==3) {
      modgroup1x=Group1x
      modgroup1y=Group1y+2.5
      
    }
    else if (s1==4) {
      modgroup1x=Group1x
      modgroup1y=Group1y-2.5
      
    }
    
    else if (s1==5) {
      
      if(Group1x[1]==Group1x[2]) {
        
        modgroup1x=Group1x
        modgroup1y=Group1y
        
        if(Group1y[1]>Group1y[2]) {
        
        modgroup1x[1]=Group1x[1]+2.5
        modgroup1y[1]=Group1y[1]-2.5
        }
        else {
          
          modgroup1x[1]=Group1x[1]-2.5
          modgroup1y[1]=Group1y[1]+2.5
          
          
        }
        
      }
      else if(Group1y[1]==Group1y[2]){
        
        modgroup1x=Group1x
        modgroup1y=Group1y
        
        if (Group1x[1]<Group1x[2]) {
        
        
        modgroup1x[1]=Group1x[1]+2.5
        modgroup1y[1]=Group1y[1]+2.5
        
        }
        
        else {
          
          modgroup1x[1]=Group1x[1]-2.5
          modgroup1y[1]=Group1y[1]-2.5
          
          
        }
        
      }
      
    }
    
    else if (s1==6) {
      
      if(Group1x[1]==Group1x[2]) {
        
        modgroup1x=Group1x
        modgroup1y=Group1y
        
        if(Group1y[1]>Group1y[2]) {
        
        
        modgroup1x[1]=Group1x[1]-2.5
        modgroup1y[1]=Group1y[1]-2.5
        }
        
        else {
          modgroup1x[1]=Group1x[1]+2.5
          modgroup1y[1]=Group1y[1]+2.5
          
        }
        
      }
      else if(Group1y[1]==Group1y[2]){
        
        modgroup1x=Group1x
        modgroup1y=Group1y
        
        if (Group1x[1]<Group1x[2]) {
        
        modgroup1x[1]=Group1x[1]+2.5
        modgroup1y[1]=Group1y[1]-2.5
        }
        
        else {
          
          modgroup1x[1]=Group1x[1]-2.5
          modgroup1y[1]=Group1y[1]+2.5
          
          
          
        }
        
      }
      
    }
    
    else if (s1==7) {
      
      if(Group1x[4]==Group1x[3]) {
        
        modgroup1x=Group1x
        modgroup1y=Group1y
        
        if(Group1y[4]<Group1y[3]) {
        modgroup1x[4]=Group1x[4]-2.5
        modgroup1y[4]=Group1y[4]+2.5
        
        }
        
        else {
          modgroup1x[4]=Group1x[4]+2.5
          modgroup1y[4]=Group1y[4]-2.5
          
          }
        
      }
      else if(Group1y[4]==Group1y[3]){
        
        modgroup1x=Group1x
        modgroup1y=Group1y
        
        if(Group1x[4]>Group1x[3]) {
        modgroup1x[4]=Group1x[4]-2.5
        modgroup1y[4]=Group1y[4]-2.5
        }
        
        else{
          modgroup1x[4]=Group1x[4]+2.5
          modgroup1y[4]=Group1y[4]+2.5
          
        }
      }
      
    }
    
    else if (s1==8) {
      
      if(Group1x[4]==Group1x[3]) {
        
        modgroup1x=Group1x
        modgroup1y=Group1y
        
        if(Group1y[4]>Group1y[3]) {
        modgroup1x[4]=Group1x[4]-2.5
        modgroup1y[4]=Group1y[4]-2.5
        
        }
        
        else {
          modgroup1x[4]=Group1x[4]+2.5
          modgroup1y[4]=Group1y[4]+2.5
        
        }
        
      }
      else if(Group1y[4]==Group1y[3]){
        
        modgroup1x=Group1x
        modgroup1y=Group1y
        
        if(Group1x[4]>Group1x[3]) {
        modgroup1x[4]=Group1x[4]-2.5
        modgroup1y[4]=Group1y[4]+2.5
        }
        
        else{
          
          modgroup1x[4]=Group1x[4]+2.5
          modgroup1y[4]=Group1y[4]-2.5
          
          
        }
        
      }
      
    }
    
    
    if(max(abs(modgroup1x))<=50 && max(abs(modgroup1y))<=50) {
      
      poolx<-c(modgroup1x,Group2x,Group3x,Group4x, Group5x,Group6x)
      pooly<-c(modgroup1y,Group2y,Group3y,Group4y, Group5y,Group6y)
      pool<-paste(poolx,pooly)
      
      if(length(unique(pool))!=24) {
        Group1x=Group1x
        Group1y=Group1y
        Einit=Einit
      }
      
      else if(length(unique(pool))==24) {
        
        x12=(abs(modgroup1x-Group2x)==2.5)
        x13=(abs(modgroup1x-Group3x)==2.5)
        x14=(abs(modgroup1x-Group4x)==2.5)
        x15=(abs(modgroup1x-Group5x)==2.5)
        x16=(abs(modgroup1x-Group6x)==2.5)
        y12=(abs(modgroup1y-Group2y)==2.5)
        y13=(abs(modgroup1y-Group3y)==2.5)
        y14=(abs(modgroup1y-Group4y)==2.5)
        y15=(abs(modgroup1y-Group5y)==2.5)
        y16=(abs(modgroup1y-Group6y)==2.5)
        
        xx12=(abs(modgroup1x-Group2x)==0)
        xx13=(abs(modgroup1x-Group3x)==0)
        xx14=(abs(modgroup1x-Group4x)==0)
        xx15=(abs(modgroup1x-Group5x)==0)
        xx16=(abs(modgroup1x-Group6x)==0)
        yy12=(abs(modgroup1y-Group2y)==0)
        yy13=(abs(modgroup1y-Group3y)==0)
        yy14=(abs(modgroup1y-Group4y)==0)
        yy15=(abs(modgroup1y-Group5y)==0)
        yy16=(abs(modgroup1y-Group6y)==0)
        
        xlogpool=c(x12,x13,x14,x15,x16)
        ylogpool=c(y12,y13,y14,y15,y16)
        logpool=xor(xlogpool,ylogpool)
        
        xxlogpool=c(xx12,xx13,xx14,xx15,xx16)
        yylogpool=c(yy12,yy13,yy14,yy15,yy16)
        xylogpool=xor(xxlogpool,yylogpool)
        
        
        Efinal=-1*sum(logpool & xylogpool)
        
        delta= Einit-Efinal
        
        if(delta<=0) {
          
          Einit=Efinal
          Group1x=modgroup1x
          Group1y=modgroup1y
        }
        
        else if(delta>0) {
          
          rand=runif(1)
          interim=exp(-1*delta)
          
          if(interim>rand) {
            Einit=Efinal
            Group1x=modgroup1x
            Group1y=modgroup1y
            
          }
          
          else {
            Group1x=Group1x
            Group1y=Group1y
            Einit=Einit
            
          }
        }
        
      }
      print("Group1")
      print(s1)
      print(length(unique(pool)))
      print(delta)
      print(Efinal)
      print(exp(-1*delta))
      print(Group1x)
      print(Group1y)
      
    }
    
    else {
    Group1x=Group1x
    Group1y=Group1y
    Einit=Einit
    
    print("Group1")
    print(s1)
    print(length(unique(pool)))
    print(delta)
    print(Efinal)
    print(exp(-1*delta))
    print(Group1x)
    print(Group1y)
    
    
    
    
    }
    
    s2=sample(1:8,1)
    if(s2==1){
      
      modgroup2x=Group2x-2.5
      modgroup2y=Group2y
    }
    else if (s2==2) {
      modgroup2x=Group2x+2.5
      modgroup2y=Group2y
      
    }
    else if (s2==3) {
      modgroup2x=Group2x
      modgroup2y=Group2y+2.5
      
    }
    else if (s2==4) {
      modgroup2x=Group2x
      modgroup2y=Group2y-2.5
      
    }
    
    else if (s2==5) {
      
      if(Group2x[1]==Group2x[2]) {
        
        modgroup2x=Group2x
        modgroup2y=Group2y
        
        if(Group2y[1]>Group2y[2]) {
          
          modgroup2x[1]=Group2x[1]+2.5
          modgroup2y[1]=Group2y[1]-2.5
        }
        else {
          
          modgroup2x[1]=Group2x[1]-2.5
          modgroup2y[1]=Group2y[1]+2.5
          
          
        }
        
      }
      else if(Group2y[1]==Group2y[2]){
        
        modgroup2x=Group2x
        modgroup2y=Group2y
        
        if (Group2x[1]<Group2x[2]) {
          
          
          modgroup2x[1]=Group2x[1]+2.5
          modgroup2y[1]=Group2y[1]+2.5
          
        }
        
        else {
          
          modgroup2x[1]=Group2x[1]-2.5
          modgroup2y[1]=Group2y[1]-2.5
          
          
        }
        
      }
      
    }
    
    else if (s2==6) {
      
      if(Group2x[1]==Group2x[2]) {
        
        modgroup2x=Group2x
        modgroup2y=Group2y
        
        if(Group2y[1]>Group2y[2]) {
          
          
          modgroup2x[1]=Group2x[1]-2.5
          modgroup2y[1]=Group2y[1]-2.5
        }
        
        else {
          modgroup2x[1]=Group2x[1]+2.5
          modgroup2y[1]=Group2y[1]+2.5
          
        }
        
      }
      else if(Group2y[1]==Group2y[2]){
        
        modgroup2x=Group2x
        modgroup2y=Group2y
        
        if (Group2x[1]<Group2x[2]) {
          
          modgroup2x[1]=Group2x[1]+2.5
          modgroup2y[1]=Group2y[1]-2.5
        }
        
        else {
          
          modgroup2x[1]=Group2x[1]-2.5
          modgroup2y[1]=Group2y[1]+2.5
          
          
          
        }
        
      }
      
    }
    
    else if (s2==7) {
      
      if(Group2x[4]==Group2x[3]) {
        
        modgroup2x=Group2x
        modgroup2y=Group2y
        
        if(Group2y[4]<Group2y[3]) {
          modgroup2x[4]=Group2x[4]-2.5
          modgroup2y[4]=Group2y[4]+2.5
          
        }
        
        else {
          modgroup2x[4]=Group2x[4]+2.5
          modgroup2y[4]=Group2y[4]-2.5
          
        }
        
      }
      else if(Group2y[4]==Group2y[3]){
        
        modgroup2x=Group2x
        modgroup2y=Group2y
        
        if(Group2x[4]>Group2x[3]) {
          modgroup2x[4]=Group2x[4]-2.5
          modgroup2y[4]=Group2y[4]-2.5
        }
        
        else{
          modgroup2x[4]=Group2x[4]+2.5
          modgroup2y[4]=Group2y[4]+2.5
          
        }
      }
      
    }
    
    else if (s2==8) {
      
      if(Group2x[4]==Group2x[3]) {
        
        modgroup2x=Group2x
        modgroup2y=Group2y
        
        if(Group2y[4]>Group2y[3]) {
          modgroup2x[4]=Group2x[4]-2.5
          modgroup2y[4]=Group2y[4]-2.5
          
        }
        
        else {
          modgroup2x[4]=Group2x[4]+2.5
          modgroup2y[4]=Group2y[4]+2.5
          
        }
        
      }
      else if(Group2y[4]==Group2y[3]){
        
        modgroup2x=Group2x
        modgroup2y=Group2y
        
        if(Group2x[4]>Group2x[3]) {
          modgroup2x[4]=Group2x[4]-2.5
          modgroup2y[4]=Group2y[4]+2.5
        }
        
        else{
          
          modgroup2x[4]=Group2x[4]+2.5
          modgroup2y[4]=Group2y[4]-2.5
          
          
        }
        
      }
      
    }
    
    if(max(abs(modgroup2x))<=50 && max(abs(modgroup2y))<=50) {
      
      poolx<-c(modgroup2x,Group1x,Group3x,Group4x, Group5x,Group6x)
      pooly<-c(modgroup2y,Group1y,Group3y,Group4y, Group5y,Group6y)
      pool<-paste(poolx,pooly)
      
      if(length(unique(pool))!=24) {
        Group2x=Group2x
        Group2y=Group2y
        Einit=Einit
      }
      
      else if(length(unique(pool))==24) {
        
        x21=(abs(modgroup2x-Group1x)==2.5)
        x23=(abs(modgroup2x-Group3x)==2.5)
        x24=(abs(modgroup2x-Group4x)==2.5)
        x25=(abs(modgroup2x-Group5x)==2.5)
        x26=(abs(modgroup2x-Group6x)==2.5)
        y21=(abs(modgroup2y-Group1y)==2.5)
        y23=(abs(modgroup2y-Group3y)==2.5)
        y24=(abs(modgroup2y-Group4y)==2.5)
        y25=(abs(modgroup2y-Group5y)==2.5)
        y26=(abs(modgroup2y-Group6y)==2.5)
        
        xx21=(abs(modgroup2x-Group1x)==0)
        xx23=(abs(modgroup2x-Group3x)==0)
        xx24=(abs(modgroup2x-Group4x)==0)
        xx25=(abs(modgroup2x-Group5x)==0)
        xx26=(abs(modgroup2x-Group6x)==0)
        yy21=(abs(modgroup2y-Group1y)==0)
        yy23=(abs(modgroup2y-Group3y)==0)
        yy24=(abs(modgroup2y-Group4y)==0)
        yy25=(abs(modgroup2y-Group5y)==0)
        yy26=(abs(modgroup2y-Group6y)==0)
        
        xlogpool=c(x21,x23,x24,x25,x26)
        ylogpool=c(y21,y23,y24,y25,y26)
        logpool=xor(xlogpool,ylogpool)
        
        xxlogpool=c(xx21,xx23,xx24,xx25,xx26)
        yylogpool=c(yy21,yy23,yy24,yy25,yy26)
        xylogpool=xor(xxlogpool,yylogpool)
        
        
        Efinal=-1*sum(logpool & xylogpool)
        
        delta= Einit-Efinal
        
        if(delta<=0) {
          
          Einit=Efinal
          Group2x=modgroup2x
          Group2y=modgroup2y
        }
        
        else if(delta>0) {
          
          rand=runif(1)
          interim=exp(-1*delta)
          
          if(interim>rand) {
            Einit=Efinal
            Group2x=modgroup2x
            Group2y=modgroup2y
            
          }
          
          else {
            Group2x=Group2x
            Group2y=Group2y
            Einit=Einit
            
          }
        }
        
      }
      print("Group2")
      print(s2)
      print(length(unique(pool)))
      print(delta)
      print(Efinal)
      print(exp(-1*delta))
      print(Group2x)
      print(Group2y)
      
    }
    
    else {Group2x=Group2x
    Group2y=Group2y
    Einit=Einit
    
    print("Group2")
    print(s2)
    print(length(unique(pool)))
    print(delta)
    print(Efinal)
    print(exp(-1*delta))
    print(Group2x)
    print(Group2y)
    
    
    
    
    }
    
    s3=sample(1:8,1)
    if(s3==1){
      
      modgroup3x=Group3x-2.5
      modgroup3y=Group3y
    }
    else if (s3==2) {
      modgroup3x=Group3x+2.5
      modgroup3y=Group3y
      
    }
    else if (s3==3) {
      modgroup3x=Group3x
      modgroup3y=Group3y+2.5
      
    }
    else if (s3==4) {
      modgroup3x=Group3x
      modgroup3y=Group3y-2.5
      
    }
    
    else if (s3==5) {
      
      if(Group3x[1]==Group3x[2]) {
        
        modgroup3x=Group3x
        modgroup3y=Group3y
        
        if(Group3y[1]>Group3y[2]) {
          
          modgroup3x[1]=Group3x[1]+2.5
          modgroup3y[1]=Group3y[1]-2.5
        }
        else {
          
          modgroup3x[1]=Group3x[1]-2.5
          modgroup3y[1]=Group3y[1]+2.5
          
          
        }
        
      }
      else if(Group3y[1]==Group3y[2]){
        
        modgroup3x=Group3x
        modgroup3y=Group3y
        
        if (Group3x[1]<Group3x[2]) {
          
          
          modgroup3x[1]=Group3x[1]+2.5
          modgroup3y[1]=Group3y[1]+2.5
          
        }
        
        else {
          
          modgroup3x[1]=Group3x[1]-2.5
          modgroup3y[1]=Group3y[1]-2.5
          
          
        }
        
      }
      
    }
    
    else if (s3==6) {
      
      if(Group3x[1]==Group3x[2]) {
        
        modgroup3x=Group3x
        modgroup3y=Group3y
        
        if(Group3y[1]>Group3y[2]) {
          
          
          modgroup3x[1]=Group3x[1]-2.5
          modgroup3y[1]=Group3y[1]-2.5
        }
        
        else {
          modgroup3x[1]=Group3x[1]+2.5
          modgroup3y[1]=Group3y[1]+2.5
          
        }
        
      }
      else if(Group3y[1]==Group3y[2]){
        
        modgroup3x=Group3x
        modgroup3y=Group3y
        
        if (Group3x[1]<Group3x[2]) {
          
          modgroup3x[1]=Group3x[1]+2.5
          modgroup3y[1]=Group3y[1]-2.5
        }
        
        else {
          
          modgroup3x[1]=Group3x[1]-2.5
          modgroup3y[1]=Group3y[1]+2.5
          
          
          
        }
        
      }
      
    }
    
    else if (s3==7) {
      
      if(Group3x[4]==Group3x[3]) {
        
        modgroup3x=Group3x
        modgroup3y=Group3y
        
        if(Group3y[4]<Group3y[3]) {
          modgroup3x[4]=Group3x[4]-2.5
          modgroup3y[4]=Group3y[4]+2.5
          
        }
        
        else {
          modgroup3x[4]=Group3x[4]+2.5
          modgroup3y[4]=Group3y[4]-2.5
          
        }
        
      }
      else if(Group3y[4]==Group3y[3]){
        
        modgroup3x=Group3x
        modgroup3y=Group3y
        
        if(Group3x[4]>Group3x[3]) {
          modgroup3x[4]=Group3x[4]-2.5
          modgroup3y[4]=Group3y[4]-2.5
        }
        
        else{
          modgroup3x[4]=Group3x[4]+2.5
          modgroup3y[4]=Group3y[4]+2.5
          
        }
      }
      
    }
    
    else if (s3==8) {
      
      if(Group3x[4]==Group3x[3]) {
        
        modgroup3x=Group3x
        modgroup3y=Group3y
        
        if(Group3y[4]>Group3y[3]) {
          modgroup3x[4]=Group3x[4]-2.5
          modgroup3y[4]=Group3y[4]-2.5
          
        }
        
        else {
          modgroup3x[4]=Group3x[4]+2.5
          modgroup3y[4]=Group3y[4]+2.5
          
        }
        
      }
      else if(Group3y[4]==Group3y[3]){
        
        modgroup3x=Group3x
        modgroup3y=Group3y
        
        if(Group3x[4]>Group3x[3]) {
          modgroup3x[4]=Group3x[4]-2.5
          modgroup3y[4]=Group3y[4]+2.5
        }
        
        else{
          
          modgroup3x[4]=Group3x[4]+2.5
          modgroup3y[4]=Group3y[4]-2.5
          
          
        }
        
      }
      
    }
    
    
    
    if(max(abs(modgroup3x))<=50 && max(abs(modgroup3y))<=50) {
      
      poolx<-c(modgroup3x,Group1x,Group2x,Group4x, Group5x,Group6x)
      pooly<-c(modgroup3y,Group1y,Group2y,Group4y, Group5y,Group6y)
      pool<-paste(poolx,pooly)
      
      if(length(unique(pool))!=24) {
        Group3x=Group3x
        Group3y=Group3y
        Einit=Einit
      }
      
      else if(length(unique(pool))==24) {
        
        x31=(abs(modgroup3x-Group1x)==2.5)
        x32=(abs(modgroup3x-Group2x)==2.5)
        x34=(abs(modgroup3x-Group4x)==2.5)
        x35=(abs(modgroup3x-Group5x)==2.5)
        x36=(abs(modgroup3x-Group6x)==2.5)
        y31=(abs(modgroup3y-Group1y)==2.5)
        y32=(abs(modgroup3y-Group2y)==2.5)
        y34=(abs(modgroup3y-Group4y)==2.5)
        y35=(abs(modgroup3y-Group5y)==2.5)
        y36=(abs(modgroup3y-Group6y)==2.5)
        
        xx31=(abs(modgroup3x-Group1x)==0)
        xx32=(abs(modgroup3x-Group2x)==0)
        xx34=(abs(modgroup3x-Group4x)==0)
        xx35=(abs(modgroup3x-Group5x)==0)
        xx36=(abs(modgroup3x-Group6x)==0)
        yy31=(abs(modgroup3y-Group1y)==0)
        yy32=(abs(modgroup3y-Group2y)==0)
        yy34=(abs(modgroup3y-Group4y)==0)
        yy35=(abs(modgroup3y-Group5y)==0)
        yy36=(abs(modgroup3y-Group6y)==0)
        
        xlogpool=c(x31,x32,x34,x35,x36)
        ylogpool=c(y31,y32,y34,y35,y36)
        logpool=xor(xlogpool,ylogpool)
        
        xxlogpool=c(xx31,xx32,xx34,xx35,xx36)
        yylogpool=c(yy31,yy32,yy34,yy35,yy36)
        xylogpool=xor(xxlogpool,yylogpool)
        
        
        Efinal=-1*sum(logpool & xylogpool)
        
        delta= Einit-Efinal
        
        if(delta<=0) {
          
          Einit=Efinal
          Group3x=modgroup3x
          Group3y=modgroup3y
        }
        
        else if(delta>0) {
          
          rand=runif(1)
          interim=exp(-1*delta)
          
          if(interim>rand) {
            Einit=Efinal
            Group3x=modgroup3x
            Group3y=modgroup3y
            
          }
          
          else {
            Group3x=Group3x
            Group3y=Group3y
            Einit=Einit
            
          }
        }
        
      }
      print("Group3")
      print(s3)
      print(length(unique(pool)))
      print(delta)
      print(Efinal)
      print(exp(-1*delta))
      print(Group3x)
      print(Group3y)
      
    }
    
    else {Group3x=Group3x
    Group3y=Group3y
    Einit=Einit
    
    print("Group3")
    print(s3)
    print(length(unique(pool)))
    print(delta)
    print(Efinal)
    print(exp(-1*delta))
    print(Group3x)
    print(Group3y)
    
    
    
    
    }
    
    
    s4=sample(1:8,1)
    if(s4==1){
      
      modgroup4x=Group4x-2.5
      modgroup4y=Group4y
    }
    else if (s4==2) {
      modgroup4x=Group4x+2.5
      modgroup4y=Group4y
      
    }
    else if (s4==3) {
      modgroup4x=Group4x
      modgroup4y=Group4y+2.5
      
    }
    else if (s4==4) {
      modgroup4x=Group4x
      modgroup4y=Group4y-2.5
      
    }
    
    else if (s4==5) {
      
      if(Group4x[1]==Group4x[2]) {
        
        modgroup4x=Group4x
        modgroup4y=Group4y
        
        if(Group4y[1]>Group4y[2]) {
          
          modgroup4x[1]=Group4x[1]+2.5
          modgroup4y[1]=Group4y[1]-2.5
        }
        else {
          
          modgroup4x[1]=Group4x[1]-2.5
          modgroup4y[1]=Group4y[1]+2.5
          
          
        }
        
      }
      else if(Group4y[1]==Group4y[2]){
        
        modgroup4x=Group4x
        modgroup4y=Group4y
        
        if (Group4x[1]<Group4x[2]) {
          
          
          modgroup4x[1]=Group4x[1]+2.5
          modgroup4y[1]=Group4y[1]+2.5
          
        }
        
        else {
          
          modgroup4x[1]=Group4x[1]-2.5
          modgroup4y[1]=Group4y[1]-2.5
          
          
        }
        
      }
      
    }
    
    else if (s4==6) {
      
      if(Group4x[1]==Group4x[2]) {
        
        modgroup4x=Group4x
        modgroup4y=Group4y
        
        if(Group4y[1]>Group4y[2]) {
          
          
          modgroup4x[1]=Group4x[1]-2.5
          modgroup4y[1]=Group4y[1]-2.5
        }
        
        else {
          modgroup4x[1]=Group4x[1]+2.5
          modgroup4y[1]=Group4y[1]+2.5
          
        }
        
      }
      else if(Group4y[1]==Group4y[2]){
        
        modgroup4x=Group4x
        modgroup4y=Group4y
        
        if (Group4x[1]<Group4x[2]) {
          
          modgroup4x[1]=Group4x[1]+2.5
          modgroup4y[1]=Group4y[1]-2.5
        }
        
        else {
          
          modgroup4x[1]=Group4x[1]-2.5
          modgroup4y[1]=Group4y[1]+2.5
          
          
          
        }
        
      }
      
    }
    
    else if (s4==7) {
      
      if(Group4x[4]==Group4x[3]) {
        
        modgroup4x=Group4x
        modgroup4y=Group4y
        
        if(Group4y[4]<Group4y[3]) {
          modgroup4x[4]=Group4x[4]-2.5
          modgroup4y[4]=Group4y[4]+2.5
          
        }
        
        else {
          modgroup4x[4]=Group4x[4]+2.5
          modgroup4y[4]=Group4y[4]-2.5
          
        }
        
      }
      else if(Group4y[4]==Group4y[3]){
        
        modgroup4x=Group4x
        modgroup4y=Group4y
        
        if(Group4x[4]>Group4x[3]) {
          modgroup4x[4]=Group4x[4]-2.5
          modgroup4y[4]=Group4y[4]-2.5
        }
        
        else{
          modgroup4x[4]=Group4x[4]+2.5
          modgroup4y[4]=Group4y[4]+2.5
          
        }
      }
      
    }
    
    else if (s4==8) {
      
      if(Group4x[4]==Group4x[3]) {
        
        modgroup4x=Group4x
        modgroup4y=Group4y
        
        if(Group4y[4]>Group4y[3]) {
          modgroup4x[4]=Group4x[4]-2.5
          modgroup4y[4]=Group4y[4]-2.5
          
        }
        
        else {
          modgroup4x[4]=Group4x[4]+2.5
          modgroup4y[4]=Group4y[4]+2.5
          
        }
        
      }
      else if(Group4y[4]==Group4y[3]){
        
        modgroup4x=Group4x
        modgroup4y=Group4y
        
        if(Group4x[4]>Group4x[3]) {
          modgroup4x[4]=Group4x[4]-2.5
          modgroup4y[4]=Group4y[4]+2.5
        }
        
        else{
          
          modgroup4x[4]=Group4x[4]+2.5
          modgroup4y[4]=Group4y[4]-2.5
          
          
        }
        
      }
      
    }
    
    if(max(abs(modgroup4x))<=50 && max(abs(modgroup4y))<=50) {
      
      poolx<-c(modgroup4x,Group1x,Group2x,Group3x, Group5x,Group6x)
      pooly<-c(modgroup4y,Group1y,Group2y,Group3y, Group5y,Group6y)
      pool<-paste(poolx,pooly)
      
      if(length(unique(pool))!=24) {
        Group4x=Group4x
        Group4y=Group4y
        Einit=Einit
      }
      
      else if(length(unique(pool))==24) {
        
        x41=(abs(modgroup4x-Group1x)==2.5)
        x42=(abs(modgroup4x-Group2x)==2.5)
        x43=(abs(modgroup4x-Group3x)==2.5)
        x45=(abs(modgroup4x-Group5x)==2.5)
        x46=(abs(modgroup4x-Group6x)==2.5)
        y41=(abs(modgroup4y-Group1y)==2.5)
        y42=(abs(modgroup4y-Group2y)==2.5)
        y43=(abs(modgroup4y-Group3y)==2.5)
        y45=(abs(modgroup4y-Group5y)==2.5)
        y46=(abs(modgroup4y-Group6y)==2.5)
        
        xx41=(abs(modgroup4x-Group1x)==0)
        xx42=(abs(modgroup4x-Group2x)==0)
        xx43=(abs(modgroup4x-Group3x)==0)
        xx45=(abs(modgroup4x-Group5x)==0)
        xx46=(abs(modgroup4x-Group6x)==0)
        yy41=(abs(modgroup4y-Group1y)==0)
        yy42=(abs(modgroup4y-Group2y)==0)
        yy43=(abs(modgroup4y-Group3y)==0)
        yy45=(abs(modgroup4y-Group5y)==0)
        yy46=(abs(modgroup4y-Group6y)==0)
        
        xlogpool=c(x41,x42,x43,x45,x46)
        ylogpool=c(y41,y42,y43,y45,y46)
        logpool=xor(xlogpool,ylogpool)
        
        xxlogpool=c(xx41,xx42,xx43,xx45,xx46)
        yylogpool=c(yy41,yy42,yy43,yy45,yy46)
        xylogpool=xor(xxlogpool,yylogpool)
        
        
        Efinal=-1*sum(logpool & xylogpool)
        
        delta= Einit-Efinal
        
        if(delta<=0) {
          
          Einit=Efinal
          Group4x=modgroup4x
          Group4y=modgroup4y
        }
        
        else if(delta>0) {
          
          rand=runif(1)
          interim=exp(-1*delta)
          
          if(interim>rand) {
            Einit=Efinal
            Group4x=modgroup4x
            Group4y=modgroup4y
            
          }
          
          else {
            Group4x=Group4x
            Group4y=Group4y
            Einit=Einit
            
          }
        }
        
      }
      print("Group4")
      print(s4)
      print(length(unique(pool)))
      print(delta)
      print(Efinal)
      print(exp(-1*delta))
      print(Group4x)
      print(Group4y)
      
    }
    
    else {Group4x=Group4x
    Group4y=Group4y
    Einit=Einit
    
    print("Group4")
    print(s4)
    print(length(unique(pool)))
    print(delta)
    print(Efinal)
    print(exp(-1*delta))
    print(Group4x)
    print(Group4y)
    
    
    
    
    }
    
    s5=sample(1:8,1)
    if(s5==1){
      
      modgroup5x=Group5x-2.5
      modgroup5y=Group5y
    }
    else if (s5==2) {
      modgroup5x=Group5x+2.5
      modgroup5y=Group5y
      
    }
    else if (s5==3) {
      modgroup5x=Group5x
      modgroup5y=Group5y+2.5
      
    }
    else if (s5==4) {
      modgroup5x=Group5x
      modgroup5y=Group5y-2.5
      
    }
    
    else if (s5==5) {
      
      if(Group5x[1]==Group5x[2]) {
        
        modgroup5x=Group5x
        modgroup5y=Group5y
        
        if(Group5y[1]>Group5y[2]) {
          
          modgroup5x[1]=Group5x[1]+2.5
          modgroup5y[1]=Group5y[1]-2.5
        }
        else {
          
          modgroup5x[1]=Group5x[1]-2.5
          modgroup5y[1]=Group5y[1]+2.5
          
          
        }
        
      }
      else if(Group5y[1]==Group5y[2]){
        
        modgroup5x=Group5x
        modgroup5y=Group5y
        
        if (Group5x[1]<Group5x[2]) {
          
          
          modgroup5x[1]=Group5x[1]+2.5
          modgroup5y[1]=Group5y[1]+2.5
          
        }
        
        else {
          
          modgroup5x[1]=Group5x[1]-2.5
          modgroup5y[1]=Group5y[1]-2.5
          
          
        }
        
      }
      
    }
    
    else if (s5==6) {
      
      if(Group5x[1]==Group5x[2]) {
        
        modgroup5x=Group5x
        modgroup5y=Group5y
        
        if(Group5y[1]>Group5y[2]) {
          
          
          modgroup5x[1]=Group5x[1]-2.5
          modgroup5y[1]=Group5y[1]-2.5
        }
        
        else {
          modgroup5x[1]=Group5x[1]+2.5
          modgroup5y[1]=Group5y[1]+2.5
          
        }
        
      }
      else if(Group5y[1]==Group5y[2]){
        
        modgroup5x=Group5x
        modgroup5y=Group5y
        
        if (Group5x[1]<Group5x[2]) {
          
          modgroup5x[1]=Group5x[1]+2.5
          modgroup5y[1]=Group5y[1]-2.5
        }
        
        else {
          
          modgroup5x[1]=Group5x[1]-2.5
          modgroup5y[1]=Group5y[1]+2.5
          
          
          
        }
        
      }
      
    }
    
    else if (s5==7) {
      
      if(Group5x[4]==Group5x[3]) {
        
        modgroup5x=Group5x
        modgroup5y=Group5y
        
        if(Group5y[4]<Group5y[3]) {
          modgroup5x[4]=Group5x[4]-2.5
          modgroup5y[4]=Group5y[4]+2.5
          
        }
        
        else {
          modgroup5x[4]=Group5x[4]+2.5
          modgroup5y[4]=Group5y[4]-2.5
          
        }
        
      }
      else if(Group5y[4]==Group5y[3]){
        
        modgroup5x=Group5x
        modgroup5y=Group5y
        
        if(Group5x[4]>Group5x[3]) {
          modgroup5x[4]=Group5x[4]-2.5
          modgroup5y[4]=Group5y[4]-2.5
        }
        
        else{
          modgroup5x[4]=Group5x[4]+2.5
          modgroup5y[4]=Group5y[4]+2.5
          
        }
      }
      
    }
    
    else if (s5==8) {
      
      if(Group5x[4]==Group5x[3]) {
        
        modgroup5x=Group5x
        modgroup5y=Group5y
        
        if(Group5y[4]>Group5y[3]) {
          modgroup5x[4]=Group5x[4]-2.5
          modgroup5y[4]=Group5y[4]-2.5
          
        }
        
        else {
          modgroup5x[4]=Group5x[4]+2.5
          modgroup5y[4]=Group5y[4]+2.5
          
        }
        
      }
      else if(Group5y[4]==Group5y[3]){
        
        modgroup5x=Group5x
        modgroup5y=Group5y
        
        if(Group5x[4]>Group5x[3]) {
          modgroup5x[4]=Group5x[4]-2.5
          modgroup5y[4]=Group5y[4]+2.5
        }
        
        else{
          
          modgroup5x[4]=Group5x[4]+2.5
          modgroup5y[4]=Group5y[4]-2.5
          
          
        }
        
      }
      
    }
    
    if(max(abs(modgroup5x))<=50 && max(abs(modgroup5y))<=50) {
      
      poolx<-c(modgroup5x,Group1x,Group2x,Group3x, Group4x,Group6x)
      pooly<-c(modgroup5y,Group1y,Group2y,Group3y, Group4y,Group6y)
      pool<-paste(poolx,pooly)
      
      if(length(unique(pool))!=24) {
        Group5x=Group5x
        Group5y=Group5y
        Einit=Einit
      }
      
      else if(length(unique(pool))==24) {
        
        x51=(abs(modgroup5x-Group1x)==2.5)
        x52=(abs(modgroup5x-Group2x)==2.5)
        x53=(abs(modgroup5x-Group3x)==2.5)
        x54=(abs(modgroup5x-Group4x)==2.5)
        x56=(abs(modgroup5x-Group6x)==2.5)
        y51=(abs(modgroup5y-Group1y)==2.5)
        y52=(abs(modgroup5y-Group2y)==2.5)
        y53=(abs(modgroup5y-Group3y)==2.5)
        y54=(abs(modgroup5y-Group4y)==2.5)
        y56=(abs(modgroup5y-Group6y)==2.5)
        
        xx51=(abs(modgroup5x-Group1x)==0)
        xx52=(abs(modgroup5x-Group2x)==0)
        xx53=(abs(modgroup5x-Group3x)==0)
        xx54=(abs(modgroup5x-Group4x)==0)
        xx56=(abs(modgroup5x-Group6x)==0)
        yy51=(abs(modgroup5y-Group1y)==0)
        yy52=(abs(modgroup5y-Group2y)==0)
        yy53=(abs(modgroup5y-Group3y)==0)
        yy54=(abs(modgroup5y-Group4y)==0)
        yy56=(abs(modgroup5y-Group6y)==0)
        
        xlogpool=c(x51,x52,x53,x54,x56)
        ylogpool=c(y51,y52,y53,y54,y56)
        logpool=xor(xlogpool,ylogpool)
        
        xxlogpool=c(xx51,xx52,xx53,xx54,xx56)
        yylogpool=c(yy51,yy52,yy53,yy54,yy56)
        xylogpool=xor(xxlogpool,yylogpool)
        
        
        Efinal=-1*sum(logpool & xylogpool)
        
        delta= Einit-Efinal
        
        if(delta<=0) {
          
          Einit=Efinal
          Group5x=modgroup5x
          Group5y=modgroup5y
        }
        
        else if(delta>0) {
          
          rand=runif(1)
          interim=exp(-1*delta)
          
          if(interim>rand) {
            Einit=Efinal
            Group5x=modgroup5x
            Group5y=modgroup5y
            
          }
          
          else {
            Group5x=Group5x
            Group5y=Group5y
            Einit=Einit
            
          }
        }
        
      }
      print("Group5")
      print(s5)
      print(length(unique(pool)))
      print(delta)
      print(Efinal)
      print(exp(-1*delta))
      print(Group5x)
      print(Group5y)
      
    }
    
    else {Group5x=Group5x
    Group5y=Group5y
    Einit=Einit
    
    print("Group5")
    print(s5)
    print(length(unique(pool)))
    print(delta)
    print(Efinal)
    print(exp(-1*delta))
    print(Group5x)
    print(Group5y)
    
    
    
    
    }
    
    s6=sample(1:8,1)
    if(s6==1){
      
      modgroup6x=Group6x-2.5
      modgroup6y=Group6y
    }
    else if (s6==2) {
      modgroup6x=Group6x+2.5
      modgroup6y=Group6y
      
    }
    else if (s6==3) {
      modgroup6x=Group6x
      modgroup6y=Group6y+2.5
      
    }
    else if (s6==4) {
      modgroup6x=Group6x
      modgroup6y=Group6y-2.5
      
    }
    
    else if (s6==5) {
      
      if(Group6x[1]==Group6x[2]) {
        
        modgroup6x=Group6x
        modgroup6y=Group6y
        
        if(Group6y[1]>Group6y[2]) {
          
          modgroup6x[1]=Group6x[1]+2.5
          modgroup6y[1]=Group6y[1]-2.5
        }
        else {
          
          modgroup6x[1]=Group6x[1]-2.5
          modgroup6y[1]=Group6y[1]+2.5
          
          
        }
        
      }
      else if(Group6y[1]==Group6y[2]){
        
        modgroup6x=Group6x
        modgroup6y=Group6y
        
        if (Group6x[1]<Group6x[2]) {
          
          
          modgroup6x[1]=Group6x[1]+2.5
          modgroup6y[1]=Group6y[1]+2.5
          
        }
        
        else {
          
          modgroup6x[1]=Group6x[1]-2.5
          modgroup6y[1]=Group6y[1]-2.5
          
          
        }
        
      }
      
    }
    
    else if (s6==6) {
      
      if(Group6x[1]==Group6x[2]) {
        
        modgroup6x=Group6x
        modgroup6y=Group6y
        
        if(Group6y[1]>Group6y[2]) {
          
          
          modgroup6x[1]=Group6x[1]-2.5
          modgroup6y[1]=Group6y[1]-2.5
        }
        
        else {
          modgroup6x[1]=Group6x[1]+2.5
          modgroup6y[1]=Group6y[1]+2.5
          
        }
        
      }
      else if(Group6y[1]==Group6y[2]){
        
        modgroup6x=Group6x
        modgroup6y=Group6y
        
        if (Group6x[1]<Group6x[2]) {
          
          modgroup6x[1]=Group6x[1]+2.5
          modgroup6y[1]=Group6y[1]-2.5
        }
        
        else {
          
          modgroup6x[1]=Group6x[1]-2.5
          modgroup6y[1]=Group6y[1]+2.5
          
          
          
        }
        
      }
      
    }
    
    else if (s6==7) {
      
      if(Group6x[4]==Group6x[3]) {
        
        modgroup6x=Group6x
        modgroup6y=Group6y
        
        if(Group6y[4]<Group6y[3]) {
          modgroup6x[4]=Group6x[4]-2.5
          modgroup6y[4]=Group6y[4]+2.5
          
        }
        
        else {
          modgroup6x[4]=Group6x[4]+2.5
          modgroup6y[4]=Group6y[4]-2.5
          
        }
        
      }
      else if(Group6y[4]==Group6y[3]){
        
        modgroup6x=Group6x
        modgroup6y=Group6y
        
        if(Group6x[4]>Group6x[3]) {
          modgroup6x[4]=Group6x[4]-2.5
          modgroup6y[4]=Group6y[4]-2.5
        }
        
        else{
          modgroup6x[4]=Group6x[4]+2.5
          modgroup6y[4]=Group6y[4]+2.5
          
        }
      }
      
    }
    
    else if (s6==8) {
      
      if(Group6x[4]==Group6x[3]) {
        
        modgroup6x=Group6x
        modgroup6y=Group6y
        
        if(Group6y[4]>Group6y[3]) {
          modgroup6x[4]=Group6x[4]-2.5
          modgroup6y[4]=Group6y[4]-2.5
          
        }
        
        else {
          modgroup6x[4]=Group6x[4]+2.5
          modgroup6y[4]=Group6y[4]+2.5
          
        }
        
      }
      else if(Group6y[4]==Group6y[3]){
        
        modgroup6x=Group6x
        modgroup6y=Group6y
        
        if(Group6x[4]>Group6x[3]) {
          modgroup6x[4]=Group6x[4]-2.5
          modgroup6y[4]=Group6y[4]+2.5
        }
        
        else{
          
          modgroup6x[4]=Group6x[4]+2.5
          modgroup6y[4]=Group6y[4]-2.5
          
          
        }
        
      }
      
    }
    
    if(max(abs(modgroup6x))<=50 && max(abs(modgroup6y))<=50) {
      
      poolx<-c(modgroup6x,Group1x,Group2x,Group3x, Group4x,Group5x)
      pooly<-c(modgroup6y,Group1y,Group2y,Group3y, Group4y,Group5y)
      pool<-paste(poolx,pooly)
      
      if(length(unique(pool))!=24) {
        Group6x=Group6x
        Group6y=Group6y
        Einit=Einit
      }
      
      else if(length(unique(pool))==24) {
        
        x61=(abs(modgroup6x-Group1x)==2.5)
        x62=(abs(modgroup6x-Group2x)==2.5)
        x63=(abs(modgroup6x-Group3x)==2.5)
        x64=(abs(modgroup6x-Group4x)==2.5)
        x65=(abs(modgroup6x-Group5x)==2.5)
        y61=(abs(modgroup6y-Group1y)==2.5)
        y62=(abs(modgroup6y-Group2y)==2.5)
        y63=(abs(modgroup6y-Group3y)==2.5)
        y64=(abs(modgroup6y-Group4y)==2.5)
        y65=(abs(modgroup6y-Group5y)==2.5)
        
        xx61=(abs(modgroup6x-Group1x)==0)
        xx62=(abs(modgroup6x-Group2x)==0)
        xx63=(abs(modgroup6x-Group3x)==0)
        xx64=(abs(modgroup6x-Group4x)==0)
        xx65=(abs(modgroup6x-Group5x)==0)
        yy61=(abs(modgroup6y-Group1y)==0)
        yy62=(abs(modgroup6y-Group2y)==0)
        yy63=(abs(modgroup6y-Group3y)==0)
        yy64=(abs(modgroup6y-Group4y)==0)
        yy65=(abs(modgroup6y-Group5y)==0)
        
        xlogpool=c(x61,x62,x63,x64,x65)
        ylogpool=c(y61,y62,y63,y64,y65)
        logpool=xor(xlogpool,ylogpool)
        
        xxlogpool=c(xx61,xx62,xx63,xx64,xx65)
        yylogpool=c(yy61,yy62,yy63,yy64,yy65)
        xylogpool=xor(xxlogpool,yylogpool)
        
        
        Efinal=-1*sum(logpool & xylogpool)
        
        delta= Einit-Efinal
        
        if(delta<=0) {
          
          Einit=Efinal
          Group6x=modgroup6x
          Group6y=modgroup6y
        }
        
        else if(delta>0) {
          
          rand=runif(1)
          interim=exp(-1*delta)
          
          if(interim>rand) {
            Einit=Efinal
            Group6x=modgroup6x
            Group6y=modgroup6y
            
          }
          
          else {
            Group6x=Group6x
            Group6y=Group6y
            Einit=Einit
            
          }
        }
        
      }
      print("Group6")
      print(s6)
      print(length(unique(pool)))
      print(delta)
      print(Efinal)
      print(exp(-1*delta))
      print(Group6x)
      print(Group6y)
      
    }
    
    else {Group6x=Group6x
    Group6y=Group6y
    Einit=Einit
    
    print("Group6")
    print(s6)
    print(length(unique(pool)))
    print(delta)
    print(Efinal)
    print(exp(-1*delta))
    print(Group6x)
    print(Group6y)
    
    
    
    
    }
    
    
    
    
    
    
    
    
    
    
    
    
  
  
    plot(1,1,asp=1,xlim=c(-50,50),ylim=c(-50,50),type="n")
    abline(h=seq(-50,50,2.5),lty=2)
    abline(v=seq(-50,50,2.5),lty=2)
    
  points(Group1x,Group1y,pch=20,col=c("brown"),type="l",lwd=2)
  points(Group2x,Group2y,pch=20,col=c("black"),type="l",lwd=2)
  points(Group3x,Group3y,pch=20,col=c("red"),type="l",lwd=2)
  points(Group4x,Group4y,pch=20,col=c("cyan"),type="l",lwd=2)
  points(Group5x,Group5y,pch=20,col=c("violet"),type="l",lwd=2)
  points(Group6x,Group6y,pch=20,col=c("green"),type="l",lwd=2)
  points(Group1x,Group1y,pch=20,col=c("Blue","Green","orange","red"))
  points(Group2x,Group2y,pch=20,col=c("Blue","Green","orange","red"))
  points(Group3x,Group3y,pch=20,col=c("Blue","Green","orange","red"))
  points(Group4x,Group4y,pch=20,col=c("Blue","Green","orange","red"))
  points(Group5x,Group5y,pch=20,col=c("Blue","Green","orange","red"))
  points(Group6x,Group6y,pch=20,col=c("Blue","Green","orange","red"))
  Sys.sleep(0.5)
  }
}