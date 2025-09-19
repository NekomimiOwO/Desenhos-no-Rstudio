
a=function(){ 
  
  ver_mar=ifelse(ver_mar == 1,1,NA)
  ver_ceu=ifelse(ver_ceu == 1,1,NA)
  ver_praia=ifelse(ver_praia == 1,1,NA)
  for(w in 1:ww){
    { padrao=c()
    
    colfunc <- colorRampPalette(c('blue','darkblue'))
    cor <- colfunc(n*1.5)
    
    plot(NULL,ylim=c(-y,y),xlim=c(0+0.05*n2,n2-0.05*n2),las=1,ylab="",xlab="",main=paste(w,"passos de",ww,"\n","Largura = ",n2,"Altura = ",y))
    if(w==1){
      set.seed(seed)
      ini=list()
    }
    for(j in 1:n){ 
      n2
      p=0.5
      if(w==1){
        if(j==1){ 
          new=-200
        }else{ 
          new=sample((-200*y/400):(200*y/400),1,replace = F)}
      }else{
        for(zeus in 1:length(ini)){ 
          padrao[zeus]=ini[[zeus]][1]}
        
      }
      if(w>1){ 
        new=padrao[j]}
      ini[[j]]=1
      ini[[j]][1]=new
      for (i in 1:n2) {
        ini[[j]][i+1]=ini[[j]][i]+sample(c(-1,1),1,replace = T,prob = c(1-p,p))
        
        if(ini[[j]][i+1]>=(new+limitesup)){
          ini[[j]][i+1]=ini[[j]][i+1]-1
        }
        if(ini[[j]][i+1]<(new-limiteinf)){
          ini[[j]][i+1]=ini[[j]][i+1]+1
        }
      }
      #lines(ini[[j]])
    }
    
    
    ##############################################################################
    num=c()
    for (k in 1:length(ini)) {
      num[k]=(ini[[k]][1])  
    }
    ll=0
    minimo=sort(num)
    colfunc <- colorRampPalette(c('white','blue'))
    rr=colfunc(10)
    
    for (j in 1:(length(ini)-1)) {
      pos=which(num==minimo[1])[1]
      ponto0=pos
      if(length(minimo)>1){
        pos2=which(num==minimo[2])[1]
      }else{
        pos2=pos
      }
      
      
      for (i in 1:(n2/pr)) {
        if(i==1){
          polygon(c(i*pr-pr,i*pr-pr,i*pr+pr,i*pr+pr*2),c(ini[[pos]][i*pr]-20,ini[[pos2]][i*pr+pr]+20,ini[[pos2]][i*pr+pr]+20,ini[[pos]][i*pr+pr]-20),col=cor[j],border = ver_mar)
        }
        else if(i>=(n2/pr)-1){
          polygon(c(i*pr,i*pr,i*pr+pr*2,i*pr+pr*2),c(ini[[pos]][i*pr]-20,ini[[pos2]][i*pr+pr]+20,ini[[pos2]][i*pr+pr]+20,ini[[pos]][i*pr+pr]-20),col=cor[j],border = ver_mar)
        }else{
          polygon(c(i*pr,i*pr,i*pr+pr,i*pr+pr),c(ini[[pos]][i*pr]-20,ini[[pos2]][i*pr+pr]+20,ini[[pos2]][i*pr+pr]+20,ini[[pos]][i*pr+pr]-20),col=cor[j],border = ver_mar)
        }
      }
      
      if(ll==0){
        for (i in 1:(n2/pr)) {
          if(i==1){ 
            polygon(c(i*pr-pr,i*pr-pr,i*pr+pr,i*pr+pr),c(-y-100,ini[[ponto0]][i*pr]+20,ini[[ponto0]][i*pr]+20,-y-100),col='yellow2',border = ver_praia)
          }else{
            polygon(c(i*pr,i*pr,i*pr+pr,i*pr+pr),c(-y-100,ini[[ponto0]][i*pr]+20,ini[[ponto0]][i*pr]+20,-y-100),col='yellow2',border = ver_praia)
          }
        }
        ll=1
      }
      
      if(j==length(ini)-1){
        for (i in 1:(n2/pr)) {
          if(i==1){ 
            polygon(c(i*pr-pr,i*pr-pr,i*pr+pr,i*pr+pr),c(ini[[pos2]][i*pr],y+100,y+100,ini[[pos2]][i*pr]),col = "deepskyblue",border =ver_ceu)
          }
          else{
            polygon(c(i*pr,i*pr,i*pr+pr,i*pr+pr),c(ini[[pos2]][i*pr],y+100,y+100,ini[[pos2]][i*pr]),col = "deepskyblue",border =ver_ceu)
          }
        }
      }
      minimo=minimo[-1]
    }
    }
    ##########################################################-> 
    
    
    
    symbols(x=n2/2500*1070+w*15-n2/2500*1200,y=320*y/400,circles =n2/2500*100,bg='white',add=T,inches = F,fg = "white")
    symbols(x=n2/2500*1070+w*15-n2/2500*1200,y=290*y/400,circles =n2/2500*100,bg='white',add=T,inches = F,fg = "white")
    symbols(x=n2/2500*1000+w*15-n2/2500*1200,y=270*y/400,circles =n2/2500*100,bg='white',add=T,inches = F,fg = "white")
    symbols(x=n2/2500*1170+w*15-n2/2500*1200,y=270*y/400,circles =n2/2500*100,bg='white',add=T,inches = F,fg = "white")
    
    
    symbols(x=n2/2500*1070+w*5,y=320*y/400,circles =n2/2500*100,bg='white',add=T,inches = F,fg = "white")
    symbols(x=n2/2500*1070+w*5,y=290*y/400,circles =n2/2500*100,bg='white',add=T,inches = F,fg = "white")
    symbols(x=n2/2500*1000+w*5,y=270*y/400,circles =n2/2500*100,bg='white',add=T,inches = F,fg = "white")
    symbols(x=n2/2500*1170+w*5,y=270*y/400,circles =n2/2500*100,bg='white',add=T,inches = F,fg = "white")
    
    
    
    symbols(x=n2/2500*1070+n2/2500*1000+w*9,y=320+50*y/400,circles =n2/2500*90,bg='white',add=T,inches = F,fg = "white")
    symbols(x=n2/2500*1070+n2/2500*1000+w*9,y=290+50*y/400,circles =n2/2500*90,bg='white',add=T,inches = F,fg = "white")
    symbols(x=n2/2500*1000+n2/2500*1000+w*9,y=270+50*y/400,circles =n2/2500*90,bg='white',add=T,inches = F,fg = "white")
    symbols(x=n2/2500*1170+n2/2500*1000+w*9,y=270+50*y/400,circles =n2/2500*90,bg='white',add=T,inches = F,fg = "white")
    if(sol==1){ 
      # cx e cy = centro x e y
      # r = raio base 
      # a = ângulo 
      # n2 e yscale: fatores de escala x e y 
      cir=function(cx,cy,r,a){
        xx = cx + r *n2/2200* cos(a)
        yy = cy + r *y/350* sin(a)
        return(c(xx,yy))
      }
      r=200
      mudanca=w/30
      for (z in seq(1,-5,-0.2)) {
        ang=z
        c1=cir(0,y,r,ang+mudanca);c2=cir(0,y,350,ang+mudanca)
        c1p=cir(0,y,r,ang-0.1+mudanca);c2p=cir(0,y,350,ang-0.1+mudanca)
        
        polygon(x=c(c1p[1],c2p[1],c2[1],c1[1]),y=c(c1p[2],c2p[2],c2[2],c1[2]),col='yellow')
        
      }
      
      symbols(x=0,y=y,circles = n2/2500*200,bg='yellow',add = T,inches=F)
    }
    posx=n2-(10*(w-1))+30
    surf=mean((-200*y/400):(200*y/400))
    pposy=seq(surf,50,length.out=4)
    posy=rep(pposy,rev(pposy),n2)[w]
    
    
    polygon(x=c(posx-10,posx,posx+10,posx),y=c(posy-40,posy-10-40,posy-40,posy+10-40),col='white')
    symbols(posx,posy,circles = 1.3,inches = F,add=T,bg='green')
    lines(c(posx,posx),c(posy,posy-20),col='green')
    lines(c(posx,posx+rep(c(2:4,4:3),n2)[w]),c(posy-10,posy+5),col='green')
    lines(c(posx,posx-rep(c(2:4,4:3),n2)[w]),c(posy-10,posy+5),col='green')
    lines(c(posx,posx-1),c(posy-20,posy-40),col='green')
    lines(c(posx,posx+1),c(posy-20,posy-40),col='green')
    
    polygon(x=c(posx-15+50,posx+50,posx+5+50),y=c(posy-30,posy+8,posy-30),col='gray36')    
    polygon(x=c((posx+w*1.5)-15+70,(posx+w*1.5)+70,(posx+w*1.5)+5+70),y=c(posy-30+10,posy+8+10,posy-30+10),col='gray36')   
    polygon(x=c((posx+w*1.1)-15+50,(posx+w*1.1)+50,(posx+w*1.1)+5+50),y=c(posy-30-50,posy+8-50,posy-30-50),col='gray36')   
    Sys.sleep(tt)
    
  }
  ##medidas padrao
  ##altura 400
  ##largura 2500
  
  #######################################
}


{
  pr=10                #resolução , tem que ser >=1 e quanto maior, menor a resolução
  n=15                  #numero de linhas
  n2=600              #tamanho x
  tt=0.5                 #segundos de espera/intervalo
  ww=20                #quantidade de quadros, ao colocar maior que aprox 10 verá uma animaçãozinha 
  seed=1               #seed
  y=500                #altura y
  loop=1               #variavel de controle(não alterar)
  limitesup=100          #limite das areas superior(positivo)
  limiteinf=50           #limite inferior das áreas(negativo)
  sol=1             #1 com sol ; 0 sem sol
  ver_mar=0        #variaveis para ver outlines 1=sim , !=1 = não
  ver_praia=0
  ver_ceu=0
}

a()



