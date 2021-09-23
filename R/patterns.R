# patterns.R
# basic patterns

wave<-function(t,w,h,d,s,e,drift=function(t,h)0){
  if(e>t && t>s){
    tm<-t+d
    ((sin((tm*w)/pi)+1)*h/2) +drift(tm, h)
  }else{
    0
  }
}

function(){#play
  matplot(unlist(lapply(seq(1,100,by=1),function(x)wave(x,60,100,0,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,70,100,0,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,70,100,10,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,70,100,30,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,60,100,30,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,60,100,60,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,50,100,30,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,30,100,30,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,20,100,30,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,10,100,30,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,15,100,30,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,15,100,15,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,15,100,150,20,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,120,by=1),function(x)wave(x,15,100,17,20,100,drift3))),type="l")
  
}
pulser<-function(t,w,h,d,s,e,drift=function(t,h)0){
  if(e>t && t>s){
    tm<-t+d
    if((tm-s)<=(w/2)){
      h*((tm-s)/w)+drift(tm, h)
    }else if(w>(tm-s) && (tm-s)>(w/2)){
      h*(1-((tm-s)/w))+drift(tm, h)
    }else{
      0
    }
  }else{
    0
  }
}

function(){ #play
  matplot(unlist(lapply(seq(1,100,by=1),function(x)pulser(x,70,100,0,40,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,100,by=1),function(x)pulser(x,70,100,0,30,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,100,by=1),function(x)pulser(x,70,100,0,0,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,100,by=1),function(x)pulser(x,70,100,0,10,100,drift3))),type="l")
}

trigger<-function(t,w,h,d,s,e,drift=function(t,h)0){
  if(e>t && t>s){
    if((t-s)<=w){
      h*((t-s)/d)+drift(t, h)
    }else{
      0
    }
  }else{
    0
  }
}
function(){ #play
  matplot(unlist(lapply(seq(1,100,by=1),function(x)trigger(x,30,100,50,40,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,100,by=1),function(x)trigger(x,30,100,50,30,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,100,by=1),function(x)trigger(x,30,100,10,30,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,100,by=1),function(x)trigger(x,30,100,50,30,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,100,by=1),function(x)trigger(x,10,100,50,30,100,drift3))),type="l")
  matplot(unlist(lapply(seq(1,100,by=1),function(x)trigger(x,10,100,10,30,100,drift3))),type="l")
}

function(){
  dmat<-matrix(0, ncol=4, nrow=140)
  #   dmat[,1]<-unlist(lapply(seq(1,140,by=1),function(x)wave(x,50,100,0,0,70,drift2)))
  #   dmat[,2]<-unlist(lapply(seq(1,140,by=1),function(x)wave(x,50,100,+80,80,140,drift2)))
  #   dmat[,3]<-unlist(lapply(seq(1,140,by=1),function(x)pulser(x,10,100,0,40,140,drift3)))
  #   dmat[,4]<-unlist(lapply(seq(1,140,by=1),function(x)trigger(x,10,100,10,40,100,drift3)))
  
  dmat[,4]<-unlist(lapply(seq(1,140,by=1),
                          function(x){
                            trigger(x,15,100,20,10,40,drift2)+
                              trigger(x,15,100,20,30,60,drift2)+
                              trigger(x,15,100,20,50,80,drift2)+
                              trigger(x,15,100,20,70,110,drift2)+
                              trigger(x,15,100,20,100,140,drift2)
                          }
  ))
  
  dmat[,3]<-unlist(lapply(seq(1,140,by=1),
                          function(x){
                            pulser(x,10,100,0,20,40,drift3)+
                              pulser(x,10,100,0,40,60,drift3)+
                              pulser(x,10,100,0,60,80,drift3)+
                              pulser(x,10,100,0,80,110,drift3)+
                              pulser(x,10,100,0,110,140,drift3)
                          }
  ))
  #     
  matplot(dmat, type="l")
  
}
