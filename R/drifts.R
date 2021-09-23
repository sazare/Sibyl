# drifts.R


drift<-function(){
  rnorm(1, 10, 5)
}

drift1<-function(t, h){
  rnorm(1, h/2, h/20)
}

drift2<-function(t, h){
  rnorm(1, h/30, h/40)+(h/30)
}

drift3<-function(t, h){ 
  rnorm(1, 10, 5)
}

