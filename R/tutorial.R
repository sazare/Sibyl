# tutorial.R

#constant

# reset envirionment
tutorial00<-function(){ #constant
  reset.vars()
  reset.compos()
  
  def.var('con1', 0)
  constfn<-function(x){
    50
  }
  def.compo('comp1', input=c('time'),output=c('con1'), constfn)
  
  pmon<-watcher$new(c('con1'), make.date(2015,8,8,10,0,0), 60) # interval 60 secs = 1m
  
  sibyl(pmon, 10, "tutorial00.csv") 
}

tutorial01<-function(){ #single var
  reset.vars()
  reset.compos()
  
  def.var('out1', 0)
  
  fivefn<-function(t){
    5*t
  }
  def.compo('five', input=c('time'),output=c('out1'), fivefn)
  
  pmon<-watcher$new(c('out1'), make.date(2015,8,8,10,0,0), 60) # interval 60 secs = 1m
  
  sibyl(pmon, 10, "tutorial01.csv")
}

tutorial02<-function(){  #feedback loop
  reset.vars()
  reset.compos()
  def.var('out1', 0)
  def.var('tmp1', 1)
  expfn<-function(t,x){
    c(t*x, x)
  }
  def.compo('expo', input=c('time', 'tmp1'),output=c('tmp1', 'out1'), expfn)
  
  pmon<-watcher$new(c('out1'), make.date(2015,8,8,10,0,0), 60) # interval 60 secs = 1m
  
  sibyl(pmon, 20, "tutorial02.csv")
}

tutorial03<-function(){  #feeedback and threshold
  reset.vars()
  reset.compos()
  def.var('y', 0)
  def.var('x', 0)
  thresholdfn<-function(t,x){
    x<-x+sample(c(1,2,3,4,5),1)
    y<-x
    if(x>20){
      x<-x-20
      y<-20
    }
    c(x, y)
  }
  def.compo('expo', input=c('time', 'x'),output=c('x', 'y'), thresholdfn)
  
  pmon<-watcher$new(c('y'), make.date(2015,8,8,10,0,0), 60) # interval 60 secs = 1m
  
  sibyl(pmon, 50, "tutorial03.csv")
}

tutorial04<-function(){  #server and device
  reset.vars()
  reset.compos()
  
  def.var('req', 0)
  def.var('server.blog', 0)
  def.var('mem.blog', 0)
  def.var('mem.load', 0)
  def.var('mem.done', 0)
  def.var('perf', 0)
  
  requestgenfn<-function(t){
    as.integer(abs(rnorm(1)*100))
  }
  
  serverfn<-function(req, blog, memdone){
    svr.cap=90
    load<-req-memdone+blog
    if(load<=0)load<-0
    if(load>svr.cap){
      blog<-load-svr.cap
      memload<-load
    }else{
      blog<-0
      memload<-load
    }
    c(blog, memload)
  }
  
  memoryfn<-function(load, blog){
    mem.cap=80
    
    fullload<-load+blog
    if(fullload>100){
      blog<-fullload-mem.cap
      mem.done<-mem.cap
      perf<-mem.done
    }else{
      blog<-0
      mem.done<-fullload
      perf<-mem.done
    }
    blog<-fullload-mem.done
    c(blog, mem.done, perf)
  }
  def.compo('generator', input=c('time'), output=c('req'), requestgenfn)
  def.compo('server', input=c('req', 'server.blog', 'mem.done'),output=c('server.blog', 'mem.load'), serverfn)
  def.compo('mem', input=c('mem.load', 'mem.blog'),output=c('mem.blog', 'mem.done', 'perf'), memoryfn)
  
  pmon<-watcher$new(c('req', 'server.blog', 'mem.load', 'mem.done', 'mem.blog', 'perf'), make.date(2015,8,8,10,0,0), 60) # interval 60 secs = 1m

  sibyl(pmon, 100, "tutorial04.csv")
}

function(){
  tutorial00()
  tutorial01()
  tutorial02()
  tutorial03()
}

function(){
  tutorial04()
  t04<-read.table("tutorial04.csv", sep=",", header=T)
  quartz()
  # or win.graph()
  matplot(t04, type="l")
  
}