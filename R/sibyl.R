# Sibyl is Simulator Bylder(Builder)

reset.vars<-function(){
  s.vars<<-NULL
}

reset.vars()

def.var<-function(name ,value){
  new.vars<-list()
  exist<-F
  for(x in s.vars){
    #    cat(x$name, name, fill=T)
    if(x$name==name){
      new.vars<-append(new.vars, list(list(name=name, value=value)))
      exist<-T
    }else{
      new.vars<-append(new.vars, list(x))
    }
  }
  if(exist){
    s.vars<<-new.vars
  }else{
    s.vars<<-append(s.vars, list(list(name=name, value=value)))
  }
}

search.value<-function(vn, vars){
  for(x in vars){
    if(x$name==vn){
      return(x$value)
    }
  }
  NaN # means this var has not initialized
}

function(){
  vs<-s.vars
  search.value('x1', vs)
  search.value('x2', vs)
}

reset.compos<-function(){
  s.compos<<-NULL
}

reset.compos()

Component <-setRefClass(
  Class="Component", 
  fields=list(
    name="character",
    input="character",
    output="character",
    step="function"
  ),
  methods=list(
    proto.step=function(...){
      # something common here
      .self$step(...)
      # and too
    }
  )
)

def.compo<-function(name, input, output, step){
  s.compos<<-append(s.compos, 
                    list(list(Component$new(name=name, input=input, 
                                            output=output, 
                                            step=step))))
}

get.compo<-function(name){
  for(x in s.compos){
    if(x[[1]]$name==name){
      return(x[[1]])
    }
  }
  NULL
}

function(){
  dfn1<-function(x){
    x+1
  }
  dfn2<-function(x){
    x*2
  }
  
  reset.compos()
  dc1<-def.compo('c1', c('x1'), c('x2'), dfn1)
  dc2<-def.compo('c2', c('x2'), c('x1'), dfn2)
  
  oc2<-get.compo('c2')
  
  dfn3<-function(x,y){
    if(is.nan(y)){
      y<-0
    }
    x+y
  }  
  def.compo('c3', c('x1','x2'), c('x2'), dfn3)
  
}

collect.values<-function(names, vars){
  lapply(names, function(x){search.value(x, vars)})
}

step.compo<-function(time, cname, before){ # do 1 step a compo
  compo<-get.compo(cname)
  input<-compo$input
  output<-compo$output
  input.value<-collect.values(input, before)
  val<-do.call(compo$proto.step, input.value)
  for(i in 1:length(val)){
    vl<-val[i]
    nm<-output[i]
    def.var(nm, vl)
  }
}

function(){
  reset.vars()
  def.var( 'x1', 10)
  step.compo(1, 'c1', s.vars)
  str(s.vars)
  
  step.compo(2, 'c2', s.vars)
  str(s.vars)
  
  step.compo(3, 'c3', s.vars)
  str(s.vars)
  
  ####
  
  reset.vars()
  def.var( 'x1', 10)  
  step.compo(1, 'c3', s.vars)
  str(s.vars)
  
}

step.all.compos<-function(){
  before<-s.vars
  for (compo in s.compos){  
    input<-compo[[1]]$input
    output<-compo[[1]]$output
    
    input.value<-collect.values(input, before)
    
    val<-do.call(compo[[1]]$proto.step, input.value)
    
    for(i in 1:length(val)){
      if(is.nan(val[i]))next()
      vl<-val[i]
      nm<-output[i]
      def.var(nm, vl)
    }
  }
}

function(){
  reset.vars()
  def.var( 'x1', 10)
  str(s.vars)
  
  step.all.compos()
  str(s.vars)
  step.all.compos()
  str(s.vars)
  step.all.compos()
  str(s.vars)
  step.all.compos()
  str(s.vars)
  
}


sibyl<-function(wat, duration, file=""){ #simulate and monitor almost same as simnon
  #wat: watcher
  #file: output filename for generated timeseries(csv)
  
  time.generator<-function(t)t
  
  # hypo: all vars and compos are defined.
  wat$header(file)
  for(t in 1:duration){
    def.var('time', time.generator(t)) # initial vars may be many 20150505
    step.all.compos()
    wat$report(t, wat$interval, wat$init.date, file) #this is not appraciate
  }
}

# reporter is a report generator called by watcher
reporter<-function(t, interval,  init.date, file){
  vars<-collect.values(watch.vars, s.vars)
  cat(unlist(c(strize.date(date.after(init.date, t*interval)), vars)), sep=",", file=file, append=T)
  cat('\n', sep="", file=file, append=T)
}

watcher<-setRefClass(
  Class="Watcher",
  fields=list(
    watch.vars="character",
    interval="numeric",
    init.date="numeric"
  ),
  methods=list(
    initialize=function(vars, init.date, interval){
      .self$watch.vars=vars
      .self$interval=interval
      .self$init.date=init.date
    },
    header=function(file){
      cat(unlist(c('date_time', watch.vars)), sep=",", file=file)
      cat('\n', sep="", file=file, append=T)
    },
    report=reporter
  )
)

sibyldemo<-function(){
  reset.vars()
  reset.compos()
  
  cfn1<-function(x,y){
    if(is.nan(y)){
      y<-0
    }
    x+y
  } 
  cfn2<-function(x){
    x+1
  }
  cfn3<-function(x){
    x*2
  }
  
  def.compo('c1', c('time','x3'), c('x1'), cfn1)
  def.compo('c2', c('x1'), c('x2'), cfn2)
  def.compo('c3', c('x2'), c('x3'), cfn3)
#   
   my.watcher<-watcher$new(c('x2', 'x3'),make.date(2015,5,5, 10,10,0), 10)
   
  sibyl(my.watcher, 200, file="sibyldemo.csv")
  
}

