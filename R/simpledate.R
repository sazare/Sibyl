#  simpledate.R

# In my world, all months have 30 days.
# the version of uru-year is commented out and it is not work.

make.date<-function(year, month, day, hour, min, sec){
  c(year, month, day, hour, min, sec)
}

objyze.datestr<-function(str){
  dts<-strsplit(str, split="[- :/]")
  unlist(lapply(dts[[1]], as.integer))
}

strize.date<-function(data){
  sprintf("%04d-%02d-%02d %02d:%02d:%02d",
          data[1], data[2], data[3], data[4], data[5], data[6])
}

# don't concerns uruu dosi
# max.time<<-c(9999, 12, 28, 24, 60, 60)
# max.dpm<<-c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
# ix.month=2
# ix.day=3
# 
# inc.carry<-function(date){
#   for(ic in c(6,5,4)){
#     if(date[ic]>=max.time[ic]){
#       date[ic-1]<-date[ic-1]+1
#       date[ic]<-date[ic]-max.time[ic]
#     }
#   }
#   for(ic in c(3,2)){
#     if(ic==ix.day){
#       max.ic<-max.dpm[date[ix.month]]
#     }else{
#       max.ic<-max.time[ic]
#     }
#     if(date[ic]>max.ic){
#       date[ic-1]<-date[ic-1]+1
#       date[ic]<-1
#     }
#   }
#   date
# }

# inc.date<-function(date){
#   ## lowest incrase, should extend to all
#   ix <- length(max.time)
#   date[ix]<-date[ix]+1
#   inc.carry(date)
# }

inc.date<-function(date){
  add.date(date, make.date(0,0,0, 0,0,1))
}

add.secs2date<-function(date, secs){
  d<-date
  if(secs==0){
    d
  }else{
    for(i in 1:secs){
      d<-inc.date(d)
    }
    d
  }
}

nth.next.date<-function(date, interval, nth){
  add.secs2date(date, interval*nth)  
}

datePool<-setRefClass(
  Class="DatePool",
  fields=list(
    name="character",
    init.date="numeric",
    max.index="numeric",
    pool="list"),
  methods=list(
    initialize=function(name, init.date){
      .self$name<-name
      .self$init.date<-init.date
      .self$max.index<-0
      .self$pool<-list()
      .self$pool[[1]]<-init.date
    },
    getnth=function(n){# n= 0,1,2,...
      if(n<=max.index){
        .self$pool[[n+1]]
      }else{
        for(i in (max.index+1):(n+1)){
          .self$pool[[i+1]]<-inc.date(pool[[i]])
        }
        .self$max.index<-n
        .self$pool[[n+1]]
      }
    }
  )
)

set.datepool<-function(name, init.date){
#  datepool<<-datePool$new(name=name, init.date=init.date)
}

# date.after<-function(nth){
#   datepool$getnth(nth)
# }

date.after<-function(init.date, secs){
  add.date(init.date, make.date(0,0,0, 0,0,secs))
}

add.date<-function(d1, d2){
  lm<-c(10000, 12,30,24,60,60)
  dsum<-d1+d2
  for( i in c(6,5,4)){
    if(dsum[i]>=lm[i]){
      dsum[i-1]<-dsum[i-1]+dsum[i]%/%lm[i]
      dsum[i]<-dsum[i]%%lm[i]
    }
  }
  for( i in c(3,2)){
    if(dsum[i]>=lm[i]){
      cr <- dsum[i]%%lm[i]
      co <- +dsum[i]%/%lm[i]
      if(cr==0){
        cr<-lm[i]
        co<-0
      }
      dsum[i-1]<-dsum[i-1]+co
      dsum[i]<-cr
    }
  }
  
  dsum
}

