pe.performance=function(x) {
  ans=list()
  ans$tvpi=sum(x$value[x$value>0])/-sum(x$value[x$value<0])
  x.c=subset(x,x$type=='C')
  x.v=subset(x,x$type=='V')
  ans$dpi=sum(x.c$value[x.c$value>0])/-sum(x.c$value[x.c$value<0])
  x.cz=zoo(x.c$value,x.c$date)
  x.vz=zoo(x.v$value,x.v$date)
  x.all=mergesum.z(x.cz,x.vz)
  ans$irr=irr.z(x.all)
  x.ciz=zoo(x.c$index,x.c$date)
  x.viz=zoo(x.v$index,x.v$date)
  x.imat=merge(x.ciz,x.viz,fill=0)
  x.ind=zoo(apply(x.imat,1,max),time(x.imat))
  fvfactor=(as.numeric(lastinvec(x.ind)))/x.ind
  x.fv=x.all*fvfactor
  ans$kspme=(sum(x.fv[x.fv>0]))/-sum(x.fv[x.fv<0])
  ans$direct.alpha=log(1+irr.z(x.fv))
  ans$ind.irr=-1+exp(log(1+ans$irr)-ans$direct.alpha)
  return(ans)
}

mergesum.z=function(x,...) {
  mt=merge(x,...,fill=0)
  zoo(rowSums(mt),time(mt))
}
irr.z=function(cf.z,gips=FALSE) {
  irr.freq=365
  if(!is.zoo(cf.z)) {warning("cash flow must be zoo object"); return(NA)}
  if("Date"!=class(time(cf.z))) {warning("need Date class for zoo index"); return(NA)}
  if(any(is.na(cf.z))) return(NA)
  if(length(cf.z)<=1) return(NA)
  if(all(cf.z<=0)) return(NA)
  if(all(cf.z>=0)) return(NA)
  if(sum(cf.z)==0) return (0)
  if (sum(cf.z)<0) {
    rangehi=0
    rangelo=-.01
    i=0
    while(i<10000&(sign(npv.znoadjust(rangehi,cf.z))==sign(npv.znoadjust(rangelo,cf.z)))) {
      rangehi=rangelo
      rangelo=rangelo-.01
      i=i+1
    }} else {
      rangehi=.01
      rangelo=0
      i=0
      while(i<10000&(sign(npv.znoadjust(rangehi,cf.z))==sign(npv.znoadjust(rangelo,cf.z)))) {
        rangelo=rangehi
        rangehi=rangehi+.01
        i=i+1
      }}
  npv1=npv.znoadjust(rangelo,cf.z)
  npv2=npv.znoadjust(rangehi,cf.z)
  if (sign(npv1)==sign(npv2)) return(NA)
  cf.n=as.numeric(cf.z)
  if((cf.n[1]<0)&(cf.n[length(cf.n)]>0)) {
    ans=uniroot(npv.znoadjust,c(rangelo,rangehi),cf=cf.z)
    apr=ans$root } else {
      int1=rangelo
      int2=rangehi
      for (i in 1:40) {
        inta=mean(c(int1,int2))
        npva=npv.znoadjust(inta,cf.z)
        if(sign(npva)==sign(npv1)) {
          int1=inta
          npv1=npva
        } else {
          int2=inta
          npv2=npva
        }}
      apr=mean(int1,int2)  
    }
  ans=((1+(apr/irr.freq))^irr.freq)-1
  if (gips) {
    if(cf.z[1]==0)  cf.z=cf.z[-1]
    dur=lastinvec(index(cf.z))-index(cf.z)[1]
    if(dur<irr.freq) ans=(1+ans)^((as.numeric(dur))/irr.freq)-1
  }
  return (ans)
}
npv.znoadjust=function(i,cf.z) {
  freq=365
  if(!is.zoo(cf.z)) {warning("cash flow must be zoo object"); return(NA)}
  if("Date"!=class(time(cf.z))) {warning("need Date class for zoo index"); return(NA)}
  tdif=as.numeric(index(cf.z)-(index(cf.z)[1]))
  d=(1+(i/freq))^tdif
  sum(cf.z/d)
}
lastinvec=function(x) {x[length(x)]}


