---
title: "Performance measurement for private equity portfolios"
author: "Karl Polen"
output: 
  html_document:
    keep_md: TRUE
---



### A demonstration of various methods

In this post I will discuss traditional private equity performance measurements of IRR, TVPI and DPI and compare them to public market equivalent ("PME") methods. I provide code in R and an example data set to illustrate these calculations.  Code files and sample data for this post are found at https://github.com/karlpolen/pme-calcs

### Background 
Private equity investments are investments in companies not listed on public exchanges. Private equity strategies are often regarded as riskier than public equity because they are illiquid, often employ higher leverage and may involve investments with companies undergoing restructuring, rapid growth, management changes or companies which are less established.  As such, investors in private equity generally expect to receive a return premium compared to investments in public markets.

Comparing private equity performance to public markets presents technical challenges.  The usual way to compare two investments is with time weighted returns.  This works with liquid investments where the investor can choose which assets to own and in what weight.  For example, you can use time weighted returns to properly compare the performance of large cap stocks with small cap stocks over whatever time frame or frames are of interest to you.

Private equity doesn't work the same way.  PE investments are typically in the form of limited partnership interests with subscription agreements pursuant to which the financial sponsor has the right to control the amount and timing of capital which is called in to the partnership.  So, the amount of capital deployed varies over time and the investor has no control over the timing of calls or distributions for the investment.  In this context, time weighted return calculations are not useful in understanding the performance of a private equity investment because the variability in the amount of capital deployed in a particular will typically be very large.  Some private equity sponsors and investors compare their IRR returns with time weighted market returns, but this practice makes no sense because of the cash flow patterns and varying amount of capital deployed.   

### Traditional private equity performance measurements
The traditional performance metrics for private equity are DPI, TVPI and IRR.  DPI stands for distributions as a percent of invested and is calculated as the sum of distributions received divided by the sum of capital contributions made.  TVPI is "total value as a percent of invested" and calculates the "total value" as the sum of distributions plus the value of the unrealized assets still owned by the partnership divided, again, by the sum of the capital calls.  IRR is the internal rate of return, customarily expressed as an annual interest rate, calculated on the cash flows with the capital calls expressed as negative numbers and including the final cash flow as part of the time series for the calculation.  The first two calculations are straightforward.  The IRR calculation presents some technical challenges in a production environment that I discussed in an earlier post http://rpubs.com/kpolen/15756.

These traditional measures collectively give a good sense of the performance of a particular PE investment and allow you to compare private equity investments to each other.  Data services tracking private equity use a combination of these metrics to rank performance in "quartiles".  They customarily group funds to control for market context in "vintages" based on the year the partnership began investing.  So, a 2006 vintage fund which invested across the global financial crisis and earned a 10% IRR might be a top quartile performer for that vintage, while the same IRR would be below average for, say, a 2009 vintage.

### Public Market Equivalent methods
Controlling for market context by vintage is useful but still crude because there is a lot of variability of patterns of calling and returning capital from fund to fund.  A number of authors proposed methods for this, but Steve Kaplan and Antoinette Schoar wrote an influential article exploring private equity performance (Kaplan and Schoar, 2005) .  In this article they formulated a method for calculating private equity performance as a "public market equivalent" which we will refer to as KSPME.  KSPME is calculated as ratio of benefits to costs and a value greater than 1 indicates performance above the opportunity cost benchmark.  The calculation is illustrated in the following formulas.

The required data are a time series of capital calls $C_t$, a time series distributions $D_t$, a final value at time $n$ of $V_n$ and a time series of dividend adjusted market indices $M_t$.  You calculate a future value factor as the ratio of the market index at time $n$  divided by the index at prior times $t$ as follows:

$$FV_t=\frac{M_n}{M_t}$$

You then multiply (pair-wise for each of the values) the future value factor times calls and distributions 

$$C_{FV}=C_t * FV_t$ ; $D_{FV} = D_t * FV_t$$  

You now have what you need to calculate

$$KSPME = \frac{\Sigma D_{FV} + V_n}{\Sigma C_{FV}}$$     

KSPME is a wealth measure reflecting how much extra money you have by investing in the private equity fund compared to an alternative public markets investment.  As an aside, the 2005 Kaplan and Schoar examines private equity performance concluding that there was not strong evidence that private equity outperformed the stock market.  In a later article, Kaplan and co-authors took a harder look at the data they were working with and found statistically significant evidence that buyout funds (the primary category of private equity) outperform public markets with a KSPME of about 1.2 (Harris, Jenkinson, and Kaplan, ).

KSPME has some nice mathematical properties that allow it to be related to the capital asset pricing model.  Gredil, Griffiths and Stucke have published an article that explores this relationship and proposes an additional metric, called "Direct Alpha", which is a measure expressed as an annual percent of outperformance of a private equity investment compared to a public market benchmark (Gredil, Griffiths, and Stucke, 2014).  It is calculated as the IRR of a time series constructed by combining the future value adjusted calls (as negative numbers), distributions and the final value.  So, it uses exactly the same data as KSPME but with different calculations to reduce the data to a single measurement.  To be consistent with the CAPM formulation, Direct Alpha is presented as a continuously compounded return.

$$Direct Alpha = \log{(1+IRR(-C_{FV},D_{FV},V_n))}$$

If you have an IRR of a private equity investment $IRR_{PE}$ calculated as a discrete annual return, you can then calculate an $IRR_M$ that you would have earned by investing in a market index with the same timing of investments and withdrawals as the calls and distributions of the private equity investment.  The below formula presumes you want $IRR_M$ to be a discrete annual amount for compatibility with $IRR_{PE}$.

$$\log{(1+IRR_M)} = \log{(1+IRR_{PE})} - DirectAlpha$$

KSPME and Direct Alpha supplement the traditional private equity performance measures with mathematically rigourous methods consistent with the CAPM framework.  KSPME provides a measure of how much additional wealth is gained with a private equity measurement.  Direct Alpha tells you the rate at which the additional wealth is accumulated.

### Illustration of calculations of private equity performance measurements in R

We work from a hypothetical data set of private equity cash flows and valuations with dates for each entry, an identifier for the fund of each entry and a variable "type" valued as "C" for cash flows and "V" for values. This data set has entries for four funds.


```r
require(tseries)
require(zoo)
pedata=read.csv('pedata.csv')
pedata$date=as.Date(pedata$date,format='%m/%d/%Y')
unique(pedata$fund)
```

```
## [1] Fund 1 Fund 2 Fund 3 Fund 4
## Levels: Fund 1 Fund 2 Fund 3 Fund 4
```

```r
head(pedata)
```

```
##     fund       date        value type
## 1 Fund 1 2008-11-14 -272.6157935    C
## 2 Fund 1 2008-12-10  -65.9728249    C
## 3 Fund 1 2008-12-19    0.1192945    C
## 4 Fund 1 2009-05-29   -0.2999770    C
## 5 Fund 1 2009-08-12   -8.5177131    C
## 6 Fund 1 2009-10-22   -7.6253831    C
```

We now want to calculate a combined cash flow and valuation for the portfolio of four funds by summing the cash flows which occur on the same day.  We add this to the data set using the name 'Total' as though it were an additional fund of that name.  Note we separate the values from the cash flows in case there is a cash flow on the same data as a valuation.

```r
pedata=split(pedata,pedata$type)
total.c=data.frame(fund="Total",aggregate(value~date,pedata$C,sum),type="C")
total.v=data.frame(fund="Total",aggregate(value~date,pedata$V,sum),type="V")
pedata=rbind(pedata$C,pedata$V,total.c,total.v)
tail(pedata)
```

```
##       fund       date       value type
## 2171 Total 2013-09-06   21.071523    C
## 2181 Total 2013-09-20   -3.840467    C
## 2191 Total 2013-09-24    1.657436    C
## 2201 Total 2013-09-26  -19.144470    C
## 2211 Total 2013-09-27   75.802883    C
## 1102 Total 2013-09-30 4015.458228    V
```
Now we download the Russell 2000 index ('\^RUT') from Yahoo finance.  We fill in missing days from the last market day in the second line of code.  Values on weekends and holidays and holidays are needed because of the accounting convetion to quote fund values on the last day of a calendar quarter.

```r
r2k=get.hist.quote('^RUT',min(pedata$date)-5,max(pedata$date)+5,
                   quote='AdjClose',provider='yahoo',retclass='zoo',quiet=TRUE)
r2k=na.locf(merge(r2k,zoo(,seq(start(r2k),end(r2k),by='day')),all=TRUE))
```
The variable `r2k` is returned from `get.hist.quote` as a `zoo` object.  We convert it to a data frame with a variable name for the date value that matches the date value variable name in the private equity data set.  We can then `merge` it with the private equity data set so that each entry in the data set has its related market index value.  Note that `merge` jumbles up the fund order -- we solve that in the next step.

```r
r2k=data.frame(date=time(r2k),index=as.numeric(r2k))
pedata=merge(pedata,r2k)
head(pedata)
```

```
##         date   fund      value type  index
## 1 2007-12-28 Fund 4 -136.82732    C 771.76
## 2 2007-12-28  Total -136.82732    C 771.76
## 3 2008-01-30  Total  -41.57570    C 695.49
## 4 2008-01-30 Fund 4  -41.57570    C 695.49
## 5 2008-02-26  Total  -10.90081    C 717.32
## 6 2008-02-26 Fund 4  -10.90081    C 717.32
```
We now `split` the data set in to a list of data frames, one for each fund and the total portfolio.

```r
pelist=split(pedata,pedata$fund)
names(pelist)
```

```
## [1] "Fund 1" "Fund 2" "Fund 3" "Fund 4" "Total"
```
I've written a function called `pe.performance` which processes data in the format of each of the data frames in `pelist`.  The code for all the functions used in this post are listed at the end of the post.  

You can calculate the performance statistics and present them in a table as follows

```r
stats=lapply(pelist,pe.performance)
stats.df=data.frame(lapply(stats,unlist))
stats.df
```

```
##                   Fund.1    Fund.2    Fund.3      Fund.4      Total
## tvpi          1.11298687 2.4007763 1.8013865  1.26647047 1.56720993
## dpi           0.18728575 0.7793920 0.9580667  0.35292838 0.55321922
## irr           0.03853564 0.6255379 0.2677777  0.07103926 0.17547727
## kspme         0.68148324 1.8831313 1.3344285  0.84649766 1.08339074
## direct.alpha -0.12964661 0.3505429 0.1197490 -0.04934003 0.02882607
## ind.irr       0.18229599 0.1448754 0.1247002  0.12520977 0.14207660
```
Here is an example of a way to plot the results.  The x axis is the market return, as if invested in Russell 2000.  The y axis is the fund return.  A line is provided with slope of 1 and intercept through the origin.  Funds above this line have outperformed their public market benchmark.  The fund names are printed in a color code indicating the value of KSPME for the fund.


```r
stats.df=data.frame(t(stats.df))
stats.df=data.frame(name=rownames(stats.df),stats.df)
require(RColorBrewer)
require(ggplot2)
pmepal=brewer.pal(n=10,name='Spectral')[c(1,3,8,9,10)]
palette(pmepal)
pmecut=cut(stats.df$kspme,c(0,.8,.95,1.05,1.2,100),labels=FALSE)
ggplot(stats.df,aes(x=ind.irr,y=irr))+
  geom_text(label=stats.df$name,colour=pmecut)+
  xlab("Market Return")+
  ylab("Fund IRR")+
  geom_abline(intercept=0,slope=1)+
  ggtitle("Private Equity Performance")+
  annotate("rect",ymin=.47,ymax=.63,xmin=.169,xmax=.181,fill='grey80')+
  annotate("text",y=.61,x=.175,label="PME > 1.2",colour=pmepal[5],size=3)+
  annotate("text",y=.58,x=.175,label="1.05 < PME < 1.2",colour=pmepal[4],size=3)+
  annotate("text",y=.55,x=.175,label=".95 < PME < 1.05",colour=pmepal[3],size=3)+
  annotate("text",y=.52,x=.175,label=".8 < PME < 1.05",colour=pmepal[2],size=3)+
  annotate("text",y=.49,x=.175,label="PME < .8",colour=pmepal[1],size=3)
```

![](pme_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### Bibliography 

```
[1] O. Gredil, B. E. Griffiths and R. Stucke. "Benchmarking
Private Equity: The Direct Alpha Method". In: _Available at SSRN_
(2014). <URL: http://ssrn.com/abstract=2403521>.
[1] R. S. Harris, T. Jenkinson and S. N. Kaplan. "Private Equity
Performance: What do we Know?" In: _The Journal of Finance_ ().
<URL:
http://papers.ssrn.com/sol3/papers.cfm?abstract_id=1932316##>.
[1] S. N. Kaplan and A. Schoar. "Private Equity Performance:
Returns, Persistance and Capital Flows". In: _The Journal of
Finance_ 60.4 (Aug. 2005).
```


### Code for functions used in this post

```r
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
```






            
