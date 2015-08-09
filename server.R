#shinyapps::setAccountInfo(name='etf-portfolio', token='4F7651C5B32878ADE6ABBDEDC5603626', secret='aKRkqF+6UdFO1YxOPaqt9XlUdUIjWxBV/d2f4q7K')
#library(shiny)
#runApp('E:/Kaggle/Rshiny')
#shinyapps::deployApp('E:/Kaggle/Rshiny')

 


library(shiny)
library(datasets)
 
# Define server logic required to summarize and view the 
# selected dataset
shinyServer(function(input, output, session) {
 
  observeEvent(input$download, {
 
    progress <- shiny::Progress$new(session, min=1, max=15)
    on.exit(progress$close())
 
     progress$set(message = 'Download in progress',
                  detail = 'This may take a while...')
 
     for (i in 1:15) {
       progress$set(value = i)
       Sys.sleep(0.5)
     }
   xdataset<-loadETFs(input$ETFdates[1],input$ETFdates[2])
   	
  })   
  # Generate a summary of the dataset
  output$summary <- renderUI({

	 
	str1=paste(" Percentage Range of Gold/real estate is:", input$goldp[1])
	str1=paste(str1,"to");str1=paste(str1,input$goldp[2]);
	#print(str1)
	str2=paste("Range of Bonds is:", input$Bondsp[1])
	str2=paste(str2,"to");str2=paste(str2,input$Bondsp[2]);
	#print(str2)
	str3=paste("Within Stock, the domestic stock percentage is:", input$pdomestic)
	str3=paste(str3,", as contrast, the foreign stock percentage is:");str3=paste(str3,1-input$pdomestic);
	#print(str3)
	str4=paste("Within domestic stock, the large-cap stock percentage is:", input$largeDom)
	str4=paste(str4,", the mid-cap stock percentage is:");str4=paste(str4,1-input$largeDom-input$smallDom);
	str4=paste(str4,", the small-cap stock percentage is:");str4=paste(str4,input$smallDom);
	#print(str4)

	str5=paste("Within foreign stock, the emerge market stock percentage is:", input$emergMKT)
	str5=paste(str5,", as contrast, the devellped market percentage is:");str5=paste(str5,1-input$emergMKT);
	#print(str5)
	HTML(paste(str1,str2, str3,str4, str5, sep = '<br/>'))

 })
 
  # Show the first "n" observations
  observeEvent(input$compute, {
	output$view <- renderTable({
    		if(exists("xdataset")) head(cccdataset<<-portfoliocomp(input$Bondsp[1],input$Bondsp[2], input$goldp[1],input$goldp[2], input$pdomestic,input$largeDom,input$smallDom, input$emergMKT), n = 50)
  	})
  })
  output$rockplot<- renderPlot({
    if(exists("xdataset")) { 
	if(input$mplot=="portfolio"){  plot(cccdataset[,ncol(cccdataset)-1], cccdataset[,ncol(cccdataset)], xlab="Voltility", ylab="Daily Return")}
	else if( input$mplot=="ETFs")   plotetfs() 
	else       
     		chartSeries(xdataset[,input$mplot], theme = chartTheme("white"), type = "line", TA = NULL)
	}
  })



})

loadETFs=function(downdate1,downdate2)
{
	require(quantmod)
	# some ETFs we might choose for a portfolio:
	symbols <- c('IWD','DHS','RSP','SPY','IWF','QQQ','DVY','IWS','IWR','MDY','RFG','IWP','IJS','VBR','IWM','PRFZ','IWO','VBK','EEM','EFA','SCZ','EFG','EFV','DLS','GOLD','VNQ','AGG','BNDX')
	#symbols <- c('IWD','DHS','RSP')

	# download data using quantmod
	getSymbols(symbols,quote="Adj", from =downdate1 , to =downdate2, auto.assign = TRUE)
	basket <- cbind(IWD,DHS,RSP,SPY,IWF,QQQ,DVY,IWS,IWR,MDY,RFG,IWP,IJS,VBR,IWM,PRFZ,IWO,VBK,EEM,EFA,SCZ, EFG,EFV,DLS,GOLD,VNQ,AGG,BNDX)
      #basket <- cbind(IWD,DHS,RSP)
	numetf=ncol(basket)/6
	X.adj=basket[,c(1:numetf)*6]
	names(X.adj)<-symbols 
	basket<-X.adj
	return(basket)

}

plotetfs=function()
{
	X<-xdataset
	myColors <- c(7,8,12,134,153,259,490,"darkviolet","goldenrod")
	plot(x = X[,1], xlab = "Time", ylab = "Return",main = "",ylim=c(0, 200) , major.ticks= "years",
          minor.ticks = FALSE, col = myColors[1])
	for( i in 2:ncol(X))
	    lines(x =  X[,i], col = myColors[i%%9])

	legend(x = 'topleft', legend = names(X), lty = 1, col = myColors)
}

portfoliocomp=function(bondpx, bondpy, goldpx, goldpy, pdomestic,largeDom,smallDom,emergMKT)
{
require(quantmod)
# some ETFs we might choose for a portfolio:
#symbols <- c('IWD','DHS','RSP','SPY','IWF','QQQ','DVY','IWS','IWR','MDY','RFG','IWP','IJS','VBR','IWM','PRFZ','IWO','VBK','EEM','EFA','SCZ','EFG','EFV','DLS','GOLD','VNQ','AGG','BNDX')

# download data using quantmod
#getSymbols(symbols,quote="Adj", from = '2005-01-01', to = '2014-12-31', auto.assign = TRUE)
#basket <- cbind(IWD,DHS,RSP,SPY,IWF,QQQ,DVY,IWS,IWR,MDY,RFG,IWP,IJS,VBR,IWM,PRFZ,IWO,VBK,EEM,EFA,SCZ, EFG,EFV,DLS,GOLD,VNQ,AGG,BNDX)

basket=xdataset
# load library
#bondpx=0.15
bondpy=-1*bondpy

#goldpx=0.05
goldpy=-1*goldpy

pdomestic=1-pdomestic

#largeDom=0.7
#smallDom=0.1
#emergMKT=0.25

numetf=ncol(basket)
symbols <-names(basket)

X<-basket;

myColors <- c(7,8,12,134,153,259,490,"darkviolet","goldenrod")
#plot(x = X[,1], xlab = "Time", ylab = "Return",main = "",ylim=c(0, 200) , major.ticks= "years",
 #       minor.ticks = FALSE, col = myColors[1])
#for( i in 2:numetf)
#	lines(x =  X[,i], col = myColors[i%%9])

#legend(x = 'topleft', legend = symbols ,
 #     lty = 1, col = myColors)

#Yearly return
for (i in 1:numetf){
	if(i==1) 
		YearX = periodReturn(X[,i],period='yearly', subset='2001::', type='arithmetic',leading=TRUE)
	else{
		Xtemp<-periodReturn(X[,i],period='yearly', subset='2001::', type='arithmetic',leading=TRUE)
		YearX =cbind(YearX , Xtemp)
	}
}

#Monthly return
for (i in 1:numetf){
	if(i==1) 
		MonthX = periodReturn(X[,i],period='monthly', subset='2001::', type='arithmetic',leading=TRUE)
	else{
		Xtemp<-periodReturn(X[,i],period='monthly', subset='2001::', type='arithmetic',leading=TRUE)
		MonthX=cbind(MonthX, Xtemp)
	}
}
#Weekly return
for (i in 1:numetf){
	if(i==1) 
		WeekX = periodReturn(X[,i],period='weekly', subset='2001::', type='arithmetic',leading=TRUE)
	else{
		Xtemp<-periodReturn(X[,i],period='weekly', subset='2001::', type='arithmetic',leading=TRUE)
		WeekX =cbind(WeekX , Xtemp)
	}
}
#daily return
for (i in 1:numetf){
	if(i==1) 
		DayX = periodReturn(X[,i],period='daily', subset='2001::', type='arithmetic',leading=TRUE)
	else{
		Xtemp<- periodReturn(X[,i],period='daily', subset='2001::', type='arithmetic',leading=TRUE)
		DayX =cbind(DayX , Xtemp)
	}
}
#plot the return data
returndata=DayX ;
returndata[returndata>1]<-NA;

plot(x = returndata[,1], xlab = "Time", ylab = "Return",main = "", major.ticks= "years",
        minor.ticks = FALSE, col = myColors[5])  #ylim=c(0, 0.1) 
for( i in 2:numetf)
	lines(x =  returndata[,i], col = myColors[i%%9])
legend(x = 'topleft', legend = symbols ,
      lty = 1, col = myColors)


X<-returndata;
#compute real daily return based on CPI, however CPI is only avaiable since 10/27/2009. Therefore, we just delete the part of CPI


mgeometric=2;
if (mgeometric==1) {
	X<-log(1+X);#geometric return
	realreturns = apply(X, 2, mean, na.rm = T)# apply(X, 2, function(x) prod(1+x[!is.na(x)])^(1/length(x[!is.na(x)]))-1);
}else
	realreturns  = apply(X, 2, mean, na.rm = T);



#install.packages('lpSolve')

 require(lpSolve)
# find maximum return portfolio (rightmost point of efficient frontier)  
# will be 100% of highest return asset > # maximize 
 # w1 * stocks +w2 *bills +w3*bonds + w4 * gold  
# subject to 0 <= w <= 1 for each w 
# will pick highest return asset with w=1 
# skipping >0 constraint, no negative return assets, so not binding 

opt.objective=realreturns 
opt.constraints <- matrix(0,nrow=ncol(X)+9, ncol=ncol(X))
opt.constraints[1,]=1;

opt.constraints[2,1:18]=pdomestic;#80% domestic
opt.constraints[2,19:24]=-1*(1-pdomestic);#80% domestic

opt.constraints[3:5,]=0
opt.constraints[3,1:6]=1/largeDom;#within domestic 70% large
opt.constraints[3,7:18]=-1/(1-largeDom);#within domestic 20% mid
opt.constraints[4,1:12]=1/(1-smallDom);#within domestic 70% large
opt.constraints[4,13:18]=-1/smallDom;#within domestic 20% mid

opt.constraints[5,19]=1/emergMKT;#within international 75% developed MKT
opt.constraints[5,20:24]=-1/(1-emergMKT);#within domestic 25% emerg MKT


lineconst=6;
opt.constraints[lineconst,ncol(X)-2]=1;
opt.constraints[lineconst,ncol(X)-3]=1;#gold
opt.constraints[lineconst+1,ncol(X)-2]=-1;
opt.constraints[lineconst+1,ncol(X)-3]=-1;#gold

opt.constraints[lineconst+2,ncol(X)]=1;
opt.constraints[lineconst+2,ncol(X)-1]=1;#bond
opt.constraints[lineconst+3,ncol(X)]=-1;
opt.constraints[lineconst+3,ncol(X)-1]=-1;#bond



for(i in 1:ncol(X)+9)
{
opt.constraints[i,i-9]=1;
}

#opt.constraints <- matrix (c(, # constrain sum of weights to 1 
#					 1, rep(0,each=1,times=ncol(X)-1), # constrain w1 <= 1 
#					 1, rep(0,each=1,times=ncol(X)-1), # constrain w2 <= 1 
#					 1, rep(0,each=1,times=ncol(X)-1), # constrain w3 <= 1 
#					 1),  # constrain w4 <= 1 
#					 nrow=5, byrow=TRUE) 

opt.operator <- c("=","=","=","=","=",">=",">=",">=",">=",rep("<=",each=1,times=ncol(X)) ) 
opt.rhs <- c(1,0,0,0,0,goldpx,goldpy,bondpx,bondpy, rep(1,each=1,times=ncol(X)) )
opt.dir="max" 
solution.maxret = lp(direction = opt.dir, 
				opt.objective, 
				opt.constraints, 
				opt.operator, 
				opt.rhs)
# portfolio weights for max return portfolio 
wts.maxret=solution.maxret$solution 
# return for max return portfolio 
ret.maxret=solution.maxret$objval

# compute return covariance matrix to determine volatility of this portfolio
covmatrix = cov(X, use = 'complete.obs', method = 'pearson')
# multiply weights x covariances x weights, gives variance 
var.maxret = wts.maxret %*% covmatrix %*% wts.maxret 
# square root gives standard deviation (volatility) 
vol.maxret = sqrt(var.maxret)

#Find minimum-volatility portfolio using quadratic programming 
#install.packages('quadprog') 
require(quadprog)

# minimize variance: w %*% covmatrix %*% t(w) 
# subject to sum of ws = 1 
# subject to each ws > 0  
# solution.minvol <- solve.QP(covmatrix, zeros, t(opt.constraints), opt.rhs, meq = opt.meq)
# first 2 parameters covmatrix, zeros define function to be minimized 
# if zeros is all 0s, the function minimized ends up equal to port variance / 2 
# opt.constraints is the left hand side of the constraints, ie the cs in 
# c1 w1 + c2 w2 ... + cn wn = K 
# opt.rhs is the Ks in the above equation 
# meq means the first meq rows are 'equals' constraints, remainder are >= constraints
# if you want to do a <= constraint, multply by -1 to make it a >= constraint 
# does not appear to accept 0 RHS, so we make it a tiny number> 0 

# compute covariance matrix
covmatrix = cov(X, use = 'complete.obs', method = 'pearson') 
nObs <- nrow(X) 
nAssets <- ncol(X) 
# 1 x numassets array of 1s 
#opt.constraints <- matrix (c(1, 1, 1, 1, # sum of weights =1 
#					1, 0, 0, 0, # w1 >= 0 
#					0, 1, 0, 0, # w1 >= 0 
#					0, 0, 1, 0, # w1 >= 0 
#					0, 0, 0, 1) # w2 >= 0 
#					, nrow=5, byrow=TRUE) 

opt.rhs <- matrix(c(1,0,0,0,0,goldpx,goldpy,bondpx,bondpy,rep(0.000001,each=1,times=ncol(X)))) 
opt.meq <- 5 # first constraint is '=', rest are '!=' 

# numassets x 1 array of 0s 
zeros <- array(0, dim = c(nAssets,1)) 

solution.minvol <- solve.QP(covmatrix, zeros, t(opt.constraints), opt.rhs, meq = opt.meq) 

wts.minvol = solution.minvol$solution 
var.minvol = solution.minvol$value *2 
ret.minvol = realreturns%*% wts.minvol  
vol.minvol = sqrt(var.minvol)

#Fill in all the points on the efficient frontier

# generate a sequence of 50 evenly spaced returns between min var return and max return 
lowreturn=ret.minvol 
highreturn=ret.maxret 
minreturns=seq(lowreturn, highreturn, length.out=50)
# add a return constraint: sum of weight * return >= x 
retconst= rbind(opt.constraints, realreturns) 
retrhs=rbind(opt.rhs, ret.minvol) 

# create vectors for the returns, vols, and weights along the frontier, 
# starting with the minvol portfolio 
out.ret=c(ret.minvol) 
out.vol=c(vol.minvol) 
out.etf=matrix(0,nrow=length(minreturns), ncol=length(wts.minvol)) 
out.etf[1,]=wts.minvol

# loop and run a minimum volatility optimization for each return level from 2-49 
for(i in 2:(length(minreturns) - 1)) { 
	 
	# start with existing constraints, no return constraint 
	tmp.constraints = retconst 
	tmp.rhs=retrhs 
	# set return constraint 
	tmp.rhs[nrow(tmp.constraints)] = minreturns[i] 

	tmpsol <- solve.QP(covmatrix, (zeros), t(tmp.constraints), tmp.rhs, meq = opt.meq) 

	tmp.wts = tmpsol$solution 
	tmp.var = tmpsol$value *2 
	out.ret[i] = realreturns %*% tmp.wts 
	out.vol[i] = sqrt(tmp.var) 
#print(tmp.wts) 
	out.etf[i,]=(tmp.wts) 
} 
	
# put maxreturn portfolio in return series for max return, index =50 
out.ret[50]=c(ret.maxret) 
out.vol[50]=c(vol.maxret) 
out.etf[50,]=wts.maxret 
#layout( matrix(1:2, nrow = 2) )
plot(out.vol,out.ret)
ccc=cbind(out.etf,out.vol,out.ret)*100
titlename=c(symbols ,"STD*100","RETURN*100")
colnames(ccc)<-titlename
return(ccc)
}




