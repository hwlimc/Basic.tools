species<-c(rep('Ps',5),rep('Pa',5),rep('Be',4))
dependent.kg<-c('Bst','Bsw','Bsb','Bcrw','Bf','Bst','Bsw','Bsb','Bcrw','Bf','Bst','Bsw','Bsb','Bcrw')
independent.d<-c(13,14,16,10,7,14,14,15,13,12,8,11,14,10)
estimates<-c(11.3264,11.4219,8.8489,9.1015,7.7681,11.3341,11.4873,9.8364,8.5242,7.8171,11.0735,10.8109,10.3876,10.2806)
intercept<-c(-2.3388,-2.2184,-2.9748,-2.8604,-3.7983,-2.0571,-2.2471,-3.3912,-1.2804,-1.9602,-3.0932,-2.3327,-3.2518,-3.3633)
r.square<-c(.989,.983,.964,.949,.917,.994,.991,.983,.972,.948,.991,.985,.973,.961)

ML.d<-data.frame(species, dependent.kg, independent.d,estimates,intercept,r.square)

independent.d<-c(13,14,16,10,7,14,14,15,13,12,7,11,14,10)
estimates.d<-c(7.5939,7.6066,7.2482,13.3955,12.1095,7.4690,7.2309,8.3089,10.9708,9.7809,8.2827,8.1184,8.3019, 10.2806)
estimates.ln.h<-c(.8799,.8658,.4487,-1.1955,-1.5650,.6858,.7030,.2295,-.4923,-.4873,.5772,.9783,.7433,.0000)
estimates.h<-c(.0151,.0200,.0000,.0000,.0413,.0289,.0355,.0147,-.0124,.0000,.0393,.0000,.0000,.0000)
intercept<-c(-2.6768,-2.6864,-3.2765,-2.5413,-3.4781,-2.1702,-2.3032,-3.4020,-1.2063,-1.8551,-3.5686,-3.3045,-4.0778,-3.3633)
r.square<-c(.995,.993,.967,.960,.930,.997,.996,.984,.974,.949,.996,.995,.979,.961)

ML.dh<-data.frame(species, dependent.kg, independent.d,estimates.d,estimates.ln.h,estimates.h,intercept,r.square)

Marklund.d<-function(species,dependent.kg,d.cm){
	df<-ML.d
	exp(d.cm/(d.cm+df[df$species==species&df$dependent.kg==dependent.kg,]$independent.d)*df[df$species==species&df$dependent.kg==dependent.kg,]$estimates+df[df$species==species&df$dependent.kg==dependent.kg,]$intercept)}
	
Marklund.dh<-function(species,dependent.kg,d.cm,h.m){	# Birch Bcrw: only branch biomass
	df<-ML.dh
	exp(d.cm/(d.cm+df[df$species==species&df$dependent.kg==dependent.kg,]$independent.d)*df[df$species==species&df$dependent.kg==dependent.kg,]$estimates.d+log(h.m)*df[df$species==species&df$dependent.kg==dependent.kg,]$estimates.ln.h+h.m*df[df$species==species&df$dependent.kg==dependent.kg,]$estimates.h+df[df$species==species&df$dependent.kg==dependent.kg,]$intercept)
	}

PS.Bcr.kg<-function(species,d.m){
	if (species=='Ps') {
		exp(3.44275+11.06537*(d.m*1000/(d.m*1000+113)))/1000	#Ps
		} else {if (species=='Pa'){
				exp(4.58761+10.44035*(d.m*1000/(d.m*1000+138)))/1000	#Pa
				} else {if (species=='Be'){
							exp(6.17080+10.01111*(d.m*1000/(d.m*1000+225)))/1000}}}}	#Be


dd<-seq(5,30,length.out=30)
(Marklund.d('Ps','Bcrw', dd)-Marklund.d('Ps','Bf', dd))/Marklund.d('Ps','Bst', dd)
(Marklund.d('Pa','Bcrw', dd)-Marklund.d('Pa','Bf', dd))/Marklund.d('Pa','Bst', dd)
(Marklund.d('Be','Bcrw', dd))/Marklund.d('Be','Bst', dd)
