
###############################################################################
### Table1 function 
###############################################################################

myTable1=function(dat,contvar=NULL,catvar=NULL,splitvar,mydec=1,pdec=3,
                docaption=F,my.docaption=NULL,
                tsdec=2,rowPCT=F,prmsd=NULL,
                my.loc="./tab1.tex", mysize="scriptsize",
                myinsert.bottom=NULL, Trace=F, splitlabel=NULL, showtable=F,
                mysim.p=F, myB=200, mylongtable=F, mylandscape=F,
                myeps=0.001,bracket=F,mylevels=NULL,contTest=NULL,
                catTest=NULL, Test=T, chi.correct=F,
                exceloutput=F, exceloutputName=NULL,
                latexoutput=F, ... ){   
  #This function computes summary of variables as need for Table 1 with test and P 
  #for continuous and categorical values:updated Dec 24, 2016
  #Takes a longer time to work for large data set    
  
  #if(is.null(catTest)){catTest=rep("chisq.t",length(catvar))} 
  #if(is.null(contTest)){contTest=rep("aov.t",length(contvar))}
  if(is.null(prmsd)){prmsd=c(rep("mean",length(contvar)))}
  
  mycaption="Summary of patients' variables across "  

  if(is.null(contTest)&!is.null(contvar))contTest=rep("aov.t",length(contvar))
  if(is.null(catTest)&!is.null(catvar))catTest=rep("chisq.t",length(contvar))
  
  
  tagnam=data.frame(aov.t="ANOVA test",fisher.t="Fisher exact test",
                    chisq.t="Chi-squared test",t.test="T-test",
                    kruskal.t="Kruskal-Wallis test"
                    ,wilcox.t="Wilcoxon ranked sum test")
  
  mysel=unique(c(contTest, catTest))
  tagnam1=tagnam%>%select(one_of(mysel))
  ttgg=as.matrix(tagnam1)
  bottom=NULL
  n.ttgg=ncol(ttgg)
  bb.prmsd=NULL
  
  prmsd.med="{\\scriptsize $a$\\ }{\\bf $b$\\ }{\\scriptsize $c$\\ } represent the lower quartile $a$, the median $b$, and 
the upper quartile $c$\\ for continuous variables."
  prmsd.mean="~~$x\\pm s$ represents $\\bar{X}\\pm 1$ SD."
  if(bracket==T)prmsd.mean="~~$x(s)$ represents $\\bar{X}(1SD)$."
  prmsd.cat=" Numbers after percents are counts." 
 test.used="\\indent Tests used:"
 if("median"%in%prmsd){
   bb.prmsd=paste(bb.prmsd,prmsd.med,sep="")
 }
 if("mean"%in%prmsd){
   bb.prmsd=paste(bb.prmsd,prmsd.mean,sep="")
 }
 
 if(!is.null(catvar)){
   bb.prmsd=paste(bb.prmsd,prmsd.cat,sep="")
 }
 
 
  
  for( i in 1:n.ttgg){
    if(i==n.ttgg){point="."}else{point=","}
    bottom=c(paste(bottom," \\textsuperscript{\\normalfont ", i,"} ",
                   ttgg[i],point , " ",sep="") )
  }

 if(Test==T){
 bottom=paste("\\noindent ",bb.prmsd," ", test.used ,bottom,sep="") }else{
 bottom=paste("\\noindent ",bb.prmsd,sep="")
 }
 #; \\textsuperscript{\\normalfont 2} Pearson test; 
        
#\\textsuperscript{\\normalfont 3}Proportional odds likelihood ratio test"
  
  
  #Make data compatible with dplyr data frames to use colwise
  #dat=dat%>%tbl_df
  
  
  #my mean function
  catnamhold=NULL
  mathold=NULL
  cont.row.names=NULL
  rownull=NULL
 
  
  #Function for mean
  mymean=function(x,mydec=mydec,...){
    m=round(mean(x,na.rm=T),digits=mydec)
    s=round(sd(x,na.rm=T),digits=mydec)
    n=sum(!is.na(x))
    rr=paste(m,"(",s,")",sep="")
    pm=paste(m,"$\\pm$",s,sep="")
    if(bracket==F)rr=pm
    
    return(rr)
  }
  # Function for mean ends 
  
  
  #Function for median   
  mymedian=function(x,mydec=mydec,...){
    m=round(quantile(x,na.rm=T),digits=mydec)
    lq=m[[2]]
    mq=m[[3]]
    uq=m[[4]]
    n=sum(!is.na(x))
    rr=paste("{\\scriptsize",lq,"~}", "{\\bf",mq,"}~","{\\scriptsize",uq,"}",sep="")
    return(rr)
  }
  #Make all catvars factors
  for(k in catvar){
    tmp.lbl=label(dat[[k]])
    dat[[k]]<-factor(dat[[k]]) # check how NA's under factors behave
    label(dat[[k]])<-tmp.lbl
  }
  

  if(is.null(mylevels)){
    dat[[splitvar]]=factor(dat[[splitvar]]) # changing split var to factors
  }else{
    dat[[splitvar]]=factor(dat[[splitvar]],levels=mylevels)                                                       
  }
  #Test to exclude contvars if there are only categorical variables 
  if(is.null(contvar)){ tabmean1<-NULL      }else{
    for( jt in contvar){
      myrow=tapply(dat[[jt]], dat[[splitvar]],function(x){mymean(x,mydec=mydec)})
      myrowmedian=tapply(dat[[jt]], dat[[splitvar]],function(x){mymedian(x,mydec=mydec)})
      if(Trace==T)cat("Variable ",jt," is done","\n")
      pos.n=which(contvar==jt)
      #Need to test for spellings of prmsd var in function and give warning later project
      if(prmsd[pos.n]=="median"){myrow<-myrowmedian}
      rownull= rbind(rownull,myrow)
      k11<-jt
      #if( !(is.na(label(dat[[jt]])))) label(dat[[jt]])->k11
      if( (label(dat[[jt]])!="")) {
        if(Trace==T) cat(label(dat[[jt]]),"\n")
        k11=label(dat[[jt]])
        if(Trace==T)cat(k11," should be same as above","\n")
      }
      k11=paste("\\bf{",k11,"}",sep="")
      #         cat(k11,"\n")
      cont.row.names=c(cont.row.names,k11)
    }
    
    
    
  if(Trace==T)cat("Now adding overall","\n") 
    mymean1=function(x)mymean(x,mydec=mydec)
    mymedian1=function(x)mymedian(x,mydec=mydec)                               
    myN=function(x)(sum(!is.na(x)))
    Combined=unname(unlist( colwise(mymean1)(dat[,contvar])   ))
    Combmed=unname(unlist( colwise(mymedian1)(dat[,contvar])   ))
    mean.n=sum(prmsd%in%"mean")
    median.n=sum(prmsd%in%"median") 
    #if((mean.n==0 & median.n !=0)){Combined=Combmed
    #}else if((median.n==0 & mean.n !=0)){Combined=Combined
    #}else{                                 
      
    #  Combined=c(Combined[1:mean.n],Combmed[-c(1:mean.n)])
      
    #} 
    
    
    for(i in 1:length(prmsd)){
      if(prmsd[i]=="mean"){
        Combined=Combined
      }else{ 
        Combined[i]=Combmed[i]
      }
      }
    #Need to test for spellings of prmsd var in function and give warning later project
    if(Trace==T)cat("Step 1 is done","\n")#steps 1/10 
    
    N=unname(unlist(colwise(myN)(dat[,contvar])))
    tabmean= cbind(N,rownull,Combined)
    
    rownames(tabmean)<-NULL
    tabmean1=cbind(cont.row.names,tabmean)   
    
    if(Trace==T)cat("Step 2 is done","\n")#steps 2/10 
  }#end of test for if(is.null(contvar))
  
  # Categorical summaries start here
  if(is.null(mylevels)){
    if(Trace==T)cat("Step 3 is done","\n")#steps 3/10     
    dat[[splitvar]]=factor(dat[[splitvar]])
    if(Trace==T)cat("Step 4 is done","\n")#steps 4/10 
  }else{
    dat[[splitvar]]=factor(dat[[splitvar]],levels=mylevels)
    
    if(Trace==T)cat("Step 5 is done","\n")#steps 5/10 
    
  }   # This is redundant think of taking out
  
  
  #Take categorical summaries out if there are only cont vars invlove in table by setting tabcat to NULL
  if(is.null(catvar)){mathold<-NULL}else{
    
    for(k in catvar){
      
      catab=t(table(dat[[splitvar]],dat[[k]]))
      if(Trace==T)cat("Step 6 is done","\n")#steps 6/10 
      coltot=apply(catab, 2,sum)
      rowtot=apply(catab,1,sum)
      
      allN=sum(rowtot)
      combtotalp=round((rowtot/allN)*100,0)
      n.v=nlevels(dat[[k]])
      if(Trace==T)cat("Step 7 is done","\n")#steps 7/10 
      if(rowPCT==T)combtotalp=c(rep(100,n.v))    
      
      #if(rowPCT==T)combtotalp=c(100,100)    
      colpct=round(sweep(catab,2,coltot,"/")*100,0) # column percentages with sweep matric and vector division BCareful
      rowpct=round(sweep(catab,1,rowtot,"/")*100,0) #row pct with sweep
      #colpct=round((catab/coltot)*100,2)
      
      #paste col totals and pct
      
      n.col=nlevels(dat[[splitvar]])
      
      n.row=nlevels(dat[[k]])
      if(Trace==T)cat("Step 8:levels shd be greater than 2 nlevel=",n.row,"\n")#steps 8/10 
      r.names=rownames(catab)
      mat1=matrix(rep("",(n.row+1)*(n.col+1) ),nrow=n.row+1)
      
      #changing row totals to columns to cbind to catab
      r.tot=as.vector(rowtot)
      rp.tot=as.vector(combtotalp)
      
      
      catab1=cbind(catab,r.tot)
      if(rowPCT==T) colpct=rowpct
      colpct1=cbind(colpct,rp.tot)
      #how do we get row percentages
      
      for(i in 2:(n.row+1)){
        for(j in 1:(n.col+1)){ 
          mat1[i,j]<-paste(colpct1[i-1,j],"\\%~","(",catab1[i-1,j],")",sep="")   
        }}
      
      if(Trace==T)cat("Step 9 is done...","\n")#steps 9/10 
      rnm=paste("~~~~",r.names,sep="")
      k1<-k
      

      
if( Hmisc::label(dat[[k]])!="" ){  #This how labels names are swap with actual rownames
        if(Trace==T)cat("Label of ",k," is =",label(dat[[k]]),"\n")
        k1=label(dat[[k]])
        if(Trace==T)cat( k1," should be same as previous","\n")
      }
      
      k1=paste("\\bf{",k1,"}",sep="")
      r.names1=c(k1,rnm)       # You may bold this cat names in future
      
      catnamhold=c(catnamhold, r.names1)
      N2=rep("",n.row)
      N3=sum(!is.na(dat[[k]]))
      N4=c(N3,N2)
      mat2=cbind(r.names1,N4,mat1)
      mathold=rbind(mathold,mat2)
    }
    
    colnames(mathold)<-NULL
  } #end of test for excluding catvar summaries 
  
  alltabb= rbind(tabmean1,mathold)
  
  #computing test statistics
  #pt.test, 1) t.test, sign.rank, 2) rank.sum, 3) kruskal.wallis, 4) anova, 5) chisq.test, 6) chisq4trend
  ## myesp=.001    
  ## contvar=c("age.at.start","last.score","L.F.score","time.L.F"  )
  ## splitvar="Fitness.Group"
  ## mydec=1
  
  TShold.anova<-NULL # aov.t
  TShold.kruskal<-NULL # kruskal.t
  TShold.ranksum<-NULL #wilcox.t
  TShold.ttest<-NULL# t.test
  TShold.chisq<-NULL # chisq.t
  TShold.fisher<-NULL #fisher.t
  
  #Test to set TScont to NULL if there are no contvars
  
  if(is.null(contvar)){TScont<-NULL}else{    
    
    lll=nlevels(dat[[splitvar]])
    if(lll < 3) {
      
      #Do the t.test
      if( any(contTest%in%"t.test")){    #Test to stop running t test if not needed                                       
        for(t.nn in contvar){
          
          vartest=with(dat,var.test(as.formula(paste(t.nn,splitvar,sep="~")))) # Variance test to check which variance to use pooled or sattarwaite 
          var.p=vartest[["p.value"]]
          myvar.equal=T
          if(var.p < .05)myvar.equal<-F
          
          tmd=with(dat,t.test(as.formula(paste(t.nn,splitvar,sep="~")),var.equal=myvar.equal))
          
          ts.t=round(tmd[["statistic"]],digits=tsdec)[[1]]
          p.t= format.pval(round(tmd[['p.value']],digits=pdec) ,eps=myeps)  
          df.t=round(tmd[["parameter"]])[[1]]
          if(round(tmd[["p.value"]],digits=pdec) < myeps){P="~P"}else{P="~P="}
          grep("t.test",mysel)->t.jn
          if(!(any(t.jn)))t.jn<-NULL 
          espo=paste("^{",t.jn,"}",sep="")
          t.ts=paste("$t(",df.t,")=",ts.t,",",P,p.t,espo, "$",sep="")
          TShold.ttest=c(TShold.ttest,t.ts)
        }}
      
      
      #Do Wilcox.test    
      if( any(contTest%in%"wilcox.t")){    #Test to stop running wilcox test if not needed                                       
        for(w.nn in contvar){
          wmd=with(dat,wilcox.test(as.formula(paste(w.nn,splitvar,sep="~"))))
          
          ts.w=round(wmd[["statistic"]], digits=tsdec)[[1]]
          p.w= format.pval(round(wmd[['p.value']],digits=pdec),eps=myeps)  
          # df.w=round(wmd[["parameter"]])[[1]]
          if(round(wmd[["p.value"]],digits=pdec) < myeps){P="~P"}else{P="~P="}
          
          grep("wilcox.t",mysel)->w.jn
          if(!(any(w.jn)))w.jn<-NULL 
          espo=paste("^{",w.jn,"}",sep="")
          w.ts=paste("$W=",ts.w,",",P,p.w,espo,"$",sep="")
          TShold.ranksum=c(TShold.ranksum, w.ts)
        }}
      
    } #end of two level split
    
    #Do kruskal wallis
    if( any(contTest%in%"kruskal.t")){    #Test to stop running kruskal test if not needed                                                                         
      for(k.nn in contvar){
        kmd=with(dat,kruskal.test(as.formula(paste(k.nn,splitvar,sep="~"))))
        ts.k=round(kmd[["statistic"]],digits=tsdec)
        p.k= format.pval(round(kmd[['p.value']], digits=pdec),eps=myeps)  
        df.k=round(kmd[["parameter"]])[[1]]
        
        if(round(kmd[["p.value"]],digits=pdec) < myeps){P=",~P"}else{P=",~P="}
        grep("kruskal.t",mysel)->k.jn
        if(!(any(k.jn)))k.jn<-NULL 
        espo=paste("^{",k.jn,"}",sep="")
        k.ts=paste("$\\chi^2_{",df.k,"}=",ts.k,P,p.k,espo,"$",sep="")
        TShold.kruskal=c(TShold.kruskal,k.ts)
      }}
    
    #And Anova  
    
    if( any(contTest%in%"aov.t")){    #Test to stop running anova test if not needed                                                                           
      for(nn in contvar){
        #md=summary(with(dat,aov(age.at.start~Fitness.Group)))
        md=summary(with(dat,aov(as.formula(paste(nn,splitvar,sep="~")))))    
        md=unlist(md)
        df1=round(md["Df1"],0)
        df2=round(md["Df2"],0)
        P.a=format.pval(round(md["Pr(>F)1"],digits=pdec),eps=myeps)
        if(round(md["Pr(>F)1"],pdec)< myeps){P=",~P"}else{P=",~P="}
        ts=round(md["F value1"],digits=tsdec)[[1]]
        grep("aov.t",mysel)->a.jn
        if(!(any(a.jn)))a.jn<-NULL 
        espo=paste("^{",a.jn,"}",sep="")
        TS=paste("$F_{",df1,", ",df2,"}=",ts,P,P.a,espo,"$", sep="" )
        TShold.anova=c(TShold.anova,TS)
        
      }}
    
    
    TScont<-NULL
    #contTest=c("aov.t","aov.t","aov.t","kruskal.t")
    for(ii in 1:length(contvar)){
      {
        if(contTest[ii]=="t.test"){tscont=TShold.ttest[ii]
        }else{
          if(contTest[ii]=="aov.t"){tscont=TShold.anova[ii]                           
          }else{
            if(contTest[ii]=="wilcox.t"){tscont=TShold.ranksum[ii]
            }else{
              if(contTest[ii]=="kruskal.t"){tscont=TShold.kruskal[ii]
              }
            }}}}
      if(Trace==T)cat(paste("Done for ",contvar[ii], "\n",sep=" "))
      TScont=c(TScont,tscont)
    }
    
    
  } #end of test for TScont equal NULL if there are no cont var 
  
  ######################## Need to dom for categorical variables########################
  #catvar=c("sex","group.percent.days")
  #fisher test
  
  if(is.null(catvar)){TScat<-NULL}else{
    
    if( any(catTest%in%"fisher.t")){    #Test to stop running fihser test if not needed
      for(f.nn in catvar){
        fmd=fisher.test(table(dat[[f.nn]],dat[[splitvar]]),simulate.p.value=mysim.p,B=myB)
        options(digits=7)
        p.f= format.pval(round(fmd[['p.value']],digits=pdec),eps=myeps)  
        
        if(round(fmd[["p.value"]],digits=pdec) < myeps){P="~~~~~~P"}else{P="~~~~~~P="}
        grep("fisher.t",mysel)->f.jn
        if(!(any(f.jn)))f.jn<-NULL 
        espo=paste("^{",f.jn,"}",sep="")
        f.ts=paste("$",P,p.f,espo,"$",sep="")
        TShold.fisher=c(TShold.fisher,f.ts)
        
      }
    }
    
    
    ###########chi-square
    if( any(catTest%in%"chisq.t")){    #Test to stop running chisquare test if not needed                                       
      for(chi.nn in catvar){
        options(digits=7)
        chimd=chisq.test(table(dat[[chi.nn]], dat[[splitvar]]  ),correct=chi.correct)
        
        tryCatch( chisq.test(table(dat[[chi.nn]], dat[[splitvar]]  ), correct=chi.correct),error=function(e)e,warning=function(w)w)->error_warn
        if(is(error_warn,"warning")){
          if(Trace==T)cat("Warning: Use fihser exact test for \"", chi.nn, "\" variable","\n")
        }
        ts.chi=round(chimd[["statistic"]],digits=tsdec)[[1]]
        p.chi=format.pval(round(chimd[['p.value']],digits=pdec),eps=myeps)  
        df.chi=round(chimd[["parameter"]])[[1]]
        
        if(round(chimd[["p.value"]],digits=pdec) < myeps){P=",~P"}else{P=",~P="}
        grep("chisq.t",mysel)->c.jn
        if(!(any(c.jn)))c.jn<-NULL 
        espo=paste("^{",c.jn,"}",sep="")
        chi.ts=paste("$\\chi^2_{",df.chi,"}=",ts.chi,P,p.chi,espo,"$",sep="")
        TShold.chisq=c(TShold.chisq,chi.ts)
      }
    }
    
    ### looping throu to get TS for catvars
    
    TScat<-NULL
    #catTest=c("chisq.t","fisher.t")
    for(iii in 1:length(catvar)){
      { 
        if(catTest[iii]=="chisq.t"){tscat=TShold.chisq[iii]
        }else{
          if(catTest[iii]=="fisher.t"){tscat=TShold.fisher[iii] }                          
        }
        if(Trace==T)cat(paste("Done for ",catvar[iii], "\n",sep=" "))
        TScat=c(TScat,tscat)
        nlevels(dat[[catvar[iii]]])->catn
        TScat=c(TScat,rep("",catn))
        
      }      
    }
    
  }
  #end of setting TScat to NULL if there are no catvars
  
  #combine TStats for cont and cat vars
  
  Test.Statistics=c(TScont,TScat)
  alltabbb=data.frame(alltabb,Test.Statistics)
  
  ch=colheads=table(dat[[splitvar]])
  hnam=names(colheads)
  colheads=c("Variables","N", hnam,"Combined","Test Statistic")
  extracolheads=c("","",paste("N=",as.vector(ch),sep=""),paste("N=",sum(as.vector(ch)),sep=""))
  insert.bottoml="\\scriptsize{   Data is presented as : 
Mean$\\pm$SD for continuous variables, row percentages (frequency) for categorical variables~~\\indent test Test used: \\textsuperscript{\\normalfont 1} T-test ~~~~~, \\textsuperscript{\\normalfont 2} Pearson chi-square test } "
  
  myres=list(alltabb,alltabbb,colheads,extracolheads,insert.bottoml)
  names(myres)<-list("mytab","mytab1","colheads","extracolheads","insert.bottom")
  
  
  #writing to an excel file
  #cleann=function(x)str_remove_all( x,"[\\bf\\{~\\\\}$]")
  #cleann=function(x)str_replace_all( x,"[\\bf\\{~\\\\}$]"," ")
  cleann=function(x){
    y=gsub("\\}","",gsub("\\{\\\\bf","",gsub("\\\\bf\\{","",x)))
    str_replace_all( y,"[\\{~\\\\}$]"," ")
  }
  
  library(stringr)
  xcelldat=apply(myres[["mytab1"]],2,FUN=cleann)
  nn.col=ncol(xcelldat)
  colnames(xcelldat)[1]<-"Variables"
  
  extraH=t(as.matrix( c(extracolheads,"") ))
  
  hhhh=t(as.matrix(colnames(xcelldat)))
   
  xlabel=splitvar
  if(!is.null(splitlabel))xlabel=splitlabel
  ssplit=t(as.matrix(c(" ","",xlabel,rep("",(nn.col-3)))))
  xcelldat=rbind(ssplit,hhhh,extraH,xcelldat)
  xcelldat=apply(xcelldat,2,FUN=function(x)gsub(mysize," ",x))
  
  if(Test==F){xcelldat=xcelldat[,-nn.col]}
  if(exceloutput==T){ 
    if(is.null(exceloutputName))exceloutputName=splitvar
    colnames(xcelldat)<-NULL
    write.csv(xcelldat,file=paste("./",exceloutputName,".csv",sep=""),row.names=F)
    }
  
  
  #Making LaTeX Tables  
  mytab=if(Test==T)myres[["mytab1"]] else myres[["mytab"]] # i change here today
  Variable=as.vector(mytab[,1])
  tab=mytab[,-1]
  l.splitvar=splitvar
  if(  !(is.null(splitlabel)))l.splitvar<-splitlabel
  n.sp=nlevels(dat[[splitvar]])
  if( !is.null(splitlabel)) splitvar<-splitlabel

  if(latexoutput==T){
  tab1=Hmisc::latex(tab,rowname=Variable,rowlabel="Variables",
             caption=(if(docaption==T)paste(mycaption,splitvar,sep="")else my.docaption),
             file=my.loc,where="!htbp",landscape=mylandscape,longtable=mylongtable,
             
             cgroup=(if(Test==T)c("",l.splitvar,"","") else c("",l.splitvar,"")),
             n.cgroup=(if(Test==T)c(1,n.sp,1,1) else c(1,n.sp,1)),
             extracolheads=(if(Test==T)c(myres[["extracolheads"]][-1],"" ) else c(myres[["extracolheads"]][-1])),
             colheads=(if(Test==T)c(myres[["colheads"]][-1]) else c(myres[["colheads"]][c(-1,-nn.col)]) ),
             
             size=mysize,
             insert.bottom=bottom,... )
    }else{
    tab1=NULL
    showtable=F
    }
  
  if(showtable==T)return(tab1) else return(myres)    
  #   return(myres)
  
  
}


