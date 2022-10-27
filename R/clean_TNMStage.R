clean_TNMStage=function(st=NULL,sn=NULL,sm=NULL,ss=NULL,gd=NULL,sex=NULL,age=NULL,age_cut=60){
  dat=cbind()
  if(!is.null(st)){
    st=toupper(as.character(st))
    st=substr(st,1,2)
    t1=gsub('[ABC]','',st)
    t1=gsub('T','',t1)
    t1=gsub(' ','',t1)
    t1=paste0('T',t1)

    t1[which(!t1%in%c('T1','T2','T3','T4'))]=NA
    dat=cbind(dat,Clinical_T=t1)
  }
  if(!is.null(sn)){
    sn=toupper(as.character(sn))
    n1=substr(sn,1,2)
    n1=gsub('N','',n1)
    n1=gsub(' ','',n1)
    n1=paste0('N',n1)
    n1[which(!n1%in%c('N0','N1','N2','N3','N4'))]=NA
    dat=cbind(dat,Clinical_N=n1)
  }
  if(!is.null(sm)){
    sm=toupper(as.character(sm))
    m1=substr(sm,1,2)
    m1=gsub('M','',m1)
    m1=gsub(' ','',m1)
    m1=paste0('M',m1)
    m1[which(!m1%in%c('M0','M1'))]=NA
    dat=cbind(dat,Clinical_M=m1)
  }
  if(!is.null(ss)){
    ss=toupper(as.character(ss))
    st1=gsub('STAGE','',ss)
    st1=gsub('[ABC]','',st1)
    st1=gsub(' ','',st1)
    st1[which(st1=='1'|st1==1)]='I'
    st1[which(st1=='2'|st1==2)]='II'
    st1[which(st1=='3'|st1==3)]='III'
    st1[which(st1=='4'|st1==4)]='IV'
    st1[which(st1=='5'|st1==5)]='V'
    st1=paste0('Stage ',st1)
    st1[which(!st1%in%c('Stage I','Stage II','Stage III','Stage IV','Stage V'))]=NA
    dat=cbind(dat,Clinical_Stage=st1)
  }
  if(!is.null(age)){
    age=as.numeric(as.character(age))
    if(!is.null(age_cut)){
      age=ifelse(age>age_cut,paste0('>=',age_cut),ifelse(is.na(age),NA,paste0('<',age_cut)))
    }
    dat=cbind(dat,Age=age)
  }
  if(!is.null(gd)){
    gd=toupper(as.character(gd))
    g1=substr(gd,1,2)
    g1=gsub('G','',g1)
    g1=gsub(' ','',g1)
    g1=paste0('G',g1)
    g1[which(!g1%in%c('G0','G1','G2','G3','G4'))]=NA
    if(sum(is.na(g1))==length(g1)){
      #g1 - well differentiated; g2 - moderately differentiated; g3 - poorly differentiated; g4 - undifferentiated)`
      gd=gsub(' .*','',gd)
      g1=substr(gd,1,3)
      g1=gsub('UND','G4',gsub('POO','G3',gsub('MOD','G2',gsub('WEL','G1',g1))))
      g1[which(!g1%in%c('G0','G1','G2','G3','G4'))]=NA
    }
    #print(which(g1=='Not Available'))
    dat=cbind(dat,Clinical_Grade=g1)
  }
  if(!is.null(sex)){
    sex=toupper(as.character(sex))
    sex=gsub(' ','',sex)
    sex[which(sex=='F')]='FEMALE'
    sex[which(sex=='M')]='MALE'
    sex[which(!sex%in%c('FEMALE','MALE'))]=NA
    dat=cbind(dat,Gender=sex)
  }
  return(crbind2DataFrame(dat))
}
