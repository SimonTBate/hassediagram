hasselayout <- function(datadesign, 
                        randomfacsid=NULL,
                        pdf="N",
                        example="example",
                        outdir=NULL,
                        produceBWPlot="N",
                        larger.fontlabelmultiplier=1,   
                        middle.fontlabelmultiplier=1,   
                        smaller.fontlabelmultiplier=1,   
                        check.shared.df="Y",
                        maxlevels.df="Y",
                        showLS="Y",
                        showpartialLS="Y", 
                        showdfLS="Y") {
  
  if (showpartialLS=="Y" || showdfLS=="Y") showLS<-"Y"

  if(is.null(outdir)) {
    data.folder.location <- getwd()
  } else {
    data.folder.location <- outdir
  }
  
  if(!is.null(randomfacsid) & ncol(datadesign) != length(randomfacsid)) stop("The length of randomfacsid should be equal to the number of variables/columns of datadesign")
  
  nfacts <- ncol(datadesign)
  if(is.null(randomfacsid)) {
    randomfacsid <- rep(0, nfacts)
  } else {
    randomfacsid <- randomfacsid
  }
  
  if(pdf=="Y") {
    edgewidth <- 1
    dottedline <- 3
    pdf(file=paste(data.folder.location, "/", example,"_output.pdf",sep=""), width=11, height=8)
  } else {
    edgewidth <- 4
    dottedline <- 3
  }
  
  Colourblue<-"blue"
  Colourred<-"red"
  Colourpurple<-"purple"
  Colourorange<-"orange"
  
  if(produceBWPlot == "Y") {	
    Colourblue<-"black"
    Colourred<-"black"
    Colourpurple<-"black"
    Colourorange<-"black"
  }
  
  datadesign <- cbind(Mean=rep(1,length(datadesign[ ,1])),datadesign[ , ])
  
  if(length(colnames(datadesign)[apply(datadesign,2,anyna)])>0) {
    warning("The following variables have missing values. Please check and complete the dataset ",colnames(datadesign)[apply(datadesign,2,anyna)])
    options(show.error.messages=F)
    if (pdf=="Y") dev.off()
    stop()
  }
  
  
  abbrev.length <- rep(2,length(colnames(datadesign)[-1]))
  names(abbrev.length) <- colnames(datadesign)[-1]
  
  if (max(table(substring(colnames(datadesign)[-1],1,2)))>1) {
    if (max(table(substring(colnames(datadesign),1,3)))>1) {
      warning("The program abbreviates factor names using the first 2 or 3 letters. It has found that two factors have the same abbreviation. Please rename to avoid this issue.")
      options(show.error.messages=F)
      if (pdf=="Y") dev.off()
      stop()
    } else {
      abbrev.length[substring(names(abbrev.length),1,2) %in% (names(table(substring(colnames(datadesign),1,2)))[table(substring(colnames(datadesign),1,2))==2])] <- 3
    }
  }
  
  main.effects.table <- structure.fun(datadesign)
  
  rm.effecti <- 0
  rm.effectj <- 0
  no.effectsi <- length(colnames(main.effects.table))
  for (i in 1:(no.effectsi-1)) {
    for (j in (i+1):no.effectsi) {
      if (main.effects.table[i,j]=="1" & main.effects.table[j,i]=="1") {
        rm.effecti <- c(rm.effecti,i)
        rm.effectj <- c(rm.effectj,j)
      }
    }
  }
  
  if (length(rm.effecti)>1) {
    rm.effecti <- rm.effecti[-1]
    rm.effectj <- rm.effectj[-1]
    for (k in (1:length(rm.effecti))) {
      warning(colnames(main.effects.table)[rm.effecti[k]]," is confounded with ",colnames(main.effects.table)[rm.effectj[k]],"\n\n")
    }
    
    if (max(table(rm.effecti)) > 1 || max(table(rm.effectj)) >1 ) {
      warning("There are three (or more) main effects which are confounded. The program only processes single or pairs of equivalent factors.
          Please remove at least one factor from any triple set of equivalent factors.")
      options(show.error.messages=F)
      
      if (pdf=="Y") dev.off()
      stop()
    } else {
      warning("There are main effects which are confounded. If the confounding is unintentional review the design and correct as appropriate.
          The program will proceed by using only one factor for each equivalent pair of factors.")     
    }      
    
    datadesign <- datadesign[ ,-(rm.effectj)]  
    randomfacsid<-randomfacsid[-(rm.effectj-1)]                                   

    confound.factor <- cbind(colnames(main.effects.table)[rm.effectj],colnames(main.effects.table)[rm.effecti]) 
    }                  
  
  levs <- apply(datadesign, 2, function (x) {length(unique(x))})
  resid.yn <- "Y"
  if (nrow(datadesign) %in% levs) resid.yn <- "N"
  if (resid.yn == "Y") {
    datadesign <- cbind(datadesign,obs.unit=(1:nrow(datadesign)))
    abbrev.length <- c(abbrev.length,obs.unit=8)
  }
  
  maxfacs <- length(colnames(datadesign))
  randomfacsidm <- c(0,randomfacsid)   
  if (resid.yn == "Y") randomfacsidm<-c(randomfacsidm, 1) 
  
  randomfacs<-colnames(datadesign)[randomfacsidm==1]
  
  keep.order.effects0 <- "Mean"
  
  main.effects.table <- structure.fun(datadesign)
  
  orig.order <- order(rownames(main.effects.table))
  xtable <- main.effects.table[orig.order,orig.order]
  
  prelim.effect.order <- c(rep(NA, nrow(xtable)))
  for (i in 1:nrow(main.effects.table)) {
    prelim.effect.order[i] <- length(xtable[i,(xtable[i, ]=="1")])
  }
  
  xtable.print <- as.data.frame(cbind(xtable,prelim.effect.order))
  
  sort.prelim <- order(prelim.effect.order)
  sort.ord <- c(1:nrow(xtable))[orig.order][sort.prelim]
  
  main.effects.table <- main.effects.table[sort.ord,sort.ord]
  main.effects.table.print <- as.data.frame(cbind(main.effects.table,prelim.effect.order=prelim.effect.order[sort.prelim]))
  prelim.effect.order <- prelim.effect.order[sort.prelim]
  
  main.effects.table.print <- data.frame(lapply(main.effects.table.print, as.character), stringsAsFactors=FALSE)
  rownames(main.effects.table.print) <- rownames(main.effects.table)
  main.effects.table.print[1, (2:(ncol(main.effects.table.print)-1))] <- "(0)"
  
  main.effects.table.nestnames <- rownames(main.effects.table)
  main.effects.table.nestintnames <- rownames(main.effects.table)
  
  for (i in 2:length(rownames(main.effects.table))) {
    first <- 1
    for (j in 2:length(rownames(main.effects.table))) {
      if (main.effects.table[i,j]==1 & i != j) {
        if (first==1) {main.effects.table.nestnames[i] <- paste(main.effects.table.nestnames[i],"(",rownames(main.effects.table)[j],sep="")} else
          main.effects.table.nestnames[i] <- paste(main.effects.table.nestnames[i],"^",rownames(main.effects.table)[j],sep="")
        first <- 0
        main.effects.table.nestintnames[i] <- paste(main.effects.table.nestintnames[i],"^",rownames(main.effects.table)[j],sep="")
      }
    }
    if (first==0) {
      main.effects.table.nestnames[i] <- paste(main.effects.table.nestnames[i],")",sep="")
    }
  }
  
  orderi <- 1
  order.effects1<-rownames(main.effects.table[prelim.effect.order==orderi, ,drop=F])
  order.effects<-c("Mean",order.effects1)    #Add in effects from lower order i.e. order 0 = Mean
  level.order.effects1<-c(0,rep(1,length(order.effects1)))    #identify which order the effect is added
  design1 <- datadesign[ ,order.effects]
  effects.table.order1 <- structure.fun(design1)
  
  orderi <- 2
  outputlist2 <- designorder(2,orderi,design1,order.effects1,level.order.effects1,"","","", main.effects.table.nestintnames, main.effects.table, prelim.effect.order, maxfacs, datadesign)
  level.order.effectsi_1 <- level.order.effects1
  outputlistip1 <- outputlist2
  order.effectsi_1 <- order.effects1
  
  for (level in 3:eval(maxfacs-1)) {
    outputlisti <- outputlistip1
    designi <-  outputlisti$designi
    level.order.effectsi <- c(level.order.effectsi_1,rep(eval(orderi),(length(colnames(designi))-length(level.order.effectsi_1))))
    orderip1 <- eval(orderi+1)
    order.effectsi <- colnames(designi)[level.order.effectsi==orderi]
    
    outputlistip1 <- designorder(level,orderip1,designi,order.effectsi,level.order.effectsi,outputlisti$keep.nested.effectsi,outputlisti$keep.nested.interactionsi,outputlisti$keep.order.effectsiuniqi, main.effects.table.nestintnames, main.effects.table, prelim.effect.order, maxfacs, datadesign)
    
    level.order.effectsi_1 <- level.order.effectsi
    order.effectsi_1 <- order.effectsi
    orderi <- orderip1
  }
  
  finaleffects <- outputlistip1$level.order.effectsi
  names(finaleffects) <- colnames(outputlistip1$designi)
  
  finaleffectrandom <- rep(NA, length(names(finaleffects)))
  for (m in 1:length(names(finaleffects))) {
    finaleffectrandom[m]<-0
    if (length(randomfacs)>0) {
      for (i in 1:length(randomfacs)) {
        finaleffectrandom[m] <- max(grep(randomfacs[i],names(finaleffects)[m]),finaleffectrandom[m])
      }
    }
  }
  names(finaleffectrandom) <- names(finaleffects)
  
  effects.table.final <- structure.fun(outputlistip1$designi)
  effects.table.final.brief <- effects.table.final
  effects.table.final.brief[1,2:ncol(effects.table.final.brief)] <- "(0)"
  ceffects.table.final.brief <- effects.table.final
  ceffects.table.final.brief[1,2:ncol(effects.table.final.brief)] <- "(0)"
  brief.colnames <- sapply(colnames(effects.table.final), brief.effect.fun, abbrev.length)
  colnames(effects.table.final.brief) <- brief.colnames
  colnames(ceffects.table.final.brief) <- brief.colnames
  
  confounded.main <- colnames(datadesign)[sort.ord[-1]][!(colnames(datadesign)[sort.ord[-1]] %in% colnames(outputlistip1$designi))]
  
  no.confounded <- length(confounded.main)
  no.all <- length(colnames(outputlistip1$designi))
  conf.nestedin.all.table <- matrix(" ",nrow=no.confounded,ncol=no.all)
  all.nestedin.conf.table <- matrix(" ",nrow=no.all,ncol=no.confounded)

  for (effectalli in 1:no.all) {
    if (no.confounded > 0) {
      for (confoundi in 1:no.confounded) {     
        freq.table <- table(datadesign[ ,sort.ord][ ,confounded.main[confoundi]],outputlistip1$designi[ ,effectalli])
        nestk <- rep(0, length=nrow(freq.table))   #Set up factor for all k levels of factorj to indicate whether nested within factor i
        for (k in 1:nrow(freq.table)) {
          if (min(freq.table[k,])==0 && table(freq.table[k, ])[1]+1==ncol(freq.table)) nestk[k] <- 1 else
            if (min(freq.table[k, ])==0) nestk[k] <- 0.5 else nestk[k] <- 0
        }
        conf.nestedin.all.table[confoundi,effectalli] <- if (all(nestk==1)) "1" else if (all(nestk==0)) "0" else "(0)"
      }
    }
  }
  
  if (no.confounded > 0) {
    for (confoundi in 1:no.confounded) {
      for (effectalli in 1:no.all) {
        freq.table <- table(outputlistip1$designi[ ,effectalli],datadesign[ ,sort.ord][ ,confounded.main[confoundi]])
        nestk <- rep(0, length=nrow(freq.table))   #Set up factor for all k levels of factorj to indicate whether nested within factor i
        for (k in 1:nrow(freq.table)) {
          if (min(freq.table[k,])==0 && table(freq.table[k, ])[1]+1==ncol(freq.table)) nestk[k] <- 1 else
            if (min(freq.table[k, ])==0) nestk[k] <- 0.5 else nestk[k] <- 0
        }
        all.nestedin.conf.table[effectalli,confoundi] <- if (all(nestk==1)) "1" else if (all(nestk==0)) "0" else "(0)"
      }
    }
  }
  
  confound.tab <- confound.tab.fun(no.confounded, no.all, all.nestedin.conf.table, conf.nestedin.all.table, confounded.main, outputlistip1)
  
  confound.tab<-cbind(confound.tab,confound.tab[ ,1])
  if (exists("confound.factor")) {
    confound.factor<-cbind(confound.factor,confound.factor[ ,1])
    if (dim(confound.tab)[1] > 0) {
      confound.factork <- NULL
      #This checks for equivalent factors also equivalent to an interaction
      if(nrow(confound.factor) > 0) { 
        for (j in 1:nrow(confound.tab)) {
          for (k in 1:nrow(confound.factor)) {
            if (confound.tab[j,3] == confound.factor[k,2]) {
              confound.tab[j,3] <- paste(confound.factor[k,3],"=", confound.tab[j,3],sep="")
              confound.factork <- c(confound.factork,k)
              confound.factork.y <- "Y"
            }
          }
        }
      }
      if (exists("confound.factork.y")) {
        confound.tab <- rbind(confound.factor[-confound.factork, ],confound.tab) }
    } else {
      confound.tab <-confound.factor
    }
  }    
  
  if (no.confounded>0) {
    contain <- rep(NA,nrow(confound.tab))
    for (i in 1:nrow(confound.tab)) {
      contain[i] <- confound.tab[i,1] %in% extract.factor.fun(confound.tab[i,2])
    }
    confound.tab <- cbind(confound.tab,contain)
  } else {
    confound.tab <- cbind(confound.tab,confound.tab[ ,3])
  }
  
  contain.false <- confound.tab[confound.tab[ ,4]=="FALSE",1]   #Selects factors equivalent to an interaction which doesn't contain them
  confound.tab <- cbind(confound.tab,inclequiv.factors=confound.tab[ ,2])     #puts in last column original names including names of equivalent factors
  
  for (i in 1: length(contain.false)) {
    if ((length(contain.false) > 0)&& (contain.false[i]!=confound.tab[i,1])) {
      removeb<-paste("\\^",contain.false[i],sep="")
      removea<-paste(contain.false[i],"\\^",sep="")
      removeb.brief<-paste("\\^",substring(contain.false[i],1,abbrev.length[contain.false[i]]),sep="")
      removea.brief<-paste(substring(contain.false[i],1,abbrev.length[contain.false[i]]),"\\^",sep="")
      confound.tab[ ,2]<-gsub(paste(removeb),"",confound.tab[ ,2])
      confound.tab[ ,2]<-gsub(paste(removea),"",confound.tab[ ,2])
      names(finaleffects)<-gsub(paste(removea),"",names(finaleffects))
      names(finaleffects)<-gsub(paste(removeb),"",names(finaleffects))
      brief.colnames<-gsub(paste(removea.brief),"",brief.colnames)
      brief.colnames<-gsub(paste(removeb.brief),"",brief.colnames)
      colnames(effects.table.final.brief)<-gsub(paste(removea.brief),"",colnames(effects.table.final.brief))
      colnames(effects.table.final.brief)<-gsub(paste(removeb.brief),"",colnames(effects.table.final.brief))
      rownames(effects.table.final.brief)<-gsub(paste(removea),"",rownames(effects.table.final.brief))
      rownames(effects.table.final.brief)<-gsub(paste(removeb),"",rownames(effects.table.final.brief))
    }
  }
  

  for (i in 1:nrow(confound.tab)) {
    if ((confound.tab[i,1] %in% randomfacs) && (finaleffectrandom[confound.tab[i,5]]==0)) {
      warning("\n", confound.tab[i,1]," was defined as random but it is equivalent to ", names(finaleffectrandom[confound.tab[i,5]]), 
          " which has all its component factors as fixed. \n", names(finaleffectrandom[confound.tab[i,5]]),
          " will be assumed to be random to align with ", confound.tab[i,1], ".",
          " If this is incorrect please change the random/fixed designation of the factor.")
      finaleffectrandom[confound.tab[i,5]]<-1
    }

    if (!(confound.tab[i,1] %in% randomfacs) && (finaleffectrandom[confound.tab[i,5]]==1)) {
      warning("\n", names(finaleffectrandom[confound.tab[i,5]]),
              " will be assumed to be random. \n", 
          confound.tab[i,1]," was defined as fixed but it is equivalent to ", names(finaleffectrandom[confound.tab[i,5]]), 
          " and therefore will also be considered as random.",
          " If this is incorrect please change the random/fixed designation of the term.")
    }
  }
  
  if (exists("userfinaleffectrandom")) {
    names(userfinaleffectrandom)<- names(finaleffectrandom)
    bothfinaleffectrandom <- cbind(finaleffectrandom,userfinaleffectrandom)
    
    Default_designation <-c(rep(NA  , length(finaleffectrandom)))
    User_defined_designation <-c(rep(NA  , length(finaleffectrandom)))
    for (i in 1:length(finaleffectrandom)) {
      if (finaleffectrandom[i] == "1") { 
        Default_designation[i] <- "Random"
      } else { 
        Default_designation[i] <- "Fixed "
      }
      
      if (userfinaleffectrandom[i] == "1") { 
        User_defined_designation[i] <- "Random"
      } else { 
        User_defined_designation[i] <- "Fixed "
      }
    }
    
    names(Default_designation)<- names(finaleffectrandom)
    names(User_defined_designation)<- names(finaleffectrandom)
    bothfinaleffectrandom_SilveR <- cbind(Default_designation,User_defined_designation)
    
    if (any(userfinaleffectrandom!=finaleffectrandom)) {
      warning("The random/fixed designation of a term has been changed by the user. The user designation will be used by the program but please chack it is appropriate.")
      print(bothfinaleffectrandom)
      finaleffectrandom <- userfinaleffectrandom
    }
  }
  
  main.effects.table.order <- main.effects.table.print[order(prelim.effect.order),order(prelim.effect.order)]
  main.effects.mat <- matrix(NA,nrow=nrow(main.effects.table.order),ncol=ncol(main.effects.table.order),dimnames=dimnames(main.effects.table.order))
  for (i in 1:nrow(main.effects.table.order)) {
    for (j in 1:ncol(main.effects.table.order)) {
      if (main.effects.table.order[i,j]=="1") main.effects.mat[i,j] <- 1
      if (main.effects.table.order[i,j]=="0") main.effects.mat[i,j] <- 0
      if (main.effects.table.order[i,j]=="(0)") main.effects.mat[i,j] <- 0
      if (main.effects.table.order[i,j]==" ") main.effects.mat[i,j] <- 0
    }
  }

  effects.equiv.interaction <- confound.tab[(confound.tab[ ,4]=="FALSE"),1]    
  
  if (nrow(confound.tab) > 0) {
    if ((length(effects.equiv.interaction)>0) && (any(rownames(main.effects.mat) %in% effects.equiv.interaction)) ) {
      main.effects.mata <- main.effects.mat[-which(rownames(main.effects.mat) %in% effects.equiv.interaction),-which(rownames(main.effects.mat) %in% effects.equiv.interaction)]
    }  else {main.effects.mata <- main.effects.mat}
    for (i in 1:nrow(main.effects.mata)) {
      for (j in 1:ncol(main.effects.mata)) {
        if (main.effects.mata[i,j]==1) {
          for (k in 1:nrow(main.effects.mata)) {
            main.effects.mata[i,k] <- max(main.effects.mata[i,k]-main.effects.mata[j,k],0)
          }
        }
      }
    }
  } 
  
  if (dim(confound.tab)[1] > 0) {
    for (i in 1:dim(confound.tab)[1]) {
      for (j in 1:length(colnames(ceffects.table.final.brief))) {
        if (names(colnames(ceffects.table.final.brief))[j] == confound.tab[i,5]) {
          colnames(ceffects.table.final.brief)[j]<-paste(confound.tab[i,3],"=",brief.colnames[confound.tab[i,5]],sep="") }
      }
    }
  }
  
  main.effects.mat.brief<-substr(rownames(main.effects.mata),1,c(4,abbrev.length[rownames(main.effects.mata)[-1]]))
  main.effects.table.nestnames <- rownames(main.effects.mata)                                                                                                                     
  main.effects.mat.nestbrief <- main.effects.mat.brief
  
  for (i in 2:length(rownames(main.effects.mata))) {
    if (no.confounded>0) {
      if (rownames(main.effects.mata)[i] %in% confound.tab[ ,1] && (confound.tab[confound.tab[ ,1]==rownames(main.effects.mata)[i],4]==T)){
        first <- 1
        for (j in 2:length(colnames(main.effects.mata)))  {
          if (main.effects.mata[i,j]==1 & i != j) {
            if (first==1) {
              main.effects.table.nestnames[i] <- paste(main.effects.table.nestnames[i],"(",main.effects.table.nestnames[colnames(main.effects.mata)==colnames(main.effects.mata)[j]],sep="")
              main.effects.mat.nestbrief[i] <- paste(main.effects.mat.nestbrief[i],"(",main.effects.mat.nestbrief[main.effects.mat.brief == main.effects.mat.brief[j]],sep="")      
            } else {
              main.effects.table.nestnames[i] <- paste(main.effects.table.nestnames[i],"^",main.effects.table.nestnames[rownames(main.effects.mata)==rownames(main.effects.mata)[j]],sep="")
              main.effects.mat.nestbrief[i] <- paste(main.effects.mat.nestbrief[i],"^",main.effects.mat.nestbrief[-1][main.effects.mat.brief[-1] == main.effects.mat.brief[j]],sep="")
            }
            first <- 0
          }
        }
        if (first==0) {
          main.effects.table.nestnames[i] <- paste(main.effects.table.nestnames[i],")",sep="")
          main.effects.mat.nestbrief[i] <- paste(main.effects.mat.nestbrief[i],")",sep="")        
        }
      }
    }
  }
  
  main.effects.table.nestnames <- cbind(rownames(main.effects.mata),main.effects.table.nestnames,main.effects.mat.brief,main.effects.mat.nestbrief)
  
  nested.names <- main.effects.table.nestnames[main.effects.table.nestnames[ ,1] %in% confound.tab[confound.tab[ ,4]==T,1], ,drop=F]
  nested.names <- cbind(nested.names,confound.tab[confound.tab[ ,4]==T,2])
  
  namesind.effects <- matrix(NA,nrow=length(names(finaleffects)),ncol=length(names(finaleffects)))
  
  for (i in 1:length(names(finaleffects))){
    extract<-extract.factor.fun(names(finaleffects)[i])
    for (j in 1:length(extract)) {
      namesind.effects[i,j] <- extract[j]
    }
  }
  
  namesind.effects <- namesind.effects[,colSums(is.na(namesind.effects))<nrow(namesind.effects)]
  
  namesind.briefeffects <- matrix(NA,nrow=length(colnames(effects.table.final.brief)),ncol=length(colnames(effects.table.final.brief)))
  
  for (i in 1:length(colnames(effects.table.final.brief))){
    extract<-extract.factor.fun(colnames(effects.table.final.brief)[i])
    for (j in 1:length(extract)) {
      namesind.briefeffects[i,j] <- extract[j]
    }
  }
  
  namesind.briefeffects <- namesind.briefeffects[,colSums(is.na(namesind.briefeffects))<nrow(namesind.briefeffects)]
  
  cnamesind.briefeffects <- matrix(NA,nrow=length(colnames(ceffects.table.final.brief)),ncol=ncol(main.effects.mat))
  for (i in 1:length(colnames(ceffects.table.final.brief))){
    extract<-extract.factor.fun(colnames(ceffects.table.final.brief)[i])
    for (j in 1:length(extract)) {
      cnamesind.briefeffects[i,j] <- extract[j]
    }
  }
  
  nestednamesind <- matrix(NA,nrow=nrow(nested.names),ncol=length(names(finaleffects)))
  
  if (nrow(nested.names)>0) {
    for (i in 1:nrow(nested.names)) {
      extract<-extract.factor.fun(nested.names[i,5])
      for (j in 1:length(extract)) {
        nestednamesind[i,j] <- extract[j]
      }
    }
  }
  
  nestednamesind.brief <- substr(nestednamesind,1,abbrev.length[nestednamesind])
  
  for (m in 1:nrow(namesind.effects)) {           
    if (nrow(nested.names)>0) {
      for (k in nrow(nested.names):1) {           
        if (nested.names[k,1] %in% namesind.effects[m, ]) {
          if (ncol(namesind.effects)>0) {
            for (r in 1:ncol(namesind.effects)) {       
              if (ncol(nestednamesind)>0) {
                for (p in 1:ncol(nestednamesind)) {      
                  if (!is.na(namesind.effects[m,r]) && !is.na(nestednamesind[k,p]) && nested.names[k,1] != namesind.effects[m,r] 
                      && namesind.effects[m,r]==nestednamesind[k,p]) { namesind.effects[m,r]<-NA }
                }
              }
            }
          }
        }
      }
    }
  }
  
  for (m in 1:nrow(namesind.briefeffects)) {
    if(nrow(nested.names)>0) {
      for (k in nrow(nested.names):1) {
        if (nested.names[k,3] %in% namesind.briefeffects[m, ]) {
          if (ncol(namesind.briefeffects)>0) {
            for (r in 1:ncol(namesind.briefeffects)) {
              if (ncol(nestednamesind.brief)>0) {
                for (p in 1:ncol(nestednamesind.brief)) {
                  if (!is.na(namesind.briefeffects[m,r]) && !is.na(nestednamesind.brief[k,p]) && nested.names[k,3] != namesind.briefeffects[m,r] && namesind.briefeffects[m,r]==nestednamesind.brief[k,p]) {
                    cnamesind.briefeffects[m,r]<-gsub(nestednamesind.brief[k,p],"",cnamesind.briefeffects[m,r])   #Replaces nested main effect by "" - note this may be contained within a term
                    if (!is.na(cnamesind.briefeffects[m,r]) && cnamesind.briefeffects[m,r]=="") {cnamesind.briefeffects[m,r]<-NA}   #Replaces cells in matrix which are "" by NA
                  } 
                }
              }
            }
          }
        }
      }
    }
  }
  
  finalnamesind.effects<-namesind.effects
  finalnamesind.briefeffects<-cnamesind.briefeffects
  finalnames.effects <- rep(NA,nrow(namesind.effects))
  finalnames.briefeffects <- rep(NA,nrow(namesind.effects))
  for (m in 1:nrow(namesind.effects)) {
    for (r in 1:ncol(namesind.effects)) {
      if(nrow(nested.names)>0) {
        for (k in 1:nrow(nested.names)) {
          if (nested.names[k,1] %in% namesind.effects[m,r]) { 
            finalnamesind.effects[m,r]<-gsub(nested.names[k,1],nested.names[k,2],namesind.effects[m,r])      
            if(length(strsplit(cnamesind.briefeffects[m,r],"=")[[1]])<=1) {
              finalnamesind.briefeffects[m,r]<-sub(nested.names[k,3],nested.names[k,4],cnamesind.briefeffects[m,r])
            } else {         
              finalnamesind.briefeffects[m,r]<-paste(substr(cnamesind.briefeffects[m,r],1,gregexpr("=",
                      cnamesind.briefeffects[m,r])[[1]][length(strsplit(cnamesind.briefeffects[m,r],"=")[[1]])-1]), nested.names[k,4] ,sep="")
            }          
          }
        }
      }
    }
    finalnames.effects[m]<-paste(finalnamesind.effects[m,!is.na(finalnamesind.effects[m, ])], collapse="^")
    finalnames.briefeffects[m]<-paste(finalnamesind.briefeffects[m,!is.na(finalnamesind.briefeffects[m, ])], collapse="^")
    finalnames.briefeffects[m]<- gsub("=\\^","=",finalnames.briefeffects[m]  )
  }
  
  cbind(finalnames.effects,finalnames.briefeffects)
  
  confound.effects<-confound.tab[ ,1:2,drop=FALSE]
  for (i in 1:nrow(confound.effects)) {
    if(nrow(nested.names)>0) {
      for (j in 1:nrow(nested.names)) {
        if (confound.effects[i,1]==nested.names[j,1]) confound.effects[i,2]<-nested.names[j,2]  
      } 
    }
  }
  
  rownames(ceffects.table.final.brief) <- finalnames.effects
  colnames(ceffects.table.final.brief) <- finalnames.briefeffects
  
  if (showLS=="Y") {
    
    names(finaleffects)<- finalnames.effects

    adjm <- matrix(NA,nrow=nrow(ceffects.table.final.brief), ncol=ncol(ceffects.table.final.brief),
                   dimnames=dimnames(ceffects.table.final.brief))
    for (i in 1:nrow(ceffects.table.final.brief)) {
      for (j in 1:ncol(ceffects.table.final.brief)) {
        if (ceffects.table.final.brief[i,j]=="1") adjm[i,j] <- 1
        if (ceffects.table.final.brief[i,j]=="0") adjm[i,j] <- 0
        if (ceffects.table.final.brief[i,j]=="(0)") adjm[i,j] <- 0
        if (ceffects.table.final.brief[i,j]==" ") adjm[i,j] <- 0
      }
    }
    
    adjm.adjust <- adjm
    for (j in 1:ncol(adjm)) {
      for (i in 1:nrow(adjm)) {
        if (adjm.adjust[i,j]==1) {
          for (k in 1:nrow(adjm)) {
            adjm.adjust[k,j] <- max(adjm.adjust[k,j]-adjm[k,i],0)
          }
        }
      }
    }
    
    adjm.adjust <- rbind(aaadum1=c(0), Mean=adjm.adjust[1,], zzzdum2=c(0), adjm.adjust[-1,])
    adjm.adjust <- cbind(aaadum1=c(0), Mean=adjm.adjust[,1], zzzdum2=c(0), adjm.adjust[,-1])
    
    adjm.reverse <- adjm.adjust[nrow(adjm.adjust):1, ncol(adjm.adjust):1]
    
    g1 <- graph.adjacency(adjm.reverse,mode="undirected")
    
    g <- simplify(g1)
    V(g)$label <- V(g)$name
    
    dscoords <- dscoords.fun("LS", finaleffects, ceffects.table.final.brief, larger.fontlabelmultiplier, smaller.fontlabelmultiplier, middle.fontlabelmultiplier)  
    g$layout <- dscoords$coords
    
    vertex.label.font <- rep(2,length(colnames(adjm.reverse)))
    vertex.label.color.blue <- c(rep(Colourblue,length(colnames(adjm.reverse))-3),"transparent",Colourblue,"transparent")
    vertex.label.color.black <- c(rep("black",length(colnames(adjm.reverse))-3),"transparent","black","transparent")
    vertex.label.color.red <- c(rep(Colourred,length(colnames(adjm.reverse))-3),"transparent",Colourred,"transparent")
    
    finaleffectrandom.reverse <- c(finaleffectrandom[length(finaleffectrandom):1], 0, 0)
    
    adjm.reverse.blank <- adjm.reverse
    for (m in 1:length(colnames(adjm.reverse.blank))) {
      if (finaleffectrandom.reverse[m]==1) {
        colnames(adjm.reverse.blank)[m] <- paste("",paste(rep("_",nchar(colnames(adjm.reverse.blank)[m])),collapse=""))
      } else {
        colnames(adjm.reverse.blank)[m] <- ""
      }
    }
    
    g2 <- graph.adjacency(adjm.reverse.blank,mode="undirected")
    
    g2a <- simplify(g2)
    V(g2a)$label <- V(g2a)$name
    g2a$layout <- dscoords$coords
    vcount(g2a)
    g2a.edges <- get.edges(g2a,1:ecount(g2a))[ ,1]-1   
    node.dumg <- c(vcount(g2a)-3,vcount(g2a)-1)   
    edge.color<-rep("grey",length(g2a.edges))
    edge.color[g2a.edges %in% node.dumg]<-"transparent"
    par(mar=c((2*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.8, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4, 0.2, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4)) 
    
    plot(g2a, asp=FALSE, add=F,vertex.label.color=vertex.label.color.black, vertex.label.cex=dscoords$textlabel.size,vertex.label.font=vertex.label.font, vertex.label.degree=pi/2, vertex.label.dist=0.6, vertex.size=5, vertex.color="transparent", vertex.shape="circle", vertex.frame.color="white", edge.color=edge.color, edge.width = edgewidth)
    
    if (showpartialLS=="Y") {
      adjm3 <- matrix(0,nrow=nrow(adjm),ncol=ncol(adjm),dimnames=dimnames(adjm))
      for (i in 1:nrow(ceffects.table.final.brief)) {
        for (j in 1:ncol(ceffects.table.final.brief)) {
          if (ceffects.table.final.brief[i,j]=="(0)" && ceffects.table.final.brief[j,i]=="(0)") adjm3[i,j] <- 1
        }
      }

      adjm3.adjust <- adjm3
      for (j in 1:ncol(adjm3)) {
        for (i in 1:nrow(adjm3)) {
          if (adjm3.adjust[i,j]==1) {
            for (k in 1:nrow(adjm3)) {
              adjm3.adjust[k,j] <- max(adjm3.adjust[k,j]-adjm3[k,i],0)
            }
          }
        }
      }
      
      adjm3.adjust <-rbind(aaadum1=c(0), Mean=adjm3.adjust[1, ], zzzdum2=c(0), adjm3.adjust[-1, ])
      adjm3.adjust <-cbind(aaadum1=c(0), Mean=adjm3.adjust[ ,1], zzzdum2=c(0), adjm3.adjust[ ,-1])
      adjm3.reverse <- adjm3.adjust[nrow(adjm3.adjust):1,ncol(adjm3.adjust):1]
      
      g3 <- graph.adjacency(adjm3.reverse,mode="undirected")                   
      g3 <- simplify(g3)
      V(g3)$label <- V(g3)$name
      
      g3$layout <- dscoords$coords
      
      plot(g3, asp=FALSE, add=TRUE, vertex.label.color="transparent",vertex.label.cex=dscoords$textlabel.size, vertex.label.font=2, vertex.size=0, vertex.color="transparent", vertex.frame.color="transparent",  edge.label.color=Colourred, edge.label.font=2, edge.color=Colourorange,edge.lty=dottedline, edge.width = edgewidth)
    }
    
    plot(g, asp=FALSE, add=T,vertex.label.color=vertex.label.color.blue, vertex.label.cex=dscoords$textlabel.size,vertex.label.font=vertex.label.font, vertex.size=0, vertex.color="transparent", vertex.frame.color="transparent", vertex.shape="circle", edge.lty=0, edge.width = edgewidth)
  }
  
  if (showdfLS=="Y") {
    LS.output <- dfs.fun("LS", no.all, finaleffects, ceffects.table.final.brief, adjm, outputlistip1, maxfacs, maxlevels.df, check.shared.df, datadesign)
    g4 <- g
    
    V(g4)$label <- paste(sep="", "[",LS.output$xdfs.reverse[ ,3],LS.output$maxlevelsf.reverse[ ],",",LS.output$xdfs.reverse[ ,4],"]")
    plot(g4, asp=FALSE, add=T,vertex.label.color=vertex.label.color.red, vertex.label.cex=dscoords$textlabel.size.df, vertex.label.font=vertex.label.font, vertex.label.degree=pi/2, vertex.label.dist=(1*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*1,vertex.size=0, vertex.color="transparent", vertex.frame.color="transparent", vertex.shape="circle", edge.lty=0, edge.width = edgewidth)
    LS.output$xdfs
  }
  
  if (pdf=="Y") dev.off()
}



