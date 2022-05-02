

Initer <- R6::R6Class(
  "Initer",
  class=TRUE, 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Scaffold,
  public=list(
    datamatic=NULL,
    formula=NULL,
    independents=NULL,
    instruments=NULL,
    initialize=function(options,dispatcher,datamatic) {
      
      super$initialize(options,dispatcher)
      self$datamatic<-datamatic
      private$.build_syntax()


    }, # here initialize ends
    #### init functions #####
    
    init_info=function() {
      
      tab<-list()
      tab[[1]]<-list(info="Model",specs="2SLS regression")
      tab[[2]]<-list(info="Call",specs=self$formula)
      tab[[3]]<-list(info="Independent Vars",specs=paste(self$independents,collapse=", "))
      tab[[4]]<-list(info="Instrument Vars",specs=paste(self$instruments,collapse = ", "))
      tab[[5]]<-list(info="",specs="")
      tab[[6]]<-list(info="C.I. Method",specs=switch(self$options$ci_method,
                                                wald = "Standard",
                                                perc     = "Quantiles",
                                                bca      =  "Bias-corrected and accelerated"
                                                ))
      tab[[7]]<-list(info="S.E. Method",specs=switch (self$options$se_method,
                                                wald = "Wald",
                                                robust   = "Robust"
      ))
      
      return(tab)
    },
    ### parameter estimates ####
    init_main_anova=function() {
      
      .terms<-self$independents
      .len<-length(.terms)+2
      lapply(1:.len, function(i) list(source=""))
      
    },
    
    ### parameter estimates ####
    init_main_coefficients=function() {
      
      .terms<-self$independents
      .len<-length(.terms)+1
      lapply(1:.len, function(i) list(source=""))
      
    },
    init_main_vcov=function() {
      
      .terms<-self$independents
      .len <- length(.terms)+1
      
      mat<-as.data.frame(matrix(".",nrow=.len,ncol=.len+1))
      names(mat)<-c("source","(Intercept)",.terms)
      mat$source<-c("(Intercept)",.terms)
      mat
    },
    
    last_fun=function() {}

  ),   # End public
  
  private=list(

    .build_syntax=function() {
      
      self$independents   <- c(self$options$endo,self$options$exo)
      idform              <- jmvcore::composeFormula(self$options$dep,self$independents)
      
      instruments         <- c(self$options$iv,self$options$exo)
      isform              <- jmvcore::composeFormula(NULL,instruments)
      isform              <- gsub("~","|",isform,fixed = T )
      self$formula        <- paste(idform,isform,collapse  = "|")
      self$instruments    <- self$options$iv
    }
  ) # end of private
) # End Rclass


