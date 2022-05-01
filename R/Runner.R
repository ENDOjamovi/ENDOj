## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
                        inherit = Initer,
                        cloneable=FALSE,
                        class=TRUE,
                        public=list(
                          model=NULL,
                          zmodel=NULL,
                          bmodel=NULL,
                          summary=NULL,
                          initialize=function(options,dispatcher,datamatic) {
                            super$initialize(options,dispatcher,datamatic)
                          },
                          estimate = function(data) {
                            opts_list<-list(formula=self$formula,data=data)

                            results<-try_hard(do.call(ivreg::ivreg,opts_list))
                            
                            if (!isFALSE(results$error))
                                stop(results$error)
                            if (!isFALSE(results$warning))
                               self$dispatcher$warnings<-list(topic="info",message=results$warning)
                            self$model<-results$obj
                            self$summary<-summary(self$model)
                            
                            if (self$option("es","beta")) {
                              for (n in names(data))
                                    data[[n]]<-as.numeric(scale(data[[n]]))
                              results<-try_hard(ivreg::ivreg(self$formula,data=data))
                              self$zmodel<-results$obj
                            }
                            if (self$option("ci_method",c("perc","bca"))) {
                              .model<-self$model
                              ### car Boot() requires a .carEnv environment
                              ### which is not loaded automatically by import
                              ### we force to attach the environment so Boot will work
                              ### however, attaching it twice produces an error
                              try_hard(attachNamespace("car"))
                              self$bmodel<-car::Boot(.model,
                                                     R=self$options$boot_r)

                            }
                            

                          },
                          run_main_r2=function() {
                            
                            test<-self$summary$waldtest
                            tab<-list(r2=self$summary$r.squared,
                                      ar2=self$summary$adj.r.squared,
                                      df1=test[[3]],
                                      df2=test[[4]],
                                      test=test[[1]],
                                      p=test[[2]])

                            return(list(tab))
                          },
                          run_main_anova=function() {
                            
                            tab<-as.data.frame(car::Anova(self$model,type=3))
                            names(tab)<-c("df","f","p")
                            tab$source<-row.names(tab)
                            return(tab)
                          },
                          
                          run_main_coefficients=function() {
                            
                            tab<-as.data.frame(self$summary$coefficients)
                            names(tab)<-c("estimate","se","t","p")
                            tab$source<-row.names(tab)
                            tab$df<-attr(self$summary$coefficients,"df")
                            ### confidence intervals ###
                            .model<-self$model
                            .type<-NULL
                            if (self$option("ci_method",c("perc","bca"))) {
                              .model<-self$bmodel
                              .type<- self$options$ci_method
                            }
                            ci<-stats::confint(.model,type=.type)
                            tab$est.ci.lower<-ci[,1]
                            tab$est.ci.upper<-ci[,2]
                            ### betas ###
                            if (is.something(self$zmodel)) {
                               tab$beta<-self$zmodel$coefficients
                               if (self$option("betas_ci")) {
                                 ci<-stats::confint(self$zmodel)
                                 tab$beta.ci.lower<-ci[,1]
                                 tab$beta.ci.upper<-ci[,2]
                               }
                                   
                            }
                            return(tab)
                          },
                          run_main_vcov=function() {

                            tab<-as.data.frame(self$summary$vcov)
                            tab$source<-row.names(tab)
                            return(tab)
                            
                          },
                          run_diagnostics_standard=function() {
                            
                            tab<-as.data.frame(self$summary$diagnostics)
                            names(tab)<-c("df1","df2","test","p")
                            tab$source<-row.names(tab)
                            return(tab)
                            
                          },
                          
                          
                          last_fun=function() {}
                          

                          ), # end of public function estimate

                        private=list(
                          # do private stuff
                        ) #end of private
)  # end of class


