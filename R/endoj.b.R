endojClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "endojClass",
    inherit = endojBase,
    private = list(
        .dispatcher = NULL,
        .data_machine = NULL,
        .runner_machine = NULL,
        .plotter_machine = NULL,
        .ready = NULL,
        .time = NULL,
        .smartObjs = list(),
        .init = function() {
            ginfo(paste("MODULE: ENDOj  #### phase init  ####"))
            
            private$.time <- Sys.time()
            private$.ready <- readiness(self$options)
            if (!private$.ready$ready) {
                if (private$.ready$report) {
                    self$results$info$addRow("info", list(info = "Setup", specs = private$.ready$reason))
                }
                return()
            }
            
            ### set up the R6 workhorse class
            private$.dispatcher               <- Dispatch$new(self$results)
            private$.data_machine             <- Datamatic$new(self$options,private$.dispatcher, self$data)
            private$.runner_machine           <- Runner$new(self$options, private$.dispatcher, private$.data_machine)
            
            
            ### info table ###
            aSmartObj<-SmartTable$new(self$results$info,private$.runner_machine )
            private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
            
            ### r2 table ###
            aSmartObj<-SmartTable$new(self$results$main$r2,private$.runner_machine )
            private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
            ### anova table ###
            aSmartObj<-SmartTable$new(self$results$main$anova,private$.runner_machine )
            private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
            
            ### estimates table ###
            aSmartObj<-SmartTable$new(self$results$main$coefficients,private$.runner_machine)
            aSmartObj$ci("est",self$options$ci_width)
            aSmartObj$ci("beta",self$options$ci_width,label=greek_vector[["beta"]])
            private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)

            ### vcov table ###
            aSmartObj<-SmartTable$new(self$results$main$vcov,private$.runner_machine)
            aSmartObj$expandable<-TRUE
            aSmartObj$expandFrom<-2
            private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
            
            ### endo test table ###
            aSmartObj<-SmartTable$new(self$results$diagnostics$standard,private$.runner_machine)
            private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
            
            ### init all tables ###    
            for (smarttab in private$.smartObjs) {
                smarttab$initTable()
            }
            
            now <- Sys.time()
            ginfo("INIT TIME:", now - private$.time, " secs")
        },
        .run = function() {
            ginfo("MODULE:  #### phase run ####")
            
            private$.ready <- readiness(self$options)
            if (!private$.ready$ready) {
                return()
            }
            runnow <- Sys.time()
            data <- private$.data_machine$cleandata(self$data)
            
            private$.runner_machine$estimate(data)
            
            ### run tables ###
            for (smarttab in private$.smartObjs) {
                smarttab$runTable()
            }
            ginfo("MODULE:  #### phase end ####")
            
            ginfo("RUN TIME:", Sys.time() - runnow, " secs")
            
            ginfo("TIME:", Sys.time() - private$.time, " secs")
            
            return()
        } ### end of run
    ) ### end of private
) # endo of class


