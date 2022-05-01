
const events = {
    update: function(ui) {
        console.log("update");
    
    },

    onChange_endo: function(ui) {
        
        console.log("onChange_endo");

    },
    onChange_exo: function(ui) {
        
        console.log("onChange_exo");

    },

    onChange_instruments: function(ui) {
        
        console.log("onChange_instruments");

    },

    onChange_doNothing: function(ui) {
        console.log("do nothing");

    },

   onUpdate_modelSupplier: function(ui) {
       
        console.log("update_modelSupplier");
       
   },    
   onChange_model_terms: function(ui) {
       
        console.log("change_model_terms");
       
   },
   onChange_model_remove: function(ui) {
       
        console.log("change_model_remove");
       
   }


    
};


module.exports = events;
