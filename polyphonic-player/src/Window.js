"use strict";

var wrapper = function() {

  return {

    print : function() {
       window.print();
    }
    
  }

}();

export var print = wrapper.print;
