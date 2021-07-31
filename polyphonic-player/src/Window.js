"use strict";

var wrapper = function() {

  return {

    print : function() {
       window.print();
    }
    
  }

}();

exports.print = wrapper.print;
