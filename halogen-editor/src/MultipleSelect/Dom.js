"use strict";

exports.resetDefaultSelected = function () {
    var selectionMenu = document.getElementById("selection-menu");
    selectionMenu.options[0].selected = true;
  };
