

function loadTextFileImpl(elementid) {
  return function (callback) {
    return function () {
      // console.log("inside load text file effects function ");
      var selectedFile = document.getElementById(elementid).files[0];
      var reader = new FileReader();

      reader.onload = function(event) {
        var contents = event.target.result;
        var filespec = {contents:contents, name:selectedFile.name};
        // console.log("reader.onload File contents: " + contents);
        callback (filespec)();
      };

      if (typeof selectedFile != 'undefined') {
         reader.readAsText(selectedFile);
      }
    }
  }
}

function saveTextFile(filespec) {
  return function () {
    var a = document.createElement("a");
    // console.log("File contents: " + filespec.contents);
    var file = new Blob([filespec.contents], {type: "text/plain;charset=utf-8"});
    url = URL.createObjectURL(file);
    a.href = url;
    a.download = filespec.name;
    document.body.appendChild(a);
    a.click();
    setTimeout(function(){
        document.body.removeChild(a);
        window.URL.revokeObjectURL(url);
    }, 100);
    return true;
  };
}



exports.loadTextFileImpl = loadTextFileImpl;
exports.saveTextFile = saveTextFile;
