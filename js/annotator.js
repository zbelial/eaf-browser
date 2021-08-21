function includeJs(jsFilePath) {
    var js = document.createElement("script");

    js.type = "text/javascript";
    js.src = jsFilePath;

    document.body.appendChild(js);
}

console.log("before jQuery");

jQuery(function ($) {
    var anno = $(document.body).annotator();

    anno.annotator('addPlugin', 'Markdown')
    anno.annotator('addPlugin', 'Tags');
});

console.log("after jQuery");
