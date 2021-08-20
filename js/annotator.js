function includeJs(jsFilePath) {
    var js = document.createElement("script");

    js.type = "text/javascript";
    js.src = jsFilePath;

    document.body.appendChild(js);
}

console.log("before jQuery");

jQuery(function ($) {
    $(document.body).annotator()
        .annotator('addPlugin', 'Markdown')
        .annotator('addPlugin', 'Tags');
});

console.log("after jQuery");
