console.log("before jQuery");

console.log(file_uri);
console.log(file_name_md5);

jQuery(function ($) {
    var anno = $(document.body).annotator();

    anno.annotator('addPlugin', 'Markdown')
    anno.annotator('addPlugin', 'Tags');
    anno.annotator('addPlugin', 'StoreEAF', {
        fileMD5: file_name_md5,
    });
});

console.log("after jQuery");
