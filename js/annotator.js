console.log("before jQuery");

console.log(file_uri);
console.log(file_name_md5);
console.log(annotator_server_port);

jQuery(function ($) {
    var anno = $(document.body).annotator();

    anno.annotator('addPlugin', 'Markdown')
    anno.annotator('addPlugin', 'Tags');
    anno.annotator('addPlugin', 'Store', {
        annotationData: {},
        emulateHTTP: false,
        loadFromSearch: false,
        prefix: 'http://localhost:' + annotator_server_port,
        urls: {
            root:  '/' + file_name_md5 + '/',
            index:  '/' + file_name_md5 + '/annotations',
            create:  '/' + file_name_md5 + '/annotations',
            read:  '/' + file_name_md5 + '/annotations/:id',
            update:  '/' + file_name_md5 + '/annotations/:id',
            destroy: '/' + file_name_md5 + '/annotations/:id',
        }
    });
});

console.log("after jQuery");
