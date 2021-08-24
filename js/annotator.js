console.log("before jQuery");

console.log(file_uri);
console.log(file_name_md5);

jQuery(function ($) {
    var anno = $(document.body).annotator();

    new QWebChannel(qt.webChannelTransport, channel => {
        console.log('init pyobject');
        window.pyobject = channel.objects.pyobject;


        // 放在这儿是为了保证插件初始化时window.pyobject已经有值了（StoreEAF初始化时会用到window.pyobject）
        if(enable_tags_plugin) {
            console.log("enable tags plugin");
            anno.annotator('addPlugin', 'Tags');
        }
        if(enable_markdown_plugin) {
            console.log("enable markdown plugin");
            anno.annotator('addPlugin', 'Markdown')
        }
        anno.annotator('addPlugin', 'StoreEAF', {
            fileNameMD5: file_name_md5,
            fileFullName: file_full_name,
            pyobject: window.pyobject,
        });
    });
});

console.log("after jQuery");
