// Serialize  HTML DOM
var page = require('webpage').create(),
    system = require('system'),
    url='http://www.google.com/search?source=hp&num=25&q='
var q='emacspeak'

if (system.args.length > 1) {
    q = Array.prototype.slice.call(system.args, 1);
}
                                                       
Element.prototype.innerText = function(){              
    var serializer = new XMLSerializer();                   
    var serialized = serializer.serializeToString(this);    
    return serialized;                                      
}                                                       
var target = url + q;
page.open(target, function(status) {
    var result;
    if (status !== 'success') {
        console.log('Error: Unable to access network!');
    } else {
        result = page.evaluate(function () {
            var r = document;
            return '<html><head>' +
                r.head.innerHTML +
                '</head><body>' +
                r.body.innerHTML +
                '</body></html>';
        });

        try {
            console.log('');
            console.log(result);
        } catch (e) {
            console.log('Error:', e.toString());
        }
    }
    phantom.exit();
});

