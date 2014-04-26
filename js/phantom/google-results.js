// Serialize Google Search Results 
var page = require('webpage').create(),
    system = require('system'),
    url='http://www.google.com/search?source=hp&num=25&q=',
var q='emacspeak';
var phantomjs.page.settings.userAgent  = 
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/53 "
+"(KHTML, like Gecko) Chrome/35.0.87";
if (system.args.length > 1) {

    q = Array.prototype.slice.call(system.args, 1);
}
                                                       
var target = url + q;
page.open(target, function(status) {
    var result;
    if (status !== 'success') {
        console.log('Error: Unable to access network!');
    } else {
        result = page.evaluate(function () {
            var r = document;
            return '<div>' + r.getElementById('res').innerHTML + '</div>';
                
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

