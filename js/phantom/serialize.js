// Serialize  HTML DOM
var page = require('webpage').create(),
    system = require('system'),
    url='http://emacspeak.sf.net'; //default


if (system.args.length > 1) {
    url = Array.prototype.slice.call(system.args, 1);
}
page.open(url, function(status) {
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
