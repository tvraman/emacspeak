// WGet using PhantomJS

var page = require('webpage').create(),
    system = require('system'),
    target='http://emacspeak.sf.net';


page.settings.userAgent  =
    "Mozilla/5.0 (Linux; Intel  )" +
    "AppleWebKit/537.36 (KHTML, like Gecko) " +
    "Chrome/36.0.1944.0 Safari/537.36";

if (system.args.length > 1) {
    target = Array.prototype.slice.call(system.args, 1);
}

var _eCallBack = function () {
    var r = document;
    return (new XMLSerializer()).serializeToString(r)  ;
};

var _pCallBack = function(status) {
    var result;
    if (status !== 'success') {
        console.log('Error: Unable to access network!');
    } else {
        result = page.evaluate(_eCallBack);
        try {
            console.log('');
            console.log(result);
        } catch (e) {
            console.log('Error:', e.toString());
        }
    }
    phantom.exit();
};

page.open(target, _pCallBack);
