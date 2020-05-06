// Copyright 2016 T. V. Raman, Volker Sorge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Commentary:


/**
 * @fileoverview Server for connecting Emacs to the Speech-Rule-Engine.
 * @author tv.raman.tv@gmail.com (T. V. Raman)
 *
 *  Expose a simple REPL as a server to emacspeak-maths.
 *
 *  Usage Model:
 *
 *  The Emacs  client sends requests of the form:
 *  command: arg.
 *  The server responds with a Lisp S-expression.
 *  This S-expression is an annotated string.
 *  Annotations are ACSS property/value specifications.
 *  At any given time, the server loop is working with a given math expression,
 *  Emacs issues browse/render commands against that expression.
 */

// Code:

// Load the TCP Library
let net = require('net');

// Initialize Math rendering system.

let mjx = require('mathjax-full');
let promise = mjx.init(
  {loader: {paths: {sre: 'mathjax-full/js/a11y/sre-node'},
            load: ['input/tex-full', 'a11y/semantic-enrich']}});

// Speech Rules Engine.
sre.setupEngine(
    {markup: 'acss', domain: 'emacspeak', rules: ['EmacspeakRules']});

// Auxiliary methods for error handling.
let errorGen = {};

errorGen.parseError = function(error) {
  return '(parse-error "' + error.replace(/\\/g, '\\\\') + '")';
};

errorGen.mathjaxErrors = function(errors, socket) {
  socket.write(errors.map(errorGen.parseError).join(' '));
};

// table of request handlers:

let handlers = {};

// Add the various handlers:

// Accept a LaTeX math expression:
handlers.enter = function(expr, socket) {
    promise.then((mjx) => {
        let mml = mjx.tex2mml(expr, {display: true});
        socket.write(sre.walk(mml));
    });
};

// Implement Handlers:
handlers.up = function(expr, socket) {
  socket.write(sre.move('UP'));
};
handlers.down = function(expr, socket) {
  socket.write(sre.move('DOWN'));
};
handlers.left = function(expr, socket) {
  socket.write(sre.move('LEFT'));
};
handlers.right = function(expr, socket) {
  socket.write(sre.move('RIGHT'));
};
handlers.repeat = function(expr, socket) {
  socket.write(sre.move('TAB'));
};
handlers.depth = function(expr, socket) {
  socket.write(sre.move('SPACE'));
};
handlers.root = function(expr, socket) {
  socket.write(sre.move('HOME'));
};

// Start a TCP Server on port 5000
net.createServer(function(socket) {
     // Identify this client
     socket.name = socket.remoteAddress + ':' + socket.remotePort;

     // Method: respond
     function respond(message, sender) {
       // message is of the form:
       // cmd: args, args, args
         let request = message.toString();
       let cmd = request.split(':', 1)[0];
       let args = request.slice(cmd.length + 1);
       let handler = handlers[cmd];
       if (handler !== undefined) {
         handler.call(null, args, socket);
       } else {
         process.stdout.write('Handler for ' + request[0] + ' not defined.\n');
       }
     }

     // Announce yourself:
     socket.write('(welcome  \"Maths Speech  Server! \")\n ');
     // Handle incoming messages from Emacs:
     socket.on('data', function(data) {
       respond(data, socket);
     });

     // Shutdown server on disconnect:
     socket.on('end', function() {
       socket.destroy();
       process.exit();
     });

   })
    .listen(5000);

// Put a friendly message on the terminal of the server.
console.log('Math server running at port 5000\n');
