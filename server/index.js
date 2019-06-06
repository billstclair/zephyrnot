var port = (process.env.PORT || 8081),
    verbose = (process.env.VERBOSE || null),
    http = require('http'),
    ecstatic = require('ecstatic'),
    repl = require('repl')
    server = http.createServer(
      ecstatic({ root: __dirname })
    ),
    WebSocketServer = require('./WebSocketServer.js'),
    app = require('./server.js').Elm.Zephyrnot.Server.Server.init({ flags: verbose }),
    wss = new WebSocketServer(
      server,
      app.ports.inputPort,
      app.ports.outputPort,
      verbose
    );

server.listen(port);

console.log(`Listening on :${port}`);

repl.start();
