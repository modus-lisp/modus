#!/usr/bin/env node
'use strict';
// serve.js — a static server for web/ that sets the cross-origin isolation
// headers SharedArrayBuffer needs.   node web/serve.js [port]

const http = require('http');
const fs = require('fs');
const path = require('path');

const root = __dirname;
const args = process.argv.slice(2);
const noCoi = args.includes('--no-coi');   // emulate GitHub Pages: no COOP/COEP headers
const port = parseInt(args.find((a) => !a.startsWith('--')) || '8080', 10);
const types = { '.html': 'text/html; charset=utf-8', '.js': 'text/javascript', '.mvmw': 'application/octet-stream', '.gz': 'application/octet-stream' };

http.createServer((req, res) => {
  let p = decodeURIComponent(req.url.split('?')[0]);
  if (p === '/') p = '/index.html';
  const file = path.join(root, path.normalize(p));
  if (!file.startsWith(root) || !fs.existsSync(file) || fs.statSync(file).isDirectory()) { res.writeHead(404); res.end('not found'); return; }
  const headers = { 'Content-Type': types[path.extname(file)] || 'application/octet-stream', 'Cache-Control': 'no-cache' };
  if (!noCoi) {
    headers['Cross-Origin-Opener-Policy'] = 'same-origin';
    headers['Cross-Origin-Embedder-Policy'] = 'require-corp';
  }
  res.writeHead(200, headers);
  fs.createReadStream(file).pipe(res);
}).listen(port, () => console.log(`serving ${root} on http://localhost:${port}/${noCoi ? ' (no COOP/COEP — Pages emulation)' : ''}`));
