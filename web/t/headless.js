#!/usr/bin/env node
'use strict';
// headless.js — drive the page in headless Chrome over the DevTools pipe and
// print the terminal's console output.  Used as a smoke test:
//
//   node web/t/headless.js 'http://localhost:18080/?eval=(print%20(*%206%207))' [timeout-ms]
//
// Needs a Chrome/Chromium binary; set CHROME or rely on the playwright cache.

const { spawn } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');

const url = process.argv[2];
const timeoutMs = parseInt(process.argv[3] || '180000', 10);
// remaining args: lines to type at the REPL, one per 'ready'; then stdin is closed
const script = process.argv.slice(4);
if (!url) { console.error('usage: headless.js URL [timeout-ms]'); process.exit(2); }

function findChrome() {
  if (process.env.CHROME) return process.env.CHROME;
  const cache = path.join(os.homedir(), '.cache', 'ms-playwright');
  const cands = [];
  if (fs.existsSync(cache)) for (const d of fs.readdirSync(cache)) {
    const p1 = path.join(cache, d, 'chrome-headless-shell-linux64', 'chrome-headless-shell');
    const p2 = path.join(cache, d, 'chrome-linux', 'chrome');
    if (fs.existsSync(p1)) cands.push(p1); if (fs.existsSync(p2)) cands.push(p2);
  }
  cands.sort().reverse();
  for (const c of ['chromium', 'chromium-browser', 'google-chrome']) cands.push(c);
  return cands[0];
}

const chrome = spawn(findChrome(), ['--headless', '--no-sandbox', '--disable-gpu', '--remote-debugging-pipe', 'about:blank'],
                     { stdio: ['ignore', 'ignore', 'ignore', 'pipe', 'pipe'] });
const out = chrome.stdio[3], inp = chrome.stdio[4];
let nextId = 1, buf = '';
const pending = new Map();
function send(method, params, sessionId) {
  const id = nextId++;
  const msg = { id, method, params: params || {} };
  if (sessionId) msg.sessionId = sessionId;
  out.write(JSON.stringify(msg) + '\0');
  return new Promise((res, rej) => pending.set(id, { res, rej }));
}
let sessionId = null, done = false;
const finish = (code) => { if (done) return; done = true; chrome.kill(); process.exit(code); };
inp.on('data', (d) => {
  buf += d.toString();
  let i;
  while ((i = buf.indexOf('\0')) >= 0) {
    const m = JSON.parse(buf.slice(0, i)); buf = buf.slice(i + 1);
    if (m.id && pending.has(m.id)) { const p = pending.get(m.id); pending.delete(m.id); m.error ? p.rej(new Error(JSON.stringify(m.error))) : p.res(m.result); }
    else if (m.method === 'Runtime.consoleAPICalled') {
      const text = m.params.args.map((a) => a.value !== undefined ? String(a.value) : a.description || '').join(' ');
      console.log(text);
      if (text === 'log: [ready]') { void processReady(); }
      if (/^\[modus exited|^err: |^log: \[modus exited/.test(text) || text.includes('[modus exited')) setTimeout(() => finish(0), 200);
    } else if (m.method === 'Runtime.exceptionThrown') {
      console.log('EXCEPTION: ' + JSON.stringify(m.params.exceptionDetails).slice(0, 500));
    }
  }
});
let readyBusy = false;
async function processReady() {
  if (readyBusy) return; readyBusy = true;
  try {
    // "@file NAME:CONTENT" drops a file; "@js EXPR" runs JS in the page and
    // AWAITS its result; any other line is typed at the REPL.
    let line = script.shift();
    while (line !== undefined && (line.startsWith('@file ') || line.startsWith('@js '))) {
      if (line.startsWith('@file ')) {
        const k = line.indexOf(':');
        await send('Runtime.evaluate', { expression: `modusAddFile(${JSON.stringify(line.slice(6, k))}, ${JSON.stringify(line.slice(k + 1))})` }, sessionId);
      } else {
        const r = await send('Runtime.evaluate', { expression: line.slice(4), awaitPromise: true, returnByValue: true }, sessionId).catch((e) => ({ error: e.message }));
        console.log('@js => ' + JSON.stringify(r && r.result ? r.result.value : (r && r.error)));
      }
      line = script.shift();
    }
    if (line !== undefined) await send('Runtime.evaluate', { expression: `modusFeed(${JSON.stringify(line + '\n')})` }, sessionId);
    else await send('Runtime.evaluate', { expression: 'modusClose()' }, sessionId);
  } finally { readyBusy = false; }
}
(async () => {
  const { targetId } = await send('Target.createTarget', { url: 'about:blank' });
  sessionId = (await send('Target.attachToTarget', { targetId, flatten: true })).sessionId;
  await send('Runtime.enable', {}, sessionId);
  await send('Page.enable', {}, sessionId);
  await send('Page.navigate', { url }, sessionId);
  setTimeout(() => { console.log('[headless: timeout]'); finish(1); }, timeoutMs);
})().catch((e) => { console.error(e); finish(1); });
