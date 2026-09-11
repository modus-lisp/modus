'use strict';
// coi-serviceworker.js — make a page cross-origin isolated on a host that
// cannot send COOP/COEP headers (GitHub Pages, most static hosts).
//
// SharedArrayBuffer — which the worker uses to block on stdin with
// Atomics.wait — is only available when `crossOriginIsolated` is true, and
// that requires the document to be served with
//     Cross-Origin-Opener-Policy: same-origin
//     Cross-Origin-Embedder-Policy: require-corp
// GitHub Pages does not let you set response headers, so instead a service
// worker re-serves same-origin responses with those headers added.  On the
// first (uncontrolled) load the page registers this worker and reloads once;
// the reloaded page is controlled, gets the headers, and is isolated.
//
// Include it from <head> BEFORE the app script:  <script src="coi-serviceworker.js"></script>
// (This is the well-known coi-serviceworker technique.)

if (typeof window === 'undefined') {
  // ---- service-worker context ----
  self.addEventListener('install', () => self.skipWaiting());
  self.addEventListener('activate', (e) => e.waitUntil(self.clients.claim()));
  self.addEventListener('message', (e) => { if (e.data === 'coi-skip-waiting') self.skipWaiting(); });
  self.addEventListener('fetch', (event) => {
    const req = event.request;
    if (req.cache === 'only-if-cached' && req.mode !== 'same-origin') return;
    // Only rewrite same-origin responses: those are the document and the app's
    // own subresources.  Cross-origin fetches (e.g. http-get to another site)
    // pass through untouched, governed by CORS as before.
    if (new URL(req.url).origin !== self.location.origin) return;
    event.respondWith((async () => {
      const res = await fetch(req);
      if (res.status === 0) return res;                 // opaque; leave alone
      const headers = new Headers(res.headers);
      headers.set('Cross-Origin-Opener-Policy', 'same-origin');
      headers.set('Cross-Origin-Embedder-Policy', 'require-corp');
      headers.set('Cross-Origin-Resource-Policy', 'same-origin');
      return new Response(res.body, { status: res.status, statusText: res.statusText, headers });
    })());
  });
} else {
  // ---- window context ----
  window.coiStatus = crossOriginIsolated ? 'isolated' : 'pending';
  if (!crossOriginIsolated) {
    if (!window.isSecureContext || !('serviceWorker' in navigator)) {
      window.coiStatus = 'unavailable';                 // http:// or no SW support
    } else {
      const reloadedKey = 'coiReloaded';
      navigator.serviceWorker.register(document.currentScript.src, { scope: './' }).then((reg) => {
        // The controller only exists after a reload following activation.
        if (navigator.serviceWorker.controller) return; // already controlled but not isolated? give up quietly
        const tryReload = () => {
          if (sessionStorage.getItem(reloadedKey)) { window.coiStatus = 'unavailable'; return; }
          sessionStorage.setItem(reloadedKey, '1');
          window.location.reload();
        };
        if (reg.active) tryReload();
        else navigator.serviceWorker.ready.then(tryReload);
      }).catch(() => { window.coiStatus = 'unavailable'; });
      // clear the one-shot guard once we do come back isolated
      if (sessionStorage.getItem(reloadedKey) && !crossOriginIsolated) { /* handled above */ }
    }
  } else {
    sessionStorage.removeItem('coiReloaded');
  }
}
