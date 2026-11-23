/* Cross-Origin Isolation fallback Service Worker for static hosts (e.g. GitHub Pages). */
if (typeof window === "undefined") {
  self.addEventListener("install", () => self.skipWaiting());
  self.addEventListener("activate", (event) => event.waitUntil(self.clients.claim()));
  self.addEventListener("fetch", (event) => {
    const req = event.request;
    if (req.cache === "only-if-cached" && req.mode !== "same-origin") return;
    event.respondWith(
      fetch(req).then((res) => {
        if (res.status === 0) return res;
        const headers = new Headers(res.headers);
        headers.set("Cross-Origin-Embedder-Policy", "require-corp");
        headers.set("Cross-Origin-Opener-Policy", "same-origin");
        headers.set("Cross-Origin-Resource-Policy", "same-origin");
        return new Response(res.body, {
          status: res.status,
          statusText: res.statusText,
          headers,
        });
      }),
    );
  });
} else if (!window.crossOriginIsolated && "serviceWorker" in navigator && window.isSecureContext) {
  navigator.serviceWorker
    .register(new URL("./coi-serviceworker.js", window.location.href).href)
    .then((reg) => {
      if (reg.installing && !navigator.serviceWorker.controller) {
        reg.installing.addEventListener("statechange", function () {
          if (this.state === "activated") window.location.reload();
        });
      }
    })
    .catch(() => {});
}
