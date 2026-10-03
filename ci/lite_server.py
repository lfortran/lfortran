#!/usr/bin/env python3
"""Static server for the local JupyterLite site (see doc/src/jupyterlite.md).

Unlike `python -m http.server`, this sends the two headers that make the page
cross-origin isolated:

    Cross-Origin-Opener-Policy: same-origin
    Cross-Origin-Embedder-Policy: require-corp

`jupyterlite-xeus` picks its kernel worker based on `crossOriginIsolated`. When
it is true the kernel uses SharedArrayBuffer + Atomics.wait for the synchronous
filesystem calls it needs. When it is false it falls back to proxying every
syscall through a synchronous XMLHttpRequest to the service worker, which races
against service-worker startup: the kernel then dies inside ___syscall_openat
with "InvalidStateError: An attempt was made to use an object that is not, or
is no longer, usable", and the notebook hangs at "Connecting" forever.

Also serves .wasm as application/wasm so WebAssembly.compileStreaming works.
"""

import argparse
import functools
import http.server
import socketserver


class Handler(http.server.SimpleHTTPRequestHandler):
    extensions_map = {
        **http.server.SimpleHTTPRequestHandler.extensions_map,
        ".wasm": "application/wasm",
        ".mjs": "text/javascript",
        ".json": "application/json",
    }

    def end_headers(self):
        self.send_header("Cross-Origin-Opener-Policy", "same-origin")
        self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
        # Same-origin resources are exempt from COEP, but be explicit so the
        # assets stay loadable if the site is ever embedded elsewhere.
        self.send_header("Cross-Origin-Resource-Policy", "same-origin")
        # The site is rebuilt in place; never let the browser reuse old assets.
        self.send_header("Cache-Control", "no-store, must-revalidate")
        super().end_headers()

    def log_message(self, fmt, *args):
        # Keep the default one-line-per-request log, minus the date noise.
        print(f"{self.address_string()} {fmt % args}", flush=True)


class Server(socketserver.ThreadingTCPServer):
    allow_reuse_address = True
    daemon_threads = True


def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("-d", "--directory", default="dist", help="directory to serve")
    p.add_argument("-b", "--bind", default="127.0.0.1", help="address to bind")
    p.add_argument("port", nargs="?", type=int, default=8000)
    args = p.parse_args()

    handler = functools.partial(Handler, directory=args.directory)
    with Server((args.bind, args.port), handler) as httpd:
        print(f"Serving {args.directory} (cross-origin isolated) on "
              f"http://{args.bind}:{args.port}/lab/index.html", flush=True)
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            pass


if __name__ == "__main__":
    main()
