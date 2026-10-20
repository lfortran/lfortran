# Native interactive sessions

The native local binding carries **Jupyter 5.4 logical messages**, not the
ZeroMQ multipart wire protocol. It needs no Python, Xeus, JSON library package,
or ZeroMQ dependency. LLVM Support's JSON implementation is used privately in
native LLVM builds. This binding is unavailable in no-LLVM and WASM builds;
the existing optional Xeus and xeus-lite kernels remain separate adapters.

## Discovery, privacy and lifetime

`lfortran` launches the same executable as an isolated managed kernel before
creating an evaluator or initializing LLVM targets in the terminal process.
The kernel inherits compiler options and the working directory, but its OS
stdin is disconnected from the terminal. An inherited startup pipe reports
readiness or failure; no startup delay is used as a readiness assumption.

Without `--connection-file`, the descriptor and endpoint live in a new private
directory under `$HOME/.lfortran` (`%USERPROFILE%\.lfortran` on Windows).
With `--connection-file PATH`, only the descriptor is put at PATH; the endpoint
still lives outside the working directory. The descriptor's immediate parent
must already be user-private. The kernel never replaces an existing descriptor,
socket or pipe. It removes only artifacts it created. Unix sockets have 0600
permissions in a 0700 directory; descriptor files have 0600 permissions.
Available Unix peer credentials are checked. Windows uses a protected user-only
ACL, rejects remote named-pipe clients, and checks descriptor ownership/ACLs.
Session IDs and JSON usernames are **not** authentication.

Example POSIX descriptor:

```json
{"lfortran_local_protocol":1,"transport":"unix",
 "socket":"/home/user/.lfortran/s-unique/kernel.sock",
 "kernel_session":"unique-kernel-id","pid":12345,"managed":true}
```

On Windows, `transport` is `named_pipe` and `pipe` replaces `socket`, for
example `"pipe":"\\\\.\\pipe\\lfortran-unique-kernel-id"`. Descriptors and
messages are UTF-8 on both transports.

A managed kernel stays alive while any authenticated client is attached and
stops on final detach, including when the original terminal goes away first.
A managed child that receives no client exits after a bounded startup grace
period. POSIX launch detaches/reaps its short-lived launcher; Windows restricts
inherited handles to startup reporting and null standard handles. The kernel
does not keep a terminal's input/output pipes open. Explicit `--serve` persists
with zero clients. A parented control-channel `shutdown_request` with
`{"restart":false}` returns `shutdown_reply`, aborts queued requests, drains
outbound messages up to a bounded shutdown deadline and exits. Shutdown remains
responsive during a runtime input wait or an infinite user-code loop.
Restart is unsupported.

Normal shutdown, termination requests and runtime `exit`/`STOP` remove owned
artifacts. An uncatchable signal or OS failure can leave a private stale
descriptor/socket. Attachment then fails explicitly; it never starts a new
evaluator or replays a cell. After independently confirming that the recorded
process/session is dead, its owner can remove the stale files. Do not blindly
unlink paths obtained from someone else's connection file.

Access permits arbitrary native code execution with the kernel user's
permissions. There is no sandbox, rollback, network encryption, automatic
reconnect, interrupt recovery, state migration or historical transcript replay.
Do not share a descriptor as a harmless status feed.

## Framing and ready barrier

One bidirectional byte stream per peer multiplexes all channels. Every frame is
a four-byte unsigned **big-endian** length followed by that many bytes of UTF-8
JSON. Length must be 1 through 16,777,216 inclusive, including base64 overhead.
Readers must handle partial headers/payloads and multiple frames in one read;
writers must handle partial writes. JSON nesting is limited to 64.

The first client frame is a local hello, not a Jupyter message:

```json
{"lfortran_local_protocol":1,"client_session":"unique-client-id",
 "features":["stdin_eof"]}
```

The acknowledgment is the subscription-ready barrier:

```json
{"lfortran_local_protocol":1,"kernel_session":"unique-kernel-id",
 "jupyter_protocol":"5.4",
 "features":["stdin","stdin_eof","rich_output"],
 "max_frame_bytes":16777216}
```

Do not submit work until this acknowledgment arrives. It establishes a live
IOPub subscription, not a transcript: a client joining during execution sees
only subsequent events. The first attachment can see unparented
`status(starting)` and `status(idle)`. Later attachments do not invent another
kernel startup. The hello session must match subsequent request headers.

Malformed peers receive a best-effort `{"connection_error":"reason"}` frame
and are disconnected. Framing errors can close the stream immediately.
Limits include 64 peers, 128 queued executions, 32 MiB of queued request bytes,
32 MiB/4096 events per peer in each output queue, and 4096 recent request IDs
globally plus all outstanding IDs (including across reconnects). Oversized,
flooded or slow peers are disconnected,
not silently truncated and not allowed to stall evaluation/control for others.
Peers that send no complete hello have a bounded admission timeout.

## Logical messages

Example execution:

```json
{"channel":"shell",
 "header":{"msg_id":"unique-request-id","session":"unique-client-id",
   "username":"display-name","date":"2026-10-20T08:00:00Z",
   "msg_type":"execute_request","version":"5.4"},
 "parent_header":{},"metadata":{},
 "content":{"code":"integer :: n = 41\nn + 1",
   "silent":false,"store_history":true,"allow_stdin":true,
   "stop_on_error":true,"user_expressions":{}},
 "buffers":[]}
```

`channel` is `shell`, `control`, `stdin`, or `iopub`. Clients send executions on
shell, input replies on stdin, and shutdown/info on shell or control.
Header fields are strings; IDs/session/type/date/version are nonempty,
header strings have bounded length, and version is `5.4`.
`header`, `parent_header`, `metadata`, and `content` are objects.
`buffers` is an ordered array of **individually base64-encoded binary frames**,
including empty strings for zero-byte frames. It is not a list of JSON values
masquerading as binary data. Unknown envelope/content/metadata fields and MIME
data remain available in raw client envelopes; forwarding adapters must keep
them and convert buffers to/from raw multipart frames without merging them.
The native evaluator does not interpret request buffers.

Every outgoing logical message has its own new ID and the kernel session.
Related replies and events copy the original request header into
`parent_header`. Shell/control replies go only to the origin; ordinary IOPub
code/output/status events go to all ready peers. The request ID, client
session, kernel session, transport peer and execution count are different
identities. Duplicate known IDs cause rejection, never a second execution.
An ordinary request's parent header must be empty. See below for input replies.

One execution worker owns one `EvaluationService`. Accepted executions are
serialized globally, including across clients. A successful ordinary cell
produces parented busy, execute_input, streams/displays as applicable,
execute_result if it has a value, execute_reply, and idle. A compiler failure
produces an error event and error execute_reply, then idle, and later cells can
recover. Other immediate shell/info requests have their own busy/reply/idle
sequence even while the execution worker is busy; correlate status by parent
rather than treating an unrelated idle as completion of your cell.

**Execution completes only after both its matching `execute_reply` and matching
IOPub `status(idle)` have arrived**, in either arrival order. A prompt, a stream,
a result, or another request's idle is not completion. Transport loss before
that fence makes the outcome unknown/dead, including fatal `STOP`, runtime
termination, signal or nonrecoverable JIT failure. Never fabricate success or
automatically resend such an execution.

### Supported request policies

* `execute_request`: `silent`, `store_history`, `allow_stdin`, and `stop_on_error`
  are honored. Silent cells suppress code, stream, error, display and result
  publication and force history off, but still send status and a private reply.
  History count is global and advances only for stored, non-silent executions.
* `stop_on_error=true` aborts the snapshot of executions queued when that cell
  fails, across all peers; every aborted cell gets an aborted execute_reply and
  parented busy/idle. Cells submitted after the failure are accepted normally.
* `user_expressions` returns an explicit `NotImplementedError` for **each key**;
  expressions are not evaluated as additional cells or silently ignored.
* `kernel_info_request`: implementation, protocol and Fortran language details.
  Control/info responsiveness supplies local liveness while code/READ blocks;
  this binding does not claim to implement a ZeroMQ heartbeat socket.
* `is_complete_request`: shared native token-based block/continuation checking;
  invalid syntax is submitted for normal compiler diagnostics, not suppressed.
  This is an editing heuristic, not a guarantee that code compiles.
* `complete_request`: valid empty matches with cursor bounds.
* `inspect_request`: `found=false`, empty data/metadata.
* `history_request`: in-memory input tail only, session 0, at most 100 entries
  and 4 MiB; use `hist_access_type:"tail",n:N`. Other history modes return an
  empty history, as do requests for output history (`output:true`); neither
  output history nor on-disk persistence is invented.
* `shutdown_request`: no restart, as described above.
* Native-only `content.lfortran_verbose=true` requests the existing detailed
  evaluation output. It is not a standard Jupyter option.

All six existing inspection magics (`%%showast`, `%%showasr`, `%%showllvm`,
`%%showasm`, `%%showcpp`, `%%showfmt`) run through the same evaluation service.

## Runtime input and output

Actual runtime input demand generates `input_request` **only to the origin**,
parented to its execution. Other clients can keep editing, submit queued code
or query info, but their submissions never become input records.

Respond on stdin with `input_reply`, `content:{"value":"one record"}` and a
`parent_header` identifying that **input_request's** msg_id and kernel session.
Only that outstanding prompt, its originating transport peer, and its client
session may answer. Wrong-peer, duplicate, late or wrongly parented replies
are rejected. Records contain no CR/LF; an empty value is a blank record.

If hello negotiated `stdin_eof`, `metadata:{"lfortran_stdin_eof":true}` with an
empty value explicitly means EOF. This is a namespaced local extension, never
implicitly forwarded to ordinary external Jupyter kernels. The runtime keeps
EOF separate from a blank line and resets input bytes between executions.
`allow_stdin=false` and peer disconnect install an unavailable-input provider,
not NULL/libc fallback; programs can observe an input error through IOSTAT.
Named/internal file reads retain ordinary file semantics.

Native stdout **and** stderr file descriptors are continuously drained while
code runs, including C runtime writes and output larger than pipe capacity.
Before input, display/clear events and final results/replies/idle, the worker
flushes C/C++ streams and waits for an explicit capture-drain acknowledgment.
There are no stdout marker bytes, one-shot reads or fixed-delay flush fences.
Order within each stream and relative to those fences is preserved; unrelated
writes to different OS descriptors have no promised total byte ordering.
Split UTF-8 codepoints are retained across reads/fences; invalid byte sequences
are replaced with U+FFFD. A final incomplete codepoint is replaced at the end
of the execution. Rich MIME data is not flattened; application/json is a JSON
value, not a double-encoded string.

The terminal uses text/plain only and filters terminal-control sequences; it
does not execute HTML or remote clear-screen commands. Clear output is shown
as a notice, without erasing the editor/history. Arbitrary foreign C code
reading OS stdin is not mediated (it sees the disconnected/null stdin).
Native compiled programs outside sessions retain the ordinary stdio fallback.

## Embedding and optional-adapter boundary

`src/lfortran/native_session.h` deliberately exposes no LLVM/JSON types:

* `SessionEndpoint`: peer-oriented `receive`, `disconnect`,
  `take_deliveries`, id/liveness/lifetime queries.
* `NativeSession`: the endpoint with one evaluation service/worker, bounded
  queues, capture, input mediation and Jupyter logical behavior.
* `serve_local_endpoint`: secure local IPC/descriptor/lifecycle reactor around
  any endpoint. A bridge can implement an endpoint with **no evaluator**.
* `SessionAdapter`: `start`, nonblocking `poll`, `deliver`, `pending`, `stop`,
  all on the reactor thread. Optional adapters allocate identities at or above
  `first_adapter_peer`; local identities use the lower half. Their deliveries
  and local deliveries come from the **same** session. Adapter stop must not
  throw and must release owned endpoints/files before forced process exit.
* `SessionClient`: protocol-only request/event/raw-envelope/input interface,
  used by `src/bin/session_terminal.h`. A foreign-kernel transport can implement
  it without embedding Fortran evaluation/completeness logic in the terminal.
  `ClientEvent.envelope` preserves the complete original envelope, including
  unknown fields, MIME metadata and base64 buffer boundaries.

Native session/transport sources are compiled only in native LLVM builds.
No compiler backend contains protocol, routing, capture or terminal logic.
The optional standard-Jupyter transport/bridge is not implemented by this
binding. Real ZeroMQ/HMAC, heartbeat sockets, external-kernel interoperability
and ZMQ/local simultaneous-client verification are separate adapter work, not
claims implied by successful native-local tests.

The registered stdlib Python `test_native_session` and existing `test_repl`,
`test_evaluation`, and `test_runtime_input` cover local behavior. The Python
protocol harness includes a Windows non-PTY path; POSIX PTY and slow-socket
checks are platform-specific. A passing macOS run is not Windows, WASM or
external-Jupyter interoperability evidence.
