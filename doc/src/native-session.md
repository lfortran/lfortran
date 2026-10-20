# Native interactive sessions

The native local binding carries **Jupyter logical messages** (5.4 behavior), not the
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

Invalid initial hellos receive a best-effort `{"connection_error":"reason"}`
and close only that connection; framing errors can close it immediately.
After hello, malformed supported requests with a valid, matching header
receive bounded, parented error replies. Uncorrelatable malformed messages,
wrong/late input replies and duplicate request IDs are ignored, not executed.
Neither case resets other clients' state or destroys accepted completion routes.
Peers that send no complete hello have a bounded admission timeout.

Limits include 64 peers, 128 queued executions, 32 MiB of queued request bytes,
and 4096 recent request IDs globally plus all outstanding IDs (including
across reconnects). When the execution queue is full and the origin still has
delivery capacity, a valid new execution receives a private
`execute_reply(status="error", ename="QueueFull")` and parented busy/idle;
that cell is not executed and the route remains available for accepted work.

### Bounded per-origin admission and delivery

Before accepting an execution, the kernel reserves its origin's completion
capacity. Each origin has at most 256 outstanding delivery reservations and
32 MiB of reserved bytes, shared across the evaluator and transport queues.
An execution reserves 128 KiB plus its encoded user-expression errors and
three copies of the parent header; requests requiring more than one frame
of reservation are rejected before evaluation. Immediate replies (including
queries and rejections) reserve their encoded size plus status/header space.
Execution error replies retain at most 4096 diagnostic bytes, with an explicit
truncation notice; full diagnostics remain live error output.

Reservations stay attached to accepted cells and their completion/input
messages through the local or ZeroMQ outbound queues. Taking a message from
the evaluator queue does **not** free admission capacity. Capacity is released
when the transport sends the retained messages, or on actual disconnect.
Already accepted queued/running executions keep their private destinations
and outcomes even if later traffic exhausts that origin's budget.

When a delivery budget is full, further traffic from that origin is
**unadmitted**: no additional cell executes, and there is no guarantee of an
individual rejection reply to an unlimited flood. Sending is not acceptance.
Resume reading retained replies to release capacity, then submit new work
with fresh IDs; the server never automatically replays dropped or accepted
cells. There is no eventual-progress guarantee for a client that never reads,
and a successful transport send is not an application acknowledgment.
Ordinary clients should bound their outstanding work and await matching
reply/idle before retry decisions.

Live output is separate: each peer's evaluator/transport live-output queue
is bounded by 32 MiB/4096 events. A slow observer may miss stream/result/
display events rather than block others; its own reserved terminal reply
and local idle do not compete with that output. Standard Jupyter PUB/IOPub
is likewise live delivery, not retained historical replay. Healthy routes,
READ completion, heartbeat and control continue while another origin is
throttled. An actual disconnected origin's queued cells are cancelled before
evaluation; an already running cell may have an unknown outcome and its input
waiter is released. Fatal evaluator/process or whole-transport failure remains
a kernel-wide failure, not a policy for a malformed or non-consuming peer.

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
header strings have bounded length. The native kernel advertises `5.4`;
compatible `5.x` header versions are accepted without rewriting their parents.
This does not enable newer subshell/registration/encryption capabilities.
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

Completion is not necessarily success: the terminal interprets the explicit
reply status, reports `aborted` as a cell not executed, and reports an invalid
or missing status as an unknown outcome. No-output cells are not assumed
successful merely because idle arrived.

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
The optional standard-Jupyter transport below implements a separate wire
binding around these same interfaces; native-local tests alone are not evidence
of external interoperability.

The registered stdlib Python `test_native_session` and existing `test_repl`,
`test_evaluation`, and `test_runtime_input` cover local behavior. The Python
protocol harness includes a Windows non-PTY path; POSIX PTY and slow-socket
checks are platform-specific. A passing macOS run is not Windows, WASM or
external-Jupyter interoperability evidence.

## Optional classic Jupyter ZeroMQ adapter

`WITH_JUPYTER_ZMQ` is **OFF by default**. It directly links libzmq's C API and
OpenSSL libcrypto, not Xeus, cppzmq, nlohmann, RapidJSON or the language server.
It requires a native LLVM build, and is explicitly rejected for no-LLVM/WASM.
The `llvm11-jupyter` Pixi environment declares the transport and real-test
dependencies (`zeromq`, `openssl`, `jupyter_client`, `ipykernel`, `pyzmq`).
Its build directory is separate from the default `build/llvm11`:

```bash
CMAKE_BUILD_PARALLEL_LEVEL=6 pixi run -e llvm11-jupyter build \
  -DWITH_JUPYTER_ZMQ=yes -DWITH_XEUS=no -DWITH_JSON=no \
  '-DCMAKE_CXX_FLAGS_DEBUG=-g -O2' '-DCMAKE_C_FLAGS_DEBUG=-g -O2' \
  > jupyter-build.log 2>&1
# After ordinary source edits:
CMAKE_BUILD_PARALLEL_LEVEL=6 pixi run -e llvm11-jupyter rebuild \
  > jupyter-rebuild.log 2>&1
pixi run -e llvm11-jupyter ctest -R '^test_jupyter_' -V \
  > jupyter-tests.log 2>&1
```

Reference tests and reference updates still use **`-e llvm11`**, not this
optional environment. Development environments currently also provision
optional Xeus packages for other build configurations; package presence does
not make them default-binary dependencies.

### Native kernel with both bindings

Create a classic connection file in an owner-private directory. For example,
on POSIX (choose a new filename; do not overwrite another kernel's file):

```bash
mkdir -p "$HOME/.lfortran/jupyter"
chmod 700 "$HOME/.lfortran/jupyter"
pixi run -e llvm11-jupyter python -c \
  'import os,secrets; from jupyter_client.connect import write_connection_file; write_connection_file(os.path.expanduser("~/.lfortran/jupyter/classic.json"), ip="127.0.0.1", key=secrets.token_hex(32).encode())'
pixi run -e llvm11-jupyter start --serve \
  --connection-file "$HOME/.lfortran/jupyter/native.json" \
  --jupyter-connection-file "$HOME/.lfortran/jupyter/classic.json"
```

`native.json` is the native discovery/hello binding, **not** a Jupyter
connection file. `classic.json` is the ordinary Jupyter five-port connection
file. A `BlockingKernelClient`/Jupyter frontend uses the latter, and local
clients/terminals use the former. Both reach **one** serialized native session,
one evaluator, one shared variable namespace and one execution/history count.
This command does not launch the legacy Xeus kernel as a second evaluator.
The caller owns the supplied classic file; server cleanup removes only its
native descriptor/IPC artifacts, never that supplied file.

The supported classic schema has `transport:"tcp"`, `ip`, five distinct integer
`shell_port`, `control_port`, `stdin_port`, `iopub_port`, `hb_port` in 1..65535,
`signature_scheme:"hmac-sha256"` and a string `key`. Empty `ip` defaults to
loopback. Use loopback unless you deliberately arrange external transport
security. IPC, registration files, other signature schemes and Curve/encryption
fields are rejected explicitly, including empty Curve fields; they are never
silently ignored or downgraded to plaintext.

Kernel sockets are shell/control/stdin ROUTER, IOPub PUB (protocol 5.4), and
heartbeat REP. Client sockets are DEALER/SUB/REQ; shell and stdin have the same
binary routing identity for each origin. All socket access is on its owning
reactor thread, not the evaluation/capture worker. Actual runtime READ demand
goes only to its transport origin. Standard clients may omit input_reply's
parent: the authenticated route and its one pending prompt supply correlation.
Other clients' code or input cannot answer that prompt. TCP disconnects are
tracked through libzmq socket monitors/source descriptors; disconnect of the
input origin releases the native runtime waiter.

The wire codec preserves full binary identity stacks, separate original JSON
frames for signature verification, and separate raw buffers (including empty
and NUL-containing frames). HMAC-SHA256 covers **exactly the four posted JSON
frame bytes**, before parsing; validation uses constant-time comparison.
Nonempty keys are always enforced. Empty keys deliberately select unsigned
messaging. Standard Jupyter HMAC does **not** cover routing/buffer frames and
does **not encrypt traffic**. A key/file grants code-execution access, not a
safe read-only feed. Keep both connection files private (0600 in a private
directory, or owner-only Windows ACLs).

Malformed frames and invalid/replayed signatures are discarded without
executing code. Multipart work, frame sizes, JSON depth, peer/output/request
queues and replay state are bounded. Signature history retains 4096 recent
digests; native request-ID history additionally retains outstanding IDs.
This is bounded replay protection, not indefinite replay persistence.
Each ROUTER peer has separate bounded outbound queues, so a blocked client
does not head-of-line block others. IOPub remains a live PUB subscription, not
reliable historical replay or a slow-consumer archive.

The same per-origin admission/reservation policy applies to the ZeroMQ
adapter, including its outbound ROUTER queues. A blocked route retains its
accepted outcomes while other routes continue; it does not terminate the
kernel or erase evaluator state. Routable malformed supported requests get
bounded correlated errors; optional `comm_info_request` returns an empty valid
`comm_info_reply` because no comm targets are implemented. Comm notifications
are ignored. Actual TCP disconnects still remove only that disconnected peer.

Use a real `kernel_info` plus IOPub/status readiness handshake, such as
`BlockingKernelClient.wait_for_ready()`. Heartbeat alone does not establish an
IOPub subscription, and a fixed startup sleep is not readiness. Heartbeat and
control remain serviced during READ and busy native code. Shutdown drains with
a deadline/bounded linger, cleans owned IPC, and terminates a still-blocked
native process rather than asynchronously unwinding through generated code.

### Connect a terminal to an external kernel

```bash
pixi run -e llvm11-jupyter start --connect /private/python-connection.json
```

`--connect` detects a native descriptor versus a classic connection file.
Without the optional feature, classic files and adapter options receive an
actionable build-option diagnostic; no JSON file is compiled as Fortran.
The terminal owns no evaluator and obtains kernel info and completeness from
the remote kernel, including Python multiline input. Execute flags, parent
correlation, streams/stderr, errors, rich text/plain, clear notices and input
use the same terminal implementation as local sessions. Password prompts
suppress terminal echo and restore it on return/error.
`is_complete` is optional in classic Jupyter: if it does not reply within one
second, the terminal reports that completeness assistance is unavailable and
allows submission without running a local language parser. Optional queries
do not consume long-lived bridge execution slots. Errors present only in a
private execute_reply are rendered after the reply/idle fence; a corresponding
IOPub error is not rendered twice.

The optional client establishes kernel_info/IOPub readiness by bounded protocol
retries, not sleeps. It checks heartbeat independently of execution duration:
quiet or slow cells are not dead merely because they produce no output.
Heartbeat loss (10 seconds without a valid echo) or a changed upstream kernel
session reports an unknown execution outcome. Execute requests are never
automatically resubmitted.

**Standard Jupyter input has no EOF/cancel representation.** Ctrl-D at a remote
input prompt reports this explicitly and sends no fake empty string, sentinel,
or local EOF metadata upstream. The external kernel may remain waiting; only
its owner/manager can safely interrupt or terminate it. Closing a client or
bridge does not invent successful execution or input cancellation.

### Evaluator-free local bridge (including pi-repl integrations)

```bash
pixi run -e llvm11-jupyter start --bridge /private/python-connection.json \
  --connection-file "$HOME/.lfortran/jupyter/native-proxy.json"
# A terminal can use the same local binding as a pi-repl connection adapter:
pixi run -e llvm11-jupyter start --connect "$HOME/.lfortran/jupyter/native-proxy.json"
```

The bridge contains **no EvaluationService/FortranEvaluator**. Every local peer
gets separate upstream shell/control/stdin sockets with matching identities.
One IOPub subscription fans out once to ready local peers, preserving upstream
kernel session, request parents, arbitrary metadata/MIME/unknown fields and
base64/raw buffer boundaries. Shell/control replies and input remain private
to their transport origin, including with several clients sharing the same
remote variable namespace. The descriptor and hello expose the real upstream
kernel session, not an invented Fortran kernel identity.

Bridge hello advertises `stdin` and `rich_output`, **not** `stdin_eof`.
EOF/cancel or an input-origin disconnect is reported as unsupported upstream
cancellation; no fabricated value is sent. Pending work is not replayed on
disconnect/restart. A local `shutdown_request` stops only the bridge and returns
`metadata.lfortran_bridge_local:true`; it never shuts down a foreign-owned
kernel. Restart is rejected. To stop/interrupt the external kernel, use its
own manager. Normal bridge termination cleans only its own descriptor/IPC.

pi-repl need not link ZeroMQ or embed a Fortran evaluator: its connection
adapter can implement the local framing/hello above, point it at
`native-proxy.json`, and retain complete logical envelopes when rendering or
forwarding events. No pi-repl source changes or unsupported pi-repl CLI flags
are prescribed here. For example, this minimal POSIX stdlib client submits
through either native descriptor (use Python code when bridging Python):

```python
import datetime, json, socket, struct, sys, uuid
descriptor = json.load(open(sys.argv[1]))
connection = socket.socket(socket.AF_UNIX)
connection.connect(descriptor["socket"])
def receive():
    def read(n):
        data = b""
        while len(data) < n:
            part = connection.recv(n-len(data))
            if not part:
                raise EOFError("kernel connection lost; outcome unknown")
            data += part
        return data
    size, = struct.unpack("!I", read(4))
    if not 0 < size <= 16777216:
        raise ValueError("invalid frame length")
    return json.loads(read(size))
def send(message):
    data = json.dumps(message).encode()
    connection.sendall(struct.pack("!I", len(data)) + data)
session, request = uuid.uuid4().hex, uuid.uuid4().hex
send(dict(lfortran_local_protocol=1, client_session=session, features=[]))
hello = receive()  # Subscription barrier; inspect advertised features.
header = dict(msg_id=request, session=session, username="pi-repl-adapter",
    date=datetime.datetime.now(datetime.timezone.utc).isoformat(),
    msg_type="execute_request", version="5.4")
send(dict(channel="shell", header=header, parent_header={}, metadata={},
    content=dict(code="1+1", silent=False, store_history=True,
                 allow_stdin=False, stop_on_error=True, user_expressions={}),
    buffers=[]))
reply = idle = False
while not (reply and idle):
    event = receive()
    if "connection_error" in event:
        raise RuntimeError(event["connection_error"])
    print(event)  # Keep full envelopes, including metadata/MIME/buffers.
    if event.get("parent_header", {}).get("msg_id") != request:
        continue
    kind = event["header"]["msg_type"]
    reply |= kind == "execute_reply"
    idle |= kind == "status" and event["content"]["execution_state"] == "idle"
connection.close()
```

### Compatibility and coverage

`lfortran kernel -f classic.json` and the existing `Fortran` kernelspec remain
the optional legacy Xeus adapter. Shared evaluation/magics/display ABI remain
common, but this new transport does not claim to repair Xeus's pre-existing
buffering/native-input limitations. xeus-lite/WASM remains separate.

The registered optional `test_jupyter_native`, `test_jupyter_foreign`, and
`test_jupyter_wire` run in the existing Linux LLVM 11 platform Quick job
(same required job name/ruleset). They use independent real jupyter_client
signed/unsigned clients, simultaneous local peers, an actual ipykernel, and an
independent Session/pyzmq fixture for exact signatures, routing, raw buffers,
unknown fields and both reply/idle arrival orders. They are not ordinary
default-suite imports. The tests establish those surfaces, not universal
interoperability with every kernel/version or locally unrun Windows/WASM
platforms. No Exhaustive run is needed for these optional capability tests.
