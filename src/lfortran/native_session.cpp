#include <lfortran/native_session.h>
#include <lfortran/native_protocol.h>
#include <lfortran/native_capture.h>
#include <lfortran/evaluation.h>
#include <libasr/config.h>
#include <lfortran/parser/parser.h>
#include <lfortran/parser/parser.tab.hh>
#include <condition_variable>
#include <algorithm>
#include <atomic>
#include <deque>
#include <map>
#include <mutex>
#include <set>
#include <thread>

namespace LCompilers::LFortran {
using namespace Local;
namespace {
bool complete(const std::string &code) {
    Allocator allocator(4096);
    diag::Diagnostics diagnostics;
    std::vector<YYSTYPE> values;
    auto result = tokens(allocator, code, diagnostics, &values, nullptr, false);
    if (!result.ok) return true; // Submit invalid syntax for normal diagnostics.
    std::vector<int> stack;
    const auto &t = result.result;
    for (size_t begin = 0; begin < t.size();) {
        size_t end = begin;
        while (end < t.size() && t[end] != TK_NEWLINE && t[end] != TK_SEMICOLON)
            ++end;
        size_t first = begin;
        begin = end + 1;
        if (first < end && t[first] == TK_LABEL) ++first;
        if (first+1 < end && t[first+1] == TK_COLON) first += 2;
        if (first >= end) continue;
        const int token = t[first];
        // Fortran keywords are not reserved: declarations, assignments and
        // expression operands named "module"/"function"/"do" are not blocks.
        if (first+1 < end && (t[first+1] == TK_EQUAL ||
                (t[first+1] == TK_DBL_COLON && token != KW_TYPE))) continue;
        bool procedure_prefix = token == KW_FUNCTION || token == KW_SUBROUTINE ||
            token == KW_RECURSIVE || token == KW_PURE || token == KW_IMPURE ||
            token == KW_ELEMENTAL || token == KW_MODULE || token == KW_INTEGER ||
            token == KW_REAL || token == KW_COMPLEX || token == KW_LOGICAL ||
            token == KW_CHARACTER || token == KW_DOUBLE_PRECISION ||
            token == KW_DOUBLE || token == KW_TYPE || token == KW_CLASS;
        bool procedure = false;
        if (procedure_prefix) {
            for (size_t j = first; j+1 < end; ++j) {
                if (t[j] == TK_DBL_COLON || t[j] == TK_EQUAL) break;
                if ((t[j] == KW_FUNCTION || t[j] == KW_SUBROUTINE) &&
                        t[j+1] != TK_LPAREN && t[j+1] != TK_EQUAL) {
                    stack.push_back(t[j]);
                    procedure = true;
                    break;
                }
            }
        }
        if (procedure) continue;
        if (token == KW_DO || token == KW_DOWHILE) stack.push_back(KW_DO);
        else if (token == KW_PROGRAM || token == KW_BLOCK || token == KW_SELECT ||
                token == KW_ASSOCIATE || token == KW_INTERFACE ||
                (token == KW_ABSTRACT && first+1 < end && t[first+1] == KW_INTERFACE) ||
                (token == KW_MODULE && first+1 < end && t[first+1] != KW_PROCEDURE) ||
                (token == KW_TYPE && first+1 < end && t[first+1] != TK_LPAREN))
            stack.push_back(token);
        else if (token == KW_IF) {
            for (size_t j = first+1; j < end; ++j)
                if (t[j] == KW_THEN) { stack.push_back(KW_IF); break; }
        } else if (token == KW_END_DO || token == KW_ENDDO ||
                token == KW_END_SUBROUTINE || token == KW_ENDSUBROUTINE ||
                token == KW_END_FUNCTION || token == KW_ENDFUNCTION ||
                token == KW_END_PROGRAM || token == KW_ENDPROGRAM ||
                token == KW_END_MODULE || token == KW_ENDMODULE ||
                token == KW_END_BLOCK || token == KW_ENDBLOCK ||
                token == KW_END_SELECT || token == KW_ENDSELECT ||
                token == KW_END_IF || token == KW_ENDIF ||
                token == KW_END_TYPE || token == KW_ENDTYPE ||
                token == KW_END_INTERFACE || token == KW_ENDINTERFACE ||
                token == KW_END_ASSOCIATE || token == KW_ENDASSOCIATE ||
                token == KW_END) {
            if (!stack.empty()) stack.pop_back();
        }
    }
    auto last = code.find_last_not_of(" \t\r\n");
    return stack.empty() && (last == std::string::npos || code[last] != '&');
}
Object expression_errors(const Object &content) {
    Object result;
    if (auto expressions = content.getObject("user_expressions")) {
        for (const auto &entry : *expressions) {
            result[entry.first] = Object{{"status", "error"},
                {"ename", "NotImplementedError"},
                {"evalue", "user_expressions are not supported"},
                {"traceback", Array{}}};
        }
    }
    return result;
}
}
struct NativeSession::Impl {
    struct Budget {
        std::atomic<size_t> count{0}, bytes{0};
    };
    struct Reservation {
        std::shared_ptr<Budget> budget;
        size_t bytes;
        Reservation(std::shared_ptr<Budget> budget, size_t bytes)
            : budget(std::move(budget)), bytes(bytes) {
            ++this->budget->count;
            this->budget->bytes += bytes;
        }
        ~Reservation() {
            budget->bytes -= bytes;
            --budget->count;
        }
    };
    struct Peer {
        std::string session;
        bool eof = false;
        std::deque<SessionDelivery> output;
        size_t bytes = 0;
        std::shared_ptr<Budget> budget = std::make_shared<Budget>();
    };
    struct Request {
        uint64_t peer;
        Value message;
        size_t bytes = 0;
        std::shared_ptr<void> retention = nullptr;
    };
    CompilerOptions options;
    const std::string session = new_session_id();
    mutable std::mutex mutex;
    std::condition_variable changed;
    std::map<uint64_t, Peer> peers;
    std::set<std::string> known_ids, outstanding_ids;
    std::deque<std::string> recent_ids;
    std::deque<Request> queue;
    size_t queue_bytes = 0, history_bytes = 0;
    std::deque<std::pair<int64_t, std::string>> history;
    bool stopping = false, active = false;
    bool announced = false;
    int64_t count = 0;
    uint64_t input_peer = 0;
    std::string input_id, input_record;
    bool input_waiting = false, input_ready = false;
    int input_status = 1;
    std::thread worker;

    explicit Impl(CompilerOptions opts) : options(std::move(opts)),
        worker([this] { work(); }) {}
    ~Impl() {
        {
            std::lock_guard<std::mutex> lock(mutex);
            stopping = true;
            input_status = 1;
            input_ready = true;
            changed.notify_all();
        }
        worker.join();
    }
    void fail_peer(uint64_t id, const std::string &reason) {
        auto p = peers.find(id);
        if (p == peers.end()) return;
        p->second.output.clear();
        auto text = encode(Object{{"connection_error", reason}});
        p->second.output.push_back({id, text, true});
        p->second.bytes = text.size();
        p->second.session.clear();
        if (input_waiting && input_peer == id) {
            input_status = 1;
            input_ready = true;
            changed.notify_all();
        }
    }
    std::shared_ptr<void> reserve(uint64_t peer, size_t bytes) {
        auto p = peers.find(peer);
        if (p == peers.end() || bytes > max_output) return {};
        auto budget = p->second.budget;
        if (budget->count >= 2 * max_requests ||
                budget->bytes + bytes > max_output) return {};
        return std::make_shared<Reservation>(std::move(budget), bytes);
    }
    void send(uint64_t peer, const Value &value,
            std::shared_ptr<void> retention = {}) {
        const auto text = encode(value);
        auto p = peers.find(peer);
        if (p == peers.end() || p->second.session.empty()) return;
        // Live output is best effort for a slow observer. Reserved outcomes
        // never compete with it or with unadmitted rejection traffic.
        if (text.size() > max_frame) return;
        if (!retention && (p->second.bytes + text.size() > max_output ||
                p->second.output.size() >= 4096)) return;
        bool best_effort = !retention;
        p->second.output.push_back({peer, text, false, std::move(retention), best_effort});
        p->second.bytes += text.size();
    }
    void publish(const std::string &type, const Object &parent, Object content,
            uint64_t origin = 0, std::shared_ptr<void> retention = {}) {
        auto value = message("iopub", type, session, parent, std::move(content));
        for (auto &p : peers)
            send(p.first, value, p.first == origin ? retention : nullptr);
    }
    void reply(const Request &request, const std::string &type, Object content) {
        const auto &m = object(request.message);
        auto value = message(string(m, "channel"), type, session,
            member(m, "header"), std::move(content));
        auto retention = request.retention;
        if (!retention) retention = reserve(request.peer, encode(value).size());
        if (retention) send(request.peer, value, std::move(retention));
        if (type == "execute_reply") {
            auto id = string(member(m, "header"), "msg_id");
            outstanding_ids.erase(id);
            remember(id);
        }
    }
    void remember(const std::string &id) {
        if (!known_ids.insert(id).second) return;
        recent_ids.push_back(id);
        if (recent_ids.size() > 4096) {
            known_ids.erase(recent_ids.front());
            recent_ids.pop_front();
        }
    }
    void abort_queue() {
        while (!queue.empty()) {
            auto aborted = std::move(queue.front());
            queue.pop_front();
            queue_bytes -= aborted.bytes;
            auto &h = member(object(aborted.message), "header");
            publish("status", h, Object{{"execution_state", "busy"}});
            reply(aborted, "execute_reply",
                Object{{"status", "aborted"}, {"execution_count", count}});
            idle(h, aborted.peer, aborted.retention);
        }
    }
    void idle(const Object &parent, uint64_t origin = 0,
            std::shared_ptr<void> retention = {}) {
        publish("status", parent, Object{{"execution_state", "idle"}},
            origin, std::move(retention));
    }
    void immediate_reply(Request &request, const std::string &type, Object content) {
        const auto &parent = member(object(request.message), "header");
        auto size = encode(message(string(object(request.message), "channel"),
            type, session, parent, Object(content))).size();
        if (size > max_frame) return;
        request.retention = reserve(request.peer,
            size + 2 * encode(Object(parent)).size() + 8192);
        if (!request.retention) return;
        publish("status", parent, Object{{"execution_state", "busy"}});
        reply(request, type, std::move(content));
        idle(parent, request.peer, request.retention);
    }
    struct Events : EvaluationEvents {
        Impl &owner;
        Object parent;
        bool silent;
        NativeCapture *capture = nullptr;
        Events(Impl &owner, const Object &parent, bool silent)
            : owner(owner), parent(parent), silent(silent) {}
        void stream(const std::string &name, const std::string &text) override {
            capture->fence();
            captured_stream(name, llvm::json::fixUTF8(text));
        }
        void captured_stream(const std::string &name, const std::string &text) {
            if (silent) return;
            std::lock_guard<std::mutex> lock(owner.mutex);
            owner.publish("stream", parent, Object{{"name", name}, {"text", text}});
        }
        void display(const std::string &mime, const std::string &text) override {
            capture->fence();
            if (silent) return;
            if (mime.empty() || !llvm::json::isUTF8(mime))
                throw std::runtime_error("invalid display MIME type");
            std::lock_guard<std::mutex> lock(owner.mutex);
            Value data = llvm::json::fixUTF8(text);
            // Jupyter application/json data is a JSON value, not JSON text.
            if (mime == "application/json") data = parse(text);
            owner.publish("display_data", parent,
                Object{{"data", Object{{mime, std::move(data)}}}, {"metadata", Object{}}});
        }
        void clear_output() override {
            capture->fence();
            if (silent) return;
            std::lock_guard<std::mutex> lock(owner.mutex);
            owner.publish("clear_output", parent, Object{{"wait", false}});
        }
    };
    struct Input {
        Impl &owner;
        const Request &request;
        Events &events;
        bool allowed;
        static int read(void *context, const char **record, size_t *length) {
            return static_cast<Input *>(context)->read(record, length);
        }
        int read(const char **record, size_t *length) {
            events.capture->fence();
            std::unique_lock<std::mutex> lock(owner.mutex);
            auto p = owner.peers.find(request.peer);
            if (!allowed || owner.stopping || p == owner.peers.end() ||
                    p->second.session.empty()) return 1;
            const auto &m = object(request.message);
            auto prompt = message("stdin", "input_request", owner.session,
                member(m, "header"), Object{{"prompt", ""}, {"password", false}});
            owner.input_id = string(member(object(prompt), "header"), "msg_id");
            owner.input_peer = request.peer;
            owner.input_ready = false;
            owner.input_waiting = true;
            owner.send(request.peer, prompt, request.retention);
            owner.changed.wait(lock, [&] {
                return owner.input_ready || owner.stopping;
            });
            owner.input_waiting = false;
            owner.input_id.clear();
            if (owner.stopping) return 1;
            *record = owner.input_record.data();
            *length = owner.input_record.size();
            return owner.input_status;
        }
    };
    void work() {
        // Evaluator initialization itself is confined to the execution thread.
        EvaluationService service(options);
        for (;;) {
            Request request{0, nullptr};
            {
                std::unique_lock<std::mutex> lock(mutex);
                changed.wait(lock, [&] { return stopping || !queue.empty(); });
                if (stopping) return;
                request = std::move(queue.front());
                queue.pop_front();
                queue_bytes -= request.bytes;
                active = true;
            }
            const auto &m = object(request.message);
            const auto &parent = member(m, "header");
            const auto &content = member(m, "content");
            const bool silent = boolean(content, "silent", false);
            int64_t execution_count;
            {
                std::lock_guard<std::mutex> lock(mutex);
                publish("status", parent, Object{{"execution_state", "busy"}});
                if (!silent && boolean(content, "store_history", true)) {
                    ++count;
                    history.emplace_back(count, string(content, "code"));
                    history_bytes += history.back().second.size();
                    while (history.size() > 100 || history_bytes > 4 * 1024 * 1024) {
                        history_bytes -= history.front().second.size();
                        history.pop_front();
                    }
                }
                execution_count = count;
                if (!silent) publish("execute_input", parent,
                    Object{{"code", string(content, "code")},
                     {"execution_count", execution_count}});
            }
            Events events(*this, parent, silent);
            EvaluationOutcome outcome;
            {
                NativeCapture capture([&](const std::string &name,
                        const std::string &text) { events.captured_stream(name, text); });
                events.capture = &capture;
                Input input{*this, request, events,
                    boolean(content, "allow_stdin", true)};
                EvaluationSettings settings;
                settings.verbose = boolean(content, "lfortran_verbose", false);
                settings.input = Input::read; // NULL would enable libc stdin.
                settings.input_context = &input;
                outcome = service.execute(string(content, "code"), events, settings);
                capture.fence(true);
                if (settings.verbose && outcome.ok) {
                    events.stream("stdout", "AST:\n" + outcome.value.ast +
                        "\nASR:\n" + outcome.value.asr + "\nLLVM IR:\n" +
                        outcome.value.llvm_ir + "\nReturn type: " + outcome.type_name +
                        "\nResult:\n" + (outcome.has_value ? "" : outcome.text + "\n"));
                }
            }
            {
                std::lock_guard<std::mutex> lock(mutex);
                if (outcome.ok) {
                    if (!silent && outcome.has_value) publish("execute_result", parent,
                        Object{{"execution_count", execution_count},
                         {"data", Object{{"text/plain", llvm::json::fixUTF8(outcome.text)}}},
                         {"metadata", Object{}}});
                    reply(request, "execute_reply", Object{{"status", "ok"},
                        {"execution_count", execution_count},
                        {"user_expressions", expression_errors(content)},
                        {"payload", Array{}}});
                } else {
                    auto text = llvm::json::fixUTF8(outcome.exception_report.empty()
                        ? outcome.error_value : outcome.exception_report);
                    Object error{{"ename", outcome.error_name}, {"evalue", text},
                        {"traceback", Array{text}}};
                    if (!silent) publish("error", parent, Object(error));
                    // Full diagnostics are live IOPub output; terminal replies
                    // have a finite, pre-reserved size even for huge errors.
                    if (text.size() > 4096)
                        text = llvm::json::fixUTF8(text.substr(0, 4096)) +
                            "\n[diagnostic truncated; see live error output]";
                    error["ename"] = outcome.error_name.substr(0, 1024);
                    error["evalue"] = text;
                    error["traceback"] = Array{text};
                    error["status"] = "error";
                    error["execution_count"] = execution_count;
                    reply(request, "execute_reply", std::move(error));
                }
                idle(parent, request.peer, request.retention);
                active = false;
                if (!outcome.ok && boolean(content, "stop_on_error", true)) {
                    // Abort the snapshot queued at this failure, across peers;
                    // later submissions are accepted normally.
                    abort_queue();
                }
            }
        }
    }
    void receive(uint64_t peer, const std::string &envelope) {
        std::lock_guard<std::mutex> lock(mutex);
        if (!peers.count(peer)) {
            if (peers.size() >= max_peers)
                throw std::runtime_error("local peer limit reached");
            peers.emplace(peer, Peer{});
        }
        Value value = nullptr;
        const bool handshaking = peers.at(peer).session.empty();
        try {
            if (stopping) throw std::runtime_error("kernel is shutting down");
            value = parse(envelope);
            const auto &m = object(value);
            auto &p = peers.at(peer);
            if (p.session.empty()) {
                if (!p.output.empty()) return;
                auto version = m.getInteger("lfortran_local_protocol");
                if (!version || *version != 1)
                    throw std::runtime_error("unsupported local protocol version");
                p.session = string(m, "client_session");
                if (p.session.empty() || p.session.size() > 1024)
                    throw std::runtime_error("invalid client session");
                if (auto features = m.getArray("features")) {
                    for (const auto &feature : *features) {
                        auto name = feature.getAsString();
                        if (!name) throw std::runtime_error("invalid hello feature");
                        if (*name == "stdin_eof") p.eof = true;
                    }
                }
                send(peer, Object{{"lfortran_local_protocol", 1},
                    {"kernel_session", session}, {"jupyter_protocol", "5.4"},
                    {"features", Array{"stdin", "stdin_eof", "rich_output"}},
                    {"max_frame_bytes", static_cast<int64_t>(max_frame)}});
                if (!announced) {
                    announced = true;
                    publish("status", Object{}, Object{{"execution_state", "starting"}});
                    idle(Object{});
                }
                return; // subscription-ready barrier; no replay
            }
            validate_message(m);
            const auto &h = member(m, "header");
            auto type = string(h, "msg_type");
            auto channel = string(m, "channel");
            auto id = string(h, "msg_id");
            if (string(h, "session") != p.session)
                throw std::runtime_error("header session disagrees with hello");
            if (known_ids.count(id) || outstanding_ids.count(id))
                return; // Never invent a second outcome for accepted work.
            remember(id);
            Request request{peer, value, envelope.size()};
            // References into the moved Value remain valid, but obtain them
            // from its new owner for clarity and portability.
            const auto &request_m = object(request.message);
            const auto &request_c = member(request_m, "content");
            const auto &request_p = member(request_m, "parent_header");
            if (type == "input_reply") {
                if (channel != "stdin" || !input_waiting || input_ready ||
                        input_peer != peer ||
                        optional_string(request_p, "msg_id") != input_id ||
                        optional_string(request_p, "session") != session)
                    throw std::runtime_error("unexpected or wrongly routed input_reply");
                bool eof = boolean(member(request_m, "metadata"),
                    "lfortran_stdin_eof", false);
                if (eof && !p.eof) throw std::runtime_error("stdin EOF not negotiated");
                input_record = string(request_c, "value");
                if (input_record.find_first_of("\r\n") != std::string::npos ||
                        (eof && !input_record.empty()))
                    throw std::runtime_error("input_reply must contain one record without a newline");
                input_status = eof ? -1 : 0;
                input_ready = true;
                changed.notify_all();
                return;
            }
            if (!request_p.empty())
                throw std::runtime_error("request parent_header must be empty");
            if (channel != "shell" && channel != "control")
                throw std::runtime_error("request on invalid channel");
            if (type == "shutdown_request") {
                bool restart = boolean(request_c, "restart", false);
                if (restart) throw std::runtime_error("kernel restart is not supported");
                reply(request, "shutdown_reply", Object{{"restart", false}});
                abort_queue();
                stopping = true;
                input_status = 1;
                input_ready = true;
                changed.notify_all();
            } else if (type == "kernel_info_request") {
                immediate_reply(request, "kernel_info_reply", Object{{"status", "ok"},
                    {"protocol_version", "5.4"}, {"implementation", "lfortran"},
                    {"implementation_version", std::string(LFORTRAN_VERSION)},
                    {"language_info", Object{{"name", "fortran"}, {"version", "2023"},
                        {"mimetype", "text/x-fortran"}, {"file_extension", ".f90"}}},
                    {"banner", "Interactive Fortran. Experimental prototype, not ready for end users.\n"
                        "LFortran version: " + std::string(LFORTRAN_VERSION)},
                    {"help_links", Array{}}});
            } else if (type == "is_complete_request") {
                auto done = complete(string(request_c, "code"));
                immediate_reply(request, "is_complete_reply",
                    Object{{"status", done ? "complete" : "incomplete"}, {"indent", ""}});
            } else if (type == "complete_request") {
                auto pos = request_c.getInteger("cursor_pos");
                auto code = string(request_c, "code");
                size_t characters = 0;
                for (unsigned char byte : code)
                    characters += (byte & 0xc0) != 0x80;
                if (!pos || *pos < 0 || static_cast<uint64_t>(*pos) > characters)
                    throw std::runtime_error("invalid cursor_pos");
                immediate_reply(request, "complete_reply", Object{{"status", "ok"},
                    {"matches", Array{}}, {"cursor_start", *pos}, {"cursor_end", *pos},
                    {"metadata", Object{}}});
            } else if (type == "inspect_request") {
                immediate_reply(request, "inspect_reply", Object{{"status", "ok"}, {"found", false},
                    {"data", Object{}}, {"metadata", Object{}}});
            } else if (type == "comm_info_request") {
                immediate_reply(request, "comm_info_reply",
                    Object{{"status", "ok"}, {"comms", Object{}}});
            } else if (type == "comm_open" || type == "comm_msg" || type == "comm_close") {
                return; // No comm targets are implemented.
            } else if (type == "history_request") {
                Array entries;
                auto n = request_c.getInteger("n");
                if (optional_string(request_c, "hist_access_type") == "tail" &&
                        !boolean(request_c, "output", false)) {
                    const size_t size = n && *n > 0 ?
                        std::min<size_t>(static_cast<size_t>(*n), history.size()) : 0;
                    for (size_t i = history.size()-size; i < history.size(); ++i)
                        entries.push_back(Array{0, history[i].first, history[i].second});
                }
                immediate_reply(request, "history_reply", Object{{"history", std::move(entries)}});
            } else if (type == "execute_request") {
                if (channel != "shell") throw std::runtime_error("execute requires shell");
                string(request_c, "code");
                for (auto flag : {"silent", "store_history", "allow_stdin",
                        "stop_on_error", "lfortran_verbose"})
                    boolean(request_c, flag, true);
                if (request_c.find("user_expressions") != request_c.end()) {
                    for (const auto &entry : member(request_c, "user_expressions"))
                        if (!entry.second.getAsString())
                            throw std::runtime_error("user_expressions values must be strings");
                }
                if (queue.size() >= max_requests ||
                        queue_bytes + envelope.size() > max_output) {
                    immediate_reply(request, "execute_reply", Object{
                        {"status", "error"}, {"ename", "QueueFull"},
                        {"evalue", "execution queue is full; cell not executed"},
                        {"traceback", Array{}}, {"execution_count", count}});
                    return;
                }
                const size_t retained = 128 * 1024 +
                    3 * encode(Object(h)).size() +
                    encode(expression_errors(request_c)).size();
                if (retained > max_frame) {
                    immediate_reply(request, "execute_reply", Object{
                        {"status", "error"}, {"ename", "RequestTooLarge"},
                        {"evalue", "completion exceeds delivery budget; cell not executed"},
                        {"traceback", Array{}}, {"execution_count", count}});
                    return;
                }
                request.retention = reserve(peer, retained);
                if (!request.retention) return; // Unadmitted flood: no mutation.
                queue_bytes += envelope.size();
                outstanding_ids.insert(id);
                queue.push_back(std::move(request));
                changed.notify_all();
            } else {
                throw std::runtime_error("unsupported request type: " + type);
            }
        } catch (const std::exception &error) {
            if (handshaking) {
                fail_peer(peer, error.what());
                return;
            }
            try {
                const auto &m = object(value);
                const auto &h = member(m, "header");
                auto type = string(h, "msg_type");
                auto channel = string(m, "channel");
                if (string(h, "session") == peers.at(peer).session &&
                        (channel == "shell" || channel == "control") &&
                        type.size() >= 8 && type.substr(type.size()-8) == "_request") {
                    Object routable{{"channel", channel}, {"header", Object(h)},
                        {"parent_header", Object{}}, {"metadata", Object{}},
                        {"content", Object{}}, {"buffers", Array{}}};
                    validate_message(routable);
                    Request rejected{peer, std::move(routable)};
                    immediate_reply(rejected, type.substr(0, type.size()-8) + "_reply",
                        Object{{"status", "error"}, {"ename", "InvalidRequest"},
                            {"evalue", error.what()}, {"traceback", Array{}},
                            {"execution_count", count}});
                }
            } catch (const std::exception &) {} // No trustworthy reply header.
        }
    }
};
NativeSession::NativeSession(CompilerOptions options)
    : impl(new Impl(std::move(options))) {}
NativeSession::~NativeSession() = default;
const std::string &NativeSession::id() const { return impl->session; }
void NativeSession::receive(uint64_t peer, const std::string &value) {
    impl->receive(peer, value);
}
void NativeSession::disconnect(uint64_t peer) {
    std::lock_guard<std::mutex> lock(impl->mutex);
    impl->fail_peer(peer, "peer disconnected");
    for (auto i = impl->queue.begin(); i != impl->queue.end();) {
        if (i->peer != peer) { ++i; continue; }
        impl->queue_bytes -= i->bytes;
        const auto id = string(member(object(i->message), "header"), "msg_id");
        impl->outstanding_ids.erase(id);
        impl->remember(id);
        i = impl->queue.erase(i);
    }
    impl->peers.erase(peer);
}
std::vector<SessionDelivery> NativeSession::take_deliveries() {
    std::lock_guard<std::mutex> lock(impl->mutex);
    std::vector<SessionDelivery> result;
    for (auto &p : impl->peers) {
        while (!p.second.output.empty()) {
            result.push_back(std::move(p.second.output.front()));
            p.second.output.pop_front();
        }
        p.second.bytes = 0;
    }
    return result;
}
bool NativeSession::shutdown_requested() const {
    std::lock_guard<std::mutex> lock(impl->mutex);
    return impl->stopping;
}
bool NativeSession::executing() const {
    std::lock_guard<std::mutex> lock(impl->mutex);
    return impl->active;
}
size_t NativeSession::peer_count() const {
    std::lock_guard<std::mutex> lock(impl->mutex);
    size_t count = 0;
    for (const auto &peer : impl->peers) count += !peer.second.session.empty();
    return count;
}
}
