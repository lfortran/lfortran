#ifndef LFORTRAN_SESSION_TERMINAL_H
#define LFORTRAN_SESSION_TERMINAL_H
#include <lfortran/native_session.h>
#include <cpp-terminal/prompt0.h>
#include <chrono>
#include <iostream>
#include <set>

namespace LCompilers::LFortran {
namespace {
#ifndef _WIN32
class InputEcho {
    termios original{};
    bool changed = false;
public:
    explicit InputEcho(bool password) {
        if (password && isatty(STDIN_FILENO)) {
            if (tcgetattr(STDIN_FILENO, &original))
                throw std::runtime_error("cannot read password terminal mode");
            auto mode = original;
            mode.c_lflag &= ~ECHO;
            if (tcsetattr(STDIN_FILENO, TCSANOW, &mode))
                throw std::runtime_error("cannot disable password echo");
            changed = true;
        }
    }
    ~InputEcho() {
        if (changed) tcsetattr(STDIN_FILENO, TCSANOW, &original);
    }
};
#endif
// The next byte of stdin that is not a console, or EOF. On Windows the
// CRT's text-mode stdin waits for the byte after a CR, and keeps bytes in its
// buffer where session_input_available() cannot see them, so read the handle.
int read_byte() {
#ifdef _WIN32
    char c;
    DWORD n = 0;
    if (!ReadFile(GetStdHandle(STD_INPUT_HANDLE), &c, 1, &n, nullptr) || n != 1)
        return EOF;
    return static_cast<unsigned char>(c);
#else
    return std::getchar();
#endif
}
class SafeTerminalText {
    enum class State { text, escape, csi, osc, osc_escape } state = State::text;
public:
    std::string filter(const std::string &input) {
        std::string output;
        for (unsigned char c : input) {
            switch (state) {
                case State::text:
                    if (c == 27) state = State::escape;
                    else if (c == '\n' || c == '\t' || c >= 32) {
                        if (c != 127) output += static_cast<char>(c);
                    }
                    break;
                case State::escape:
                    state = c == '[' ? State::csi : c == ']' ? State::osc : State::text;
                    break;
                case State::csi:
                    if (c >= 0x40 && c <= 0x7e) state = State::text;
                    break;
                case State::osc:
                    if (c == 7) state = State::text;
                    else if (c == 27) state = State::osc_escape;
                    break;
                case State::osc_escape:
                    state = c == '\\' ? State::text : State::osc;
                    break;
            }
        }
        return output;
    }
};
class TerminalSession {
    SessionClient &client;
    SafeTerminalText safe;
    std::string waiting, completeness, info, banner;
    bool reply = false, idle = false, complete_ready = false, is_complete = true;
    bool info_ready = false;
    bool completeness_notice = false, published_error = false;
    std::string reply_error, reply_status;
    std::string rendered;
    std::vector<std::pair<bool, std::string>> rendered_parts;

    void append(std::string text, bool error = false) {
        if (text.empty()) return;
        rendered += text;
        rendered_parts.emplace_back(error, std::move(text));
    }

    void input(const ClientEvent &event) {
        // Editing's raw-mode scope has ended before execution starts. READ
        // uses canonical terminal input, including EOF, without code history.
        Terminal terminal(false);
#ifndef _WIN32
        InputEcho echo(event.password);
#endif
        std::cout << safe.filter(event.prompt) << std::flush;
        std::string record;
        bool eof = false;
#ifdef _WIN32
        HANDLE console = GetStdHandle(STD_INPUT_HANDLE);
        DWORD mode;
        if (GetConsoleMode(console, &mode)) {
            // Read console events rather than a blocking canonical ReadFile:
            // shutdown/output must still be serviced before Enter is typed.
            // The console's line/echo mode itself is left unchanged.
            std::wstring line;
            bool done = false;
            while (!done) {
                DWORD count = 0;
                if (!GetNumberOfConsoleInputEvents(console, &count))
                    throw std::runtime_error("cannot query console input");
                if (!count) { pump(10); flush(); continue; }
                INPUT_RECORD key{};
                if (!ReadConsoleInputW(console, &key, 1, &count))
                    throw std::runtime_error("cannot read console input");
                if (!count || key.EventType != KEY_EVENT || !key.Event.KeyEvent.bKeyDown)
                    continue;
                auto &press = key.Event.KeyEvent;
                for (WORD i = 0; i < press.wRepeatCount && !done; ++i) {
                    wchar_t ch = press.uChar.UnicodeChar;
                    if (ch == L'\r' || ch == L'\n') done = true;
                    else if (ch == 4 || ch == 26) { eof = line.empty(); done = true; }
                    else if (ch == L'\b') {
                        if (!line.empty()) {
                            auto last = line.back();
                            line.pop_back();
                            if (last >= 0xdc00 && last <= 0xdfff && !line.empty())
                                line.pop_back();
                            if (!event.password) std::cout << "\b \b" << std::flush;
                        }
                    } else if (ch >= 32 || ch == L'\t') {
                        line.push_back(ch);
                        if (!event.password) {
                            DWORD written;
                            WriteConsoleW(GetStdHandle(STD_OUTPUT_HANDLE), &ch, 1,
                                &written, nullptr);
                        }
                    }
                }
                pump(0);
                flush();
            }
            std::cout << '\n' << std::flush;
            int size = WideCharToMultiByte(CP_UTF8, 0, line.data(),
                static_cast<int>(line.size()), nullptr, 0, nullptr, nullptr);
            record.resize(size);
            if (size) WideCharToMultiByte(CP_UTF8, 0, line.data(),
                static_cast<int>(line.size()), record.data(), size, nullptr, nullptr);
            client.input_reply(event, record, eof);
            return;
        }
#endif
        for (;;) {
            if (session_input_available()) {
                int c = read_byte();
                if (c == EOF || c == 4) {
                    eof = record.empty();
                    clearerr(stdin);
                    break;
                }
                if (c == '\n' || c == '\r') break;
                record += static_cast<char>(c);
            } else {
                // Continue to detect kernel death/control shutdown while the
                // user's canonical line is not ready.
                pump(10);
                flush();
            }
        }
        if (event.password) std::cout << '\n' << std::flush;
        client.input_reply(event, record, eof);
    }
    void dispatch(const ClientEvent &event) {
        const bool own = event.parent_session == client.session_id();
        if (own && event.parent_id == info && event.type == "kernel_info_reply") {
            banner = safe.filter(event.text);
            info_ready = true;
        }
        if (own && event.parent_id == waiting && event.type == "execute_reply") {
            reply = true;
            reply_error = event.text;
            reply_status = event.status;
        }
        if (own && event.parent_id == waiting && event.type == "error")
            published_error = true;
        if (own && event.parent_id == waiting && event.type == "status" &&
                event.status == "idle") idle = true;
        if (own && event.parent_id == completeness && event.type == "is_complete_reply") {
            complete_ready = true;
            is_complete = event.status != "incomplete";
        }
        if (event.type == "input_request") {
            if (!own || event.parent_id != waiting)
                throw std::runtime_error("unexpected input request at terminal");
            flush();
            input(event);
        } else if (event.type == "execute_input" &&
                event.parent_session != client.session_id()) {
            auto code = safe.filter(event.code);
            if (code.empty() || code.back() != '\n') code += '\n';
            append("[other client]\n" + code);
        } else if (event.type == "stream" || event.type == "execute_result" ||
                event.type == "error" || event.type == "display_data") {
            auto text = safe.filter(event.text);
            if ((event.type == "execute_result" || event.type == "error" ||
                    event.type == "display_data") &&
                    !text.empty() && text.back() != '\n') text += '\n';
            append(std::move(text), event.type == "error" ||
                (event.type == "stream" && event.stream_name == "stderr"));
        } else if (event.type == "clear_output") {
            // Do not let a remote kernel erase the user's editor/history.
            append("[output cleared]\n");
        }
    }
    void flush() {
        for (const auto &part : rendered_parts)
            (part.first ? std::cerr : std::cout) << part.second << std::flush;
        rendered_parts.clear();
        rendered.clear();
    }
public:
    explicit TerminalSession(SessionClient &client) : client(client) {}
    void pump(int timeout) {
        ClientEvent event;
        if (client.next(event, timeout)) dispatch(event);
    }
    int run(bool verbose) {
        setvbuf(stdin, nullptr, _IONBF, 0);
        info = client.request("kernel_info_request", "{}");
        while (!info_ready) pump(100);
        std::cout << banner << "\n"
            << "  * Use Ctrl-D to exit\n"
            << "  * Use Enter to submit; Alt-Enter or Ctrl-N for a new line\n"
            << "  * Editing: arrows, Home, End, Backspace, Delete; history: Up, Down\n";
        flush();
        std::vector<std::string> history;
        std::function<bool(std::string)> complete = [&](std::string code) {
            complete_ready = false;
            completeness = client.request("is_complete_request",
                "{\"code\":" + json_string(code) + "}");
            auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(1);
            while (!complete_ready && std::chrono::steady_clock::now() < deadline) pump(100);
            // is_complete is optional in classic Jupyter. Missing assistance
            // must not prevent submission or invoke a local language parser.
            if (!complete_ready) {
                if (!completeness_notice) {
                    append("kernel did not answer is_complete; submitting without "
                        "completeness assistance\n", true);
                    completeness_notice = true;
                }
                return true;
            }
            return is_complete;
        };
        for (;;) {
            std::string code;
            {
                Terminal terminal(true, false);
                code = prompt0(terminal, ">>> ", history, complete, [&] {
                    pump(session_input_available() ? 0 : 10);
                    return rendered;
                }, session_input_available, [&] { flush(); });
            }
            flush();
            if (code.size() == 1 && code[0] == 4) {
                std::cout << "\nExiting.\n";
                return 0;
            }
            reply = idle = false;
            published_error = false;
            reply_error.clear();
            reply_status.clear();
            waiting = client.request("execute_request", "{\"code\":" + json_string(code) +
                ",\"silent\":false,\"store_history\":true,\"allow_stdin\":true,"
                "\"stop_on_error\":true,\"user_expressions\":{},\"lfortran_verbose\":" +
                (verbose ? "true" : "false") + "}");
            while (!reply || !idle) { pump(100); flush(); }
            if (reply_status == "aborted")
                append("cell aborted; not executed\n", true);
            else if (reply_status != "ok" && reply_status != "error")
                append("invalid execute reply status; execution outcome unknown\n", true);
            else if (reply_status == "error" && !published_error)
                append(reply_error.empty() ? "cell failed\n" :
                    safe.filter(reply_error) + "\n", true);
            flush();
            waiting.clear();
        }
    }
};
}
inline int session_terminal(SessionClient &client, bool verbose) {
    return TerminalSession(client).run(verbose);
}
}
#endif
