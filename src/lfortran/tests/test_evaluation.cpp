#include <lfortran/evaluation.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <vector>

using namespace LCompilers;
using namespace LCompilers::LFortran;

static void require(bool condition, const std::string &message) {
    if (!condition) throw std::runtime_error(message);
}

struct Events : EvaluationEvents {
    std::string output, mime, data;
    int clears = 0;
    void stream(const std::string &, const std::string &text) override { output += text; }
    void display(const std::string &type, const std::string &text) override {
        mime = type; data = text;
    }
    void clear_output() override { clears++; }
};

struct Input {
    std::vector<std::string> records;
    size_t next = 0;
    int calls = 0;
    int end = -1;
    static int read(void *context, const char **data, size_t *length) noexcept {
        auto &input = *static_cast<Input *>(context);
        input.calls++;
        if (input.next == input.records.size()) return input.end;
        auto &record = input.records[input.next++];
        *data = record.data();
        *length = record.size();
        return 0;
    }
};

static CompilerOptions options() {
    CompilerOptions options;
    options.use_colors = false;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    return options;
}

static EvaluationOutcome run(EvaluationService &service, const std::string &code,
        Events &events, Input *input = nullptr) {
    EvaluationSettings settings;
    if (input) {
        settings.input = Input::read;
        settings.input_context = input;
    }
    auto outcome = service.execute(code, events, settings);
    require(outcome.ok, code + "\n" + outcome.diagnostics + outcome.exception_report);
    return outcome;
}

static void check_input(const std::string &code,
        std::vector<std::string> records, int expected_calls = -1) {
    EvaluationService service(options());
    Events events;
    Input input{std::move(records)};
    run(service, code, events, &input);
    require(input.calls == (expected_calls < 0 ? (int)input.records.size() : expected_calls),
        "unexpected number of input records requested: " + std::to_string(input.calls));
}

static void results_and_cells() {
    EvaluationService service(options());
    Events events;
    const std::vector<std::pair<std::string, std::string>> examples = {
        {"42", "42"}, {"42_8", "42"}, {"1.25", "1.25"},
        {"1.2345678901234567_8", "1.2345678901234567"},
        {"(1.25, -2.5)", "(1.25, -2.5)"},
        {"(1.25_8, -2.5_8)", "(1.25, -2.5)"},
        {".true.", "True"}, {".false.", "False"}, {"'hello'", "hello"}
    };
    for (auto &example : examples) {
        auto outcome = run(service, example.first, events);
        require(outcome.has_value && outcome.text == example.second, example.first);
    }
    require(!run(service, "integer :: n\nn = 41", events).has_value, "statement result");
    require(run(service, "n+1", events).text == "42", "persistent symbols");
    require(!run(service, "", events).has_value, "empty cell");
    auto error = service.execute("integer :: broken(", events);
    require(!error.ok && error.error_name == "CompilerError" &&
        error.diagnostics.find("<cell ") != std::string::npos, "cell diagnostics");
    require(run(service, "n", events).text == "41", "state after parse error");
    for (const std::string magic : {"ast", "asr", "llvm", "asm", "cpp", "fmt"}) {
        EvaluationService inspection(options());
        events.output.clear();
        run(inspection, "%%show" + magic + "\ninteger :: x", events);
        require(!events.output.empty(), magic + " inspection");
    }
}

static void input_records() {
    check_input(R"(
integer :: a, b, c, u, ios
u = 5
read(*,*,iostat=ios) a,b
if (ios /= 0 .or. a /= 11 .or. b /= 12) error stop
read(u,*,iostat=ios) c
if (ios /= 0 .or. c /= 13) error stop
read(*,*)
read(5,*,iostat=ios) a
if (ios /= 0 .or. a /= 14) error stop
)", {"11 12 ignored", "13", "", "14"});
    check_input(R"(
integer :: a(3), ios
real :: r(2)
complex :: c(2), z
complex(8) :: d(2)
read(*,*,iostat=ios) a
if (ios /= 0 .or. any(a /= [1,2,3])) error stop
read(5,*,iostat=ios) r
if (ios /= 0 .or. any(abs(r-[1.5,2.5]) > 1e-6)) error stop
read(*,*,iostat=ios) c
if (ios /= 0 .or. any(abs(c-[(1.,2.),(3.,4.)]) > 1e-6)) error stop
read(5,*,iostat=ios) d
if (ios /= 0 .or. any(abs(d-[(5.,6.),(7.,8.)]) > 1e-6)) error stop
read(*,*,iostat=ios) z
if (ios /= 0 .or. abs(z-(9.,10.)) > 1e-6) error stop
)", {"1,2", "3", "1.5 2.5", "(1,2), ( 3, 4 )", "(5,6) (7,8)", "(9,10)"});
    check_input(R"(
character(4) :: a
character(2) :: b
integer :: i,j,k,ios
read(*,'(a4)',iostat=ios) a
if (ios /= 0 .or. a /= '    ') error stop
read(5,'(a4)',iostat=ios) a
if (ios /= 0 .or. a /= 'text') error stop
read(*,'(i2,/,i2)',iostat=ios) i,j
if (ios /= 0 .or. i /= 21 .or. j /= 22) error stop
read(5,'(i2)',iostat=ios) i,j,k
if (ios /= 0 .or. i /= 31 .or. j /= 32 .or. k /= 33) error stop
read(5,'(a2)',advance='no',iostat=ios) b
if (ios /= 0 .or. b /= 'ab') error stop
read(*,'(a2)',iostat=ios) b
if (ios /= 0 .or. b /= 'cd') error stop
)", {"", "text", "21", "22", "31", "32", "33", "abcd"});
    check_input(R"(
program input_namelist
integer :: a,ios
namelist /values/ a
read(5,nml=values,iostat=ios)
if (ios /= 0 .or. a /= 57) error stop
read(*,nml=values,iostat=ios)
if (ios /= 0 .or. a /= 58) error stop
end program
)", {"&values", "a=57 /", "&values a=58 /"});
    check_input(R"(
integer(1) :: a(2)
integer(2) :: b(2)
integer(8) :: c(2)
real :: r(2)
real(8) :: d(2)
logical :: l(2)
character(3) :: t(2)
integer :: ios
read(*,*,iostat=ios) a
if (ios /= 0 .or. any(a /= [1,2])) error stop
read(5,*,iostat=ios) b
if (ios /= 0 .or. any(b /= [3,4])) error stop
read(*,*,iostat=ios) c
if (ios /= 0 .or. any(c /= [5,6])) error stop
read(*,*,iostat=ios) r
if (ios /= 0 .or. any(abs(r-[1.5,2.5]) > 1e-6)) error stop
read(*,*,iostat=ios) d
if (ios /= 0 .or. any(abs(d-[3.5,4.5]) > 1e-6)) error stop
read(*,*,iostat=ios) l
if (ios /= 0 .or. .not. l(1) .or. l(2)) error stop
read(*,*,iostat=ios) t
if (ios /= 0 .or. t(1) /= 'abc' .or. t(2) /= 'def') error stop
)", {"1 2", "3 4", "5 6", "1.5,2.5", "3.5,4.5", "T F", "'abc' 'def'"});
    check_input(R"(
character(2000) :: text
integer :: ios
read(5,'(a2000)',advance='no',iostat=ios) text
if (ios /= 0 .or. text /= repeat('x',2000)) error stop
read(*,'(a2000)',iostat=ios) text
if (ios /= 0 .or. text /= repeat('x',2000)) error stop
)", {std::string(4000, 'x')});
    check_input(R"(
character(2) :: a,b,c
integer :: ios
read(5,'(a2,t1,a2,tl1,a2)',iostat=ios) a,b,c
if (ios /= 0 .or. a /= 'ab' .or. b /= 'ab' .or. c /= 'bc') error stop
read(*,'(t2,a2)',iostat=ios) a
if (ios /= 0 .or. a /= 'bc') error stop
read(5,'(tl1,a2)',iostat=ios) a
if (ios /= 0 .or. a /= 'ab') error stop
)", {"abcd", "abcd", "abcd"});
}

static void eof_and_reset() {
    check_input(R"(
program input_eof
integer :: a(2),ios
complex :: z
character(3) :: text
namelist /values/ a
read(*,'(a3)',iostat=ios) text
if (ios /= -1) error stop 'formatted EOF'
read(5,*,iostat=ios) a
if (ios /= -1) error stop 'array EOF'
read(*,*,iostat=ios) z
if (ios /= -1) error stop 'complex EOF'
read(5,nml=values,iostat=ios)
if (ios /= -1) error stop 'namelist EOF'
read(*,*,iostat=ios)
if (ios /= -1) error stop 'empty read EOF'
end program
)", {}, 1);
    EvaluationService service(options());
    Events events;
    Input eof;
    run(service, R"(
integer :: a,ios
read(*,*,iostat=ios) a
if (ios /= -1) error stop
read(5,*,iostat=ios) a
if (ios /= -1) error stop
)", events, &eof);
    require(eof.calls == 1, "EOF must remain latched during execution");
    Input fresh{{"42"}};
    run(service, "read(5,*,iostat=ios) a\nif (ios /= 0 .or. a /= 42) error stop",
        events, &fresh);
    Input disconnected;
    disconnected.end = 1;
    run(service, R"(
read(*,*,iostat=ios) a
if (ios <= 0) error stop
read(5,'(i2)',iostat=ios) a
if (ios <= 0) error stop
)", events, &disconnected);
    require(disconnected.calls == 1, "unavailable input must remain latched");
    EvaluationSettings throwing;
    throwing.input = [](void *, const char **, size_t *) -> int {
        throw std::runtime_error("input callback failure");
    };
    auto caught = service.execute("read(*,*,iostat=ios) a\nif (ios <= 0) error stop",
        events, throwing);
    require(caught.ok, "input exceptions must not cross generated code");
    run(service, "a+1", events);
}

static void file_isolation() {
    EvaluationService service(options());
    Events events;
    Input forbidden;
    forbidden.end = 1;
    std::ifstream file(std::string(LFORTRAN_PROJECT_SOURCE_DIR) +
        "/integration_tests/runtime_input_records.f90");
    require(file.good(), "compiled-program regression source");
    std::ostringstream source;
    source << file.rdbuf();
    run(service, source.str(), events, &forbidden);
    run(service, R"(
program unit5_named_file
integer :: u,i,ios
character(3) :: r
u=5
open(unit=u,file='test_evaluation_unit5.dat',status='replace')
write(u,'(a)') '123'
rewind(u)
read(u,*,iostat=ios) i
if (ios /= 0 .or. i /= 123) error stop
close(u,status='delete')
r='456'
read(r,*,iostat=ios) i
if (ios /= 0 .or. i /= 456) error stop
end program
)", events, &forbidden);
    require(forbidden.calls == 0, "named/internal files must never call provider");
}

static void display_events() {
    EvaluationService service(options());
    Events events;
    run(service, R"(
use lfortran_display, only: display_data, clear_output
call display_data('text/html', '<b>hello</b>')
call clear_output()
)", events);
    require(events.mime == "text/html" && events.data == "<b>hello</b>" &&
        events.clears == 1, "display C ABI");
    lfortran_display_data("text/plain", "outside execution");
    lfortran_clear_output();
    require(events.clears == 1, "event sink must be execution scoped");
    class ThrowingEvents : public Events {
        void clear_output() override { throw std::runtime_error("display failure"); }
    } throwing;
    auto failed = service.execute("call clear_output()", throwing);
    require(!failed.ok && failed.error_name == "DisplayError",
        "display exceptions must not cross generated code");
}

int main() {
    try {
        results_and_cells();
        input_records();
        eof_and_reset();
        file_isolation();
        display_events();
        std::cout << "shared evaluation and runtime input checks passed\n";
        return 0;
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
