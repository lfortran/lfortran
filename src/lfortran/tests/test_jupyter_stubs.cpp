#include <lfortran/jupyter.h>
#include <libasr/config.h>
#include <stdexcept>

using LCompilers::LFortran::jupyter::Client;

// Keep references to every platform stub, including methods not called by
// JupyterLite today: a declaration-only stub must fail at link time.
bool (Client::*volatile execute_cell)(const std::string &, bool) = &Client::execute;
void (Client::*volatile show_cells)(const std::function<void()> &) = &Client::show_others;
int (Client::*volatile status)(int) = &Client::kernel_status;
bool (*volatile parent)(long long, const std::string &)
    = &LCompilers::LFortran::jupyter::stop_with_parent;
void (*volatile started)() = &LCompilers::LFortran::jupyter::kernel_started;
void (*volatile interrupt)() = &LCompilers::LFortran::jupyter::handle_interrupt;
#ifndef HAVE_LFORTRAN_XEUS
int (*volatile kernel)(const std::string &, LCompilers::CompilerOptions &, bool)
    = &LCompilers::LFortran::run_kernel;
#endif

int main()
{
    try {
        Client client("unused");
        return 1;
    } catch (const std::runtime_error &) {
    }
    std::string connection;
    try {
        Client::start("unused", {}, connection);
        return 2;
    } catch (const std::runtime_error &) {
    }
    return 0;
}
