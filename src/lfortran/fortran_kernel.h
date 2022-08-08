#ifndef LFORTRAN_FORTRAN_KERNEL_H
#define LFORTRAN_FORTRAN_KERNEL_H

#include <libasr/config.h>
#include <lfortran/fortran_evaluator.h>

#include <xeus/xinterpreter.hpp>


namespace LFortran {

    class custom_interpreter : public xeus::xinterpreter
    {
    private:
        FortranEvaluator e;

    public:
        custom_interpreter() : e{CompilerOptions()} {}
        virtual ~custom_interpreter() = default;

    private:

        void configure_impl() override;

        nl::json execute_request_impl(int execution_counter,
                                      const std::string& code,
                                      bool silent,
                                      bool store_history,
                                      nl::json user_expressions,
                                      bool allow_stdin) override;

        nl::json complete_request_impl(const std::string& code,
                                       int cursor_pos) override;

        nl::json inspect_request_impl(const std::string& code,
                                      int cursor_pos,
                                      int detail_level) override;

        nl::json is_complete_request_impl(const std::string& code) override;

        nl::json kernel_info_request_impl() override;

        void shutdown_request_impl() override;
    };

#ifdef HAVE_LFORTRAN_XEUS
    int run_kernel(const std::string &connection_filename);
#endif

}

#endif // LFORTRAN_FORTRAN_KERNEL_H
