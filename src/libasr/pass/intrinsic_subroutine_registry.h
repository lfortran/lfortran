#ifndef LFORTRAN_PASS_INTRINSIC_SUBROUTINE_REGISTRY_H
#define LFORTRAN_PASS_INTRINSIC_SUBROUTINE_REGISTRY_H

// #include <libasr/pass/intrinsic_function_registry_util.h>
#include <libasr/pass/intrinsic_subroutines.h>

#include <cmath>
#include <string>
#include <tuple>

namespace LCompilers {

namespace ASRUtils {

#define INTRINSIC_SUBROUTINE_NAME_CASE(X)                                                  \
    case (static_cast<int64_t>(ASRUtils::IntrinsicImpureSubroutines::X)) : {   \
        return #X;                                                              \
    }

inline std::string get_intrinsic_subroutine_name(int x) {
    switch (x) {
        INTRINSIC_SUBROUTINE_NAME_CASE(RandomNumber)
        INTRINSIC_SUBROUTINE_NAME_CASE(RandomInit)
        INTRINSIC_SUBROUTINE_NAME_CASE(RandomSeed)
        INTRINSIC_SUBROUTINE_NAME_CASE(GetCommand)
        INTRINSIC_SUBROUTINE_NAME_CASE(GetCommandArgument)
        INTRINSIC_SUBROUTINE_NAME_CASE(GetEnvironmentVariable)
        INTRINSIC_SUBROUTINE_NAME_CASE(ExecuteCommandLine)
        INTRINSIC_SUBROUTINE_NAME_CASE(CpuTime)
        INTRINSIC_SUBROUTINE_NAME_CASE(Srand)
        INTRINSIC_SUBROUTINE_NAME_CASE(SystemClock)
        INTRINSIC_SUBROUTINE_NAME_CASE(DateAndTime)
        INTRINSIC_SUBROUTINE_NAME_CASE(MoveAlloc)
        INTRINSIC_SUBROUTINE_NAME_CASE(Mvbits)
        INTRINSIC_SUBROUTINE_NAME_CASE(Abort)
        INTRINSIC_SUBROUTINE_NAME_CASE(System)
        INTRINSIC_SUBROUTINE_NAME_CASE(Sleep)
        default : {
            throw LCompilersException("pickle: intrinsic_id not implemented");
        }
    }
}

/************************* Intrinsic Impure Subroutine **************************/

namespace IntrinsicImpureSubroutineRegistry {

    inline const std::map<int64_t,
        std::tuple<impl_subroutine,
                   verify_subroutine>>& get_intrinsic_subroutine_by_id_db() {
        static const std::map<int64_t,
            std::tuple<impl_subroutine,
                       verify_subroutine>> intrinsic_subroutine_by_id_db = {
        {static_cast<int64_t>(IntrinsicImpureSubroutines::RandomNumber),
            {&RandomNumber::instantiate_RandomNumber, &RandomNumber::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::RandomInit),
            {&RandomInit::instantiate_RandomInit, &RandomInit::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::RandomSeed),
            {&RandomSeed::instantiate_RandomSeed, &RandomSeed::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::Srand),
            {&Srand::instantiate_Srand, &Srand::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::GetCommand),
            {&GetCommand::instantiate_GetCommand, &GetCommand::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::GetCommandArgument),
            {&GetCommandArgument::instantiate_GetCommandArgument, &GetCommandArgument::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::SystemClock),
            {&SystemClock::instantiate_SystemClock, &SystemClock::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::DateAndTime),
            {&DateAndTime::instantiate_DateAndTime, &DateAndTime::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::GetEnvironmentVariable),
            {&GetEnvironmentVariable::instantiate_GetEnvironmentVariable, &GetEnvironmentVariable::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::ExecuteCommandLine),
            {&ExecuteCommandLine::instantiate_ExecuteCommandLine, &ExecuteCommandLine::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::CpuTime),
            {&CpuTime::instantiate_CpuTime, &CpuTime::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::MoveAlloc),
            {&MoveAlloc::instantiate_MoveAlloc, &MoveAlloc::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::Abort),
            {&Abort::instantiate_Abort, &Abort::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::Mvbits),
            {&Mvbits::instantiate_Mvbits, &Mvbits::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::System),
            {&System::instantiate_System, &System::verify_args}},
        {static_cast<int64_t>(IntrinsicImpureSubroutines::Sleep),
            {&Sleep::instantiate_Sleep, &Sleep::verify_args}},
        };
        return intrinsic_subroutine_by_id_db;
    }

    inline const std::map<std::string,
        create_intrinsic_subroutine>& get_intrinsic_subroutine_by_name_db() {
        static const std::map<std::string,
            create_intrinsic_subroutine> intrinsic_subroutine_by_name_db = {
                {"random_number", &RandomNumber::create_RandomNumber},
                {"random_init", &RandomInit::create_RandomInit},
                {"random_seed", &RandomSeed::create_RandomSeed},
                {"srand", &Srand::create_Srand},
                {"get_command", &GetCommand::create_GetCommand},
                {"get_command_argument", &GetCommandArgument::create_GetCommandArgument},
                {"system_clock", &SystemClock::create_SystemClock},
                {"get_environment_variable", &GetEnvironmentVariable::create_GetEnvironmentVariable},
                {"execute_command_line", &ExecuteCommandLine::create_ExecuteCommandLine},
                {"cpu_time", &CpuTime::create_CpuTime},
                {"date_and_time", &DateAndTime::create_DateAndTime},
                {"move_alloc", &MoveAlloc::create_MoveAlloc},
                {"mvbits", &Mvbits::create_Mvbits},
                {"abort", &Abort::create_Abort},
                {"system", &System::create_System},
                {"sleep", &Sleep::create_Sleep},
        };
        return intrinsic_subroutine_by_name_db;
    }

    static inline bool is_intrinsic_subroutine(const std::string& name) {
        return get_intrinsic_subroutine_by_name_db().find(name) != get_intrinsic_subroutine_by_name_db().end();
    }

    static inline bool is_intrinsic_subroutine(int64_t id) {
        return get_intrinsic_subroutine_by_id_db().find(id) != get_intrinsic_subroutine_by_id_db().end();
    }

    static inline create_intrinsic_subroutine get_create_subroutine(const std::string& name) {
        return  get_intrinsic_subroutine_by_name_db().at(name);
    }

    static inline verify_subroutine get_verify_subroutine(int64_t id) {
        return std::get<1>(get_intrinsic_subroutine_by_id_db().at(id));
    }

    static inline impl_subroutine get_instantiate_subroutine(int64_t id) {
        if( get_intrinsic_subroutine_by_id_db().find(id) == get_intrinsic_subroutine_by_id_db().end() ) {
            return nullptr;
        }
        return std::get<0>(get_intrinsic_subroutine_by_id_db().at(id));
    }

    inline std::string get_intrinsic_subroutine_name_from_registry(int64_t id) {
        // Use switch statement instead of lazy map for zero runtime overhead
        return ASRUtils::get_intrinsic_subroutine_name(static_cast<int>(id));
    }

} // namespace IntrinsicImpureSubroutineRegistry

} // namespace ASRUtils

} // namespace LCompilers

#endif // LFORTRAN_PASS_INTRINSIC_SUBROUTINE_REGISTRY_H
