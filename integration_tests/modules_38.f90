module fpm_compiler

implicit none

public :: compiler_t

type string_t
    character(len=:), allocatable :: s
end type

enum, bind(C)
    enumerator :: &
        id_unknown, &
        id_intel_classic_windows, &
        id_intel_llvm_windows
end enum

integer, parameter :: compiler_enum = kind(id_unknown)

type :: compiler_t
    integer(compiler_enum) :: id = id_unknown
    character(len=:), allocatable :: fc
    character(len=:), allocatable :: cc
    character(len=:), allocatable :: cxx
    logical :: echo = .true.
    logical :: verbose = .true.

contains

    procedure :: enumerate_libraries

end type compiler_t

contains

function enumerate_libraries(self, prefix, libs) result(r)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: prefix
    type(string_t), intent(in) :: libs(:)
    character(len=:), allocatable :: r

    if (self%id == id_intel_classic_windows .or. &
        self%id == id_intel_llvm_windows) then
    end if

end function enumerate_libraries

end module fpm_compiler

program modules_38
use fpm_compiler, only : string_t, compiler_t

type(compiler_t) :: compiler_arg
character(len=3) :: prefix_arg
type(string_t) :: libs_arg(4)

print *, compiler_arg%enumerate_libraries(prefix_arg, libs_arg)

end program
