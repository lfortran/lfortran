
program host_assoc_21
use, intrinsic :: iso_c_binding, only: c_alert
use, intrinsic :: iso_c_binding, only: c_associated
use, intrinsic :: iso_c_binding, only: c_backspace
use, intrinsic :: iso_c_binding, only: c_bool
use, intrinsic :: iso_c_binding, only: c_carriage_return
use, intrinsic :: iso_c_binding, only: c_char
use, intrinsic :: iso_c_binding, only: c_double
use, intrinsic :: iso_c_binding, only: c_double_complex
use, intrinsic :: iso_c_binding, only: c_f_pointer
use, intrinsic :: iso_c_binding, only: c_f_procpointer
use, intrinsic :: iso_c_binding, only: c_float
use, intrinsic :: iso_c_binding, only: c_float_complex
use, intrinsic :: iso_c_binding, only: c_form_feed
use, intrinsic :: iso_c_binding, only: c_funloc
use, intrinsic :: iso_c_binding, only: c_funptr
use, intrinsic :: iso_c_binding, only: c_horizontal_tab
use, intrinsic :: iso_c_binding, only: c_int
use, intrinsic :: iso_c_binding, only: c_int16_t
use, intrinsic :: iso_c_binding, only: c_int32_t
use, intrinsic :: iso_c_binding, only: c_int64_t
use, intrinsic :: iso_c_binding, only: c_int8_t
use, intrinsic :: iso_c_binding, only: c_int_fast16_t
use, intrinsic :: iso_c_binding, only: c_int_fast32_t
use, intrinsic :: iso_c_binding, only: c_int_fast64_t
use, intrinsic :: iso_c_binding, only: c_int_fast8_t
use, intrinsic :: iso_c_binding, only: c_int_least16_t
use, intrinsic :: iso_c_binding, only: c_int_least32_t
use, intrinsic :: iso_c_binding, only: c_int_least64_t
use, intrinsic :: iso_c_binding, only: c_int_least8_t
use, intrinsic :: iso_c_binding, only: c_intmax_t
use, intrinsic :: iso_c_binding, only: c_intptr_t
use, intrinsic :: iso_c_binding, only: c_loc
use, intrinsic :: iso_c_binding, only: c_long
use, intrinsic :: iso_c_binding, only: c_long_double
use, intrinsic :: iso_c_binding, only: c_long_double_complex
use, intrinsic :: iso_c_binding, only: c_long_long
use, intrinsic :: iso_c_binding, only: c_new_line
use, intrinsic :: iso_c_binding, only: c_null_char
use, intrinsic :: iso_c_binding, only: c_null_funptr
use, intrinsic :: iso_c_binding, only: c_null_ptr
use, intrinsic :: iso_c_binding, only: c_ptr
use, intrinsic :: iso_c_binding, only: c_ptrdiff_t
use, intrinsic :: iso_c_binding, only: c_short
use, intrinsic :: iso_c_binding, only: c_signed_char
use, intrinsic :: iso_c_binding, only: c_size_t
use, intrinsic :: iso_c_binding, only: c_sizeof
use, intrinsic :: iso_c_binding, only: c_vertical_tab
implicit none
type :: __lcompilers_frame_host_assoc_21
    integer(4), reference :: n
    procedure(callback), closure, pointer, reference :: p
end type __lcompilers_frame_host_assoc_21
type(__lcompilers_frame_host_assoc_21) :: __lcompilers_frame
type(c_ptr) :: address
integer(4) :: n
procedure(callback), closure, pointer :: p
bind_reference(__lcompilers_frame%n, n)
bind_reference(__lcompilers_frame%p, p)
n = 2
p => procedure_closure(fill_closure_entry, interface=fill_closure_interface, environment_parameter=__lcompilers_frame, environment=__lcompilers_frame)
call invoke(procedure_closure(worker_closure_entry, interface=worker_closure_interface,&
         environment_parameter=__lcompilers_frame, environment=__lcompilers_frame))
address = export(procedure_closure(plain, interface=plain))
if (.not. c_associated(address)) then
    error stop
end if
call consume(procedure_raw_pointer(procedure_closure(plain, interface=plain)))

contains

pure integer(4) function __lcompilers_get_n(__lcompilers_frame)
    type(__lcompilers_frame_host_assoc_21), intent(in), host_frame_argument :: __lcompilers_frame
    integer(4), host_associated :: n => __lcompilers_frame%n
    __lcompilers_get_n = n
end function __lcompilers_get_n

pure integer(4) function __lcompilers_get_n1(__lcompilers_frame)
    type(__lcompilers_frame_host_assoc_21), intent(in), host_frame_argument :: __lcompilers_frame
    integer(4), host_associated :: n => __lcompilers_frame%n
    __lcompilers_get_n1 = n
end function __lcompilers_get_n1

interface
    pure integer(4) function __lcompilers_get_n_closure_interface()
    end function __lcompilers_get_n_closure_interface
end interface

interface
    subroutine callback(values)
        integer(4), dimension(Max(0, __lcompilers_get_n_closure_interface(lexical_environment(__lcompilers_frame)))),&
         intent(out) :: values
    end subroutine callback
end interface

subroutine consume(f) bind(c)
    procedure(plain), raw_pointer :: f
    call f()
end subroutine consume

type(c_ptr) function export(f) result(address)
    procedure(plain), closure :: f
    address = c_loc(procedure_raw_pointer(f))
end function export

subroutine fill(values, __lcompilers_frame)
    integer(4) :: __do_loop_end
    type(__lcompilers_frame_host_assoc_21), intent(in), host_frame_argument :: __lcompilers_frame
    integer(4) :: __libasr_length_values_1 = __lcompilers_get_n1(__lcompilers_frame)
    integer(4) :: __libasr_extent_values_1 = Max(0, __libasr_length_values_1)
    integer(4) :: __libasr_index_0_
    integer(4), host_associated :: n => __lcompilers_frame%n
    integer(4), dimension(__libasr_extent_values_1), intent(out) :: values
    __do_loop_end = int(__libasr_extent_values_1, kind=4) + int(1, kind=4) - 1
    __libasr_index_0_ = int(1, kind=4) - 1
    do while (__libasr_index_0_ + 1 <= __do_loop_end)
        __libasr_index_0_ = __libasr_index_0_ + 1
        values(__libasr_index_0_) = n
    end do
end subroutine fill

subroutine fill_closure_entry(__lcompilers_frame, values)
    type(__lcompilers_frame_host_assoc_21), intent(in), closure_environment :: __lcompilers_frame
    integer(4), dimension(:), intent(out) :: values
    call fill(values, __lcompilers_frame)
end subroutine fill_closure_entry

interface
    subroutine fill_closure_interface(values)
        integer(4) :: __libasr_length_values_1 = __lcompilers_get_n1(lexical_environment(__lcompilers_frame))
        integer(4) :: __libasr_extent_values_1 = Max(0, __libasr_length_values_1)
        integer(4), dimension(__libasr_extent_values_1), intent(out) :: values
    end subroutine fill_closure_interface
end interface

subroutine invoke(f)
    procedure(f_interface), closure, intent(in) :: f
    interface
        subroutine f_interface()
        end subroutine f_interface

    end interface
    call f()
end subroutine invoke

subroutine plain() bind(c)
end subroutine plain

subroutine worker(__lcompilers_frame)
    integer(4) :: __do_loop_end
    type(__lcompilers_frame_host_assoc_21), intent(in), host_frame_argument :: __lcompilers_frame
    logical(4) :: __libasr_created__intrinsic_array_function_Any
    integer(4), host_associated :: n => __lcompilers_frame%n
    integer(4) :: __libasr_extent_values_1 = Max(0, n)
    integer(4) :: __libasr_index_0_
    logical(4) :: __libasr_reduction_acc
    procedure(callback), closure, pointer, host_associated :: p => __lcompilers_frame%p
    integer(4), dimension(__libasr_extent_values_1) :: values
    call p(values)
    __libasr_reduction_acc = .false.
    __do_loop_end = ubound(values, 1)
    __libasr_index_0_ = lbound(values, 1) - 1
    do while (__libasr_index_0_ + 1 <= __do_loop_end)
        __libasr_index_0_ = __libasr_index_0_ + 1
        __libasr_reduction_acc = __libasr_reduction_acc .or. values(__libasr_index_0_) /= n
    end do
    __libasr_created__intrinsic_array_function_Any = __libasr_reduction_acc
    if (__libasr_created__intrinsic_array_function_Any) then
        error stop
    end if
end subroutine worker

subroutine worker_closure_entry(__lcompilers_frame)
    type(__lcompilers_frame_host_assoc_21), intent(in), closure_environment :: __lcompilers_frame
    call worker(__lcompilers_frame)
end subroutine worker_closure_entry

interface
    subroutine worker_closure_interface()
    end subroutine worker_closure_interface
end interface

integer(4) function _lcompilers_max0_i32(x0, x1)
    integer(4), intent(in) :: x0
    integer(4), intent(in) :: x1
    _lcompilers_max0_i32 = x0
    if (x1 > _lcompilers_max0_i32) then
        _lcompilers_max0_i32 = x1
    end if
end function _lcompilers_max0_i32

end program host_assoc_21
