module testdrive
  use, intrinsic :: iso_fortran_env, only : error_unit
  implicit none
  private

  public :: run_testsuite, new_unittest, unittest_type, testsuite_type, &
      error_type, skip_test

  integer, parameter :: success = 0, fatal = 1, skipped = 77

  type :: error_type
    integer :: stat = success
    character(len=:), allocatable :: message
  end type error_type

  abstract interface
    subroutine test_interface(error)
      import :: error_type
      type(error_type), allocatable, intent(out) :: error
    end subroutine test_interface
  end interface

  type :: unittest_type
    character(len=:), allocatable :: name
    procedure(test_interface), pointer, nopass :: test => null()
    logical :: should_fail = .false.
  end type unittest_type

  abstract interface
    subroutine collect_interface(testsuite)
      import :: unittest_type
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
    end subroutine collect_interface
  end interface

  type :: testsuite_type
    character(len=:), allocatable :: name
    procedure(collect_interface), pointer, nopass :: collect => null()
  end type testsuite_type

  character(len=*), parameter :: fmt = '(1x, *(1x, a))'

contains

  recursive subroutine run_testsuite(collect, unit, stat, parallel)
    procedure(collect_interface) :: collect
    integer, intent(in) :: unit
    integer, intent(inout) :: stat
    logical, intent(in), optional :: parallel
    type(unittest_type), allocatable :: testsuite(:)
    integer :: it
    logical :: parallel_
    parallel_ = .true.
    if(present(parallel)) parallel_ = parallel
    call collect(testsuite)
    do it = 1, size(testsuite)
      write(unit, '(1x, 3(1x, a), 1x, "(", i0, "/", i0, ")")') &
        & "Starting", testsuite(it)%name, "...", it, size(testsuite)
      call run_unittest(testsuite(it), unit, stat)
    end do
  end subroutine run_testsuite

  recursive subroutine run_unittest(test_var, unit, stat)
    type(unittest_type), intent(in) :: test_var
    integer, intent(in) :: unit
    integer, intent(inout) :: stat
    type(error_type), allocatable :: error
    character(len=:), allocatable :: message
    call test_var%test(error)
    if (.not.test_skipped(error)) then
      if (allocated(error) .neqv. test_var%should_fail) stat = stat + 1
    end if
    call make_output(message, test_var, error)
    write(unit, '(a)') message
  end subroutine run_unittest

  pure function test_skipped(error) result(is_skipped)
    type(error_type), intent(in), optional :: error
    logical :: is_skipped
    is_skipped = .false.
    if (present(error)) then
      is_skipped = error%stat == skipped
    end if
  end function test_skipped

  pure subroutine make_output(output, test, error)
    character(len=:), allocatable, intent(out) :: output
    type(unittest_type), intent(in) :: test
    type(error_type), intent(in), optional :: error
    character(len=:), allocatable :: label
    character(len=*), parameter :: indent = "       " // "..." // " "
    if (test_skipped(error)) then
      output = indent // test%name // " [SKIPPED]" &
        & // new_line("a") // "  Message: " // error%message
      return
    end if
    if (present(error) .neqv. test%should_fail) then
      if (test%should_fail) then
        label = " [UNEXPECTED PASS]"
      else
        label = " [FAILED]"
      end if
    else
      if (test%should_fail) then
        label = " [EXPECTED FAIL]"
      else
        label = " [PASSED]"
      end if
    end if
    output = indent // test%name // label
    if (present(error)) then
      output = output // new_line("a") // "  Message: " // error%message
    end if
  end subroutine make_output

  function new_unittest(name, test, should_fail) result(self)
    character(len=*), intent(in) :: name
    procedure(test_interface) :: test
    logical, intent(in), optional :: should_fail
    type(unittest_type) :: self
    self%name = name
    self%test => test
    if (present(should_fail)) self%should_fail = should_fail
  end function new_unittest

  subroutine test_failed(error, message, more, and_more)
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: message
    character(len=*), intent(in), optional :: more
    character(len=*), intent(in), optional :: and_more
    character(len=*), parameter :: skip = new_line("a") // "           "
    allocate(error)
    error%stat = fatal
    error%message = message
    if (present(more)) then
      error%message = error%message // skip // more
    end if
    if (present(and_more)) then
      error%message = error%message // skip // and_more
    end if

  end subroutine test_failed

  subroutine skip_test(error, message, more, and_more)
    type(error_type), allocatable, intent(out) :: error
    character(len=*), intent(in) :: message
    character(len=*), intent(in), optional :: more
    character(len=*), intent(in), optional :: and_more
    call test_failed(error, message, more, and_more)
    error%stat = skipped
  end subroutine skip_test

end module testdrive

module test_linalg
use testdrive, only : new_unittest, unittest_type, error_type, skip_test
implicit none

contains

    subroutine collect_linalg(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
        new_unittest("diag_rqp", test_diag_rqp) &
        ]
    end subroutine collect_linalg

    subroutine test_diag_rqp(error)
    type(error_type), allocatable, intent(out) :: error
    call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_diag_rqp

end module


program tester
use, intrinsic :: iso_fortran_env, only : error_unit
use testdrive, only : run_testsuite, testsuite_type
use test_linalg, only : collect_linalg
implicit none
integer :: stat
type(testsuite_type), allocatable :: testsuites(:)
stat = 0
call run_testsuite(collect_linalg, error_unit, stat)
print *, "stat: ", stat
if (stat > 0) error stop
end program
