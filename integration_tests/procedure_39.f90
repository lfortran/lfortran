! Test that the promote_allocatable_to_nonallocatable optimization pass
! does not crash when a procedure pointer component in a derived type
! with an allocatable character component is called via a class (polymorphic)
! dummy argument. The subroutine_from_function pass can produce calls
! where n_args exceeds the function type's n_arg_types.
module procedure_39_mod
  implicit none

  type :: result_t
    logical :: passed = .false.
  end type

  abstract interface
    function result_fn_i() result(r)
      import result_t
      type(result_t) :: r
    end function
  end interface

  type :: desc_t
    character(len=:), allocatable :: text
    procedure(result_fn_i), pointer, nopass :: get_result => null()
  contains
    procedure :: run
  end type

contains

  function make_pass() result(r)
    type(result_t) :: r
    r%passed = .true.
  end function

  subroutine run(self)
    class(desc_t), intent(in) :: self
    type(result_t) :: res
    if (associated(self%get_result)) then
      res = self%get_result()
      if (.not. res%passed) error stop
    end if
  end subroutine

end module

program procedure_39
  use procedure_39_mod
  implicit none
  type(desc_t) :: d

  ! Test with null procedure pointer (should not call)
  call d%run()

  ! Test with assigned procedure pointer
  d%get_result => make_pass
  call d%run()

  print *, "ok"
end program
