! Test procedure pointer component in a derived type with allocatable
! character component, accessed via a class (polymorphic) dummy argument.
! This triggered a GEP type mismatch in codegen when the subroutine_from_function
! pass incorrectly double-wrapped the m_dt StructInstanceMember expression.
module procedure_27_mod
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

program procedure_27
  use procedure_27_mod
  implicit none
  type(desc_t) :: d

  ! Test with null procedure pointer (should not call)
  call d%run()

  ! Test with assigned procedure pointer
  d%get_result => make_pass
  call d%run()

  print *, "ok"
end program
