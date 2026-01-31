module array_constructor_03_mod
  implicit none

  type :: string_t
    character(len=:), allocatable :: s
  end type string_t

contains

  subroutine new(args)
    type(string_t), intent(in) :: args(:)

    if (size(args) /= 0) then
      error stop "ERROR: args is not zero-sized"
    end if

    if (lbound(args,1) /= 1) then
      error stop "ERROR: unexpected lower bound"
    end if

    print *, "OK: new() called with zero arguments"

  end subroutine new

end module array_constructor_03_mod


program array_constructor_03
  use array_constructor_03_mod
  implicit none

  call new([string_t::])

end program array_constructor_03
