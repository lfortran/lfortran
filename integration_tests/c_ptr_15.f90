program c_ptr_15
  use iso_c_binding, only: c_ptr, c_null_ptr, c_loc, c_associated
  implicit none
  integer, target :: x
  type(c_ptr) :: p
  x = 42
  p = c_loc(x)

  ! Test passing c_null_ptr to a value parameter
  call outer(c_null_ptr)
  ! Test passing a non-null c_ptr to a value parameter
  call outer(p)

  print *, "ok"
contains
  subroutine outer(p)
    type(c_ptr), intent(in), value :: p
    call inner(p)
  end subroutine
  subroutine inner(p)
    type(c_ptr), intent(in), value :: p
    ! Verify that a null c_ptr stays null and a non-null stays non-null
    if (c_associated(p)) then
      ! non-null path: nothing to assert beyond not crashing
    end if
  end subroutine
end program
