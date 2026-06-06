module intent_out_default_init_with_finalizer_mod
  implicit none

  type :: my_type
     integer :: id = -1
     character(len=32) :: operation = 'none'
   contains
     final :: finalise_my_type
  end type my_type

contains
  recursive subroutine finalise_my_type(this)
    type(my_type), intent(inout) :: this
  end subroutine finalise_my_type

  subroutine reset_it(x)
    type(my_type), intent(out) :: x
    if (x%id /= -1) error stop "id should be -1 after intent(out) entry"
    if (trim(x%operation) /= 'none') &
       error stop "operation should be 'none' after intent(out) entry"
  end subroutine reset_it
end module

program intent_out_default_init_with_finalizer
  use intent_out_default_init_with_finalizer_mod
  implicit none
  type(my_type) :: a
  a%id = 100
  a%operation = 'first_op'
  call reset_it(a)
  print *, "ok"
end program
