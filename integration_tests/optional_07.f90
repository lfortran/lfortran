program optional_07
  ! Test for issue #9561: absent optional array arguments passed through nested calls
  implicit none
  integer :: x(666)
  call outer( x(:) )
contains

  subroutine outer( x, opt )
    integer, intent(inout)        :: x(:)
    integer, intent(in), optional :: opt(:)
    call inner( x(:), opt )
  end subroutine outer

  subroutine inner( x, opt )
    integer, intent(inout)        :: x(:)
    integer, intent(in), optional :: opt(size(x))
    print *,'opt present?',present(opt)
    if (present(opt)) error stop "Expected opt to be absent"
  end subroutine inner

end program optional_07
