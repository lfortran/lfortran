program callback_06
  implicit none
  integer, allocatable :: v(:)
  allocate(v(2))
  call run(f)
  if (v(1) /= 1) error stop
  if (v(2) /= 2) error stop
  print *, v
contains
  subroutine run(F)
    interface
      subroutine cb(x)
        integer, intent(in) :: x(:)
      end subroutine
    end interface
    procedure(cb) :: F
    call F([1, 2])
  end subroutine

  subroutine f(x)
    integer, intent(in) :: x(:)
    v = reshape(x, shape(v))
  end subroutine
end program
