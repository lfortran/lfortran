module sequence_assoc_01_mod
  implicit none
  type :: worker
  contains
    procedure, nopass :: take_explicit
    procedure, nopass :: take_assumed_size
  end type
contains

  subroutine take_explicit(x, n)
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    ! covers a(3:7) = 3+4+5+6+7
    if (abs(sum(x) - 25.0) > 1.0e-5) error stop "explicit-shape dummy"
  end subroutine

  subroutine take_assumed_size(x)
    real, intent(in) :: x(*)
    if (abs(x(1) - 6.0) > 1.0e-5) error stop "assumed-size dummy first"
    if (abs(x(5) - 10.0) > 1.0e-5) error stop "assumed-size dummy last"
  end subroutine

  subroutine outer(me, work, n)
    class(worker), intent(inout) :: me
    integer, intent(in) :: n
    real, intent(inout) :: work(n)
    ! Sequence association: an array element is the actual argument for an
    ! explicit-shape or assumed-size dummy, which then covers the elements
    ! of the actual's array from that element on.
    call me%take_explicit(work(3), 5)
    call me%take_assumed_size(work(6))
  end subroutine
end module

program sequence_assoc_01
  use sequence_assoc_01_mod
  implicit none
  type(worker) :: me
  real :: a(10)
  integer :: i
  do i = 1, 10
    a(i) = real(i)
  end do
  call outer(me, a, 10)
end program
