program gpu_metal_201
  ! Test: a double precision ALLOCATABLE dummy argument of a procedure that
  ! runs a `do concurrent` over it must keep the loop on the CPU.  The dummy
  ! carries an `Allocatable(Array(Real(8)))` type, which the kind-8 bail-out
  ! guard failed to see through, so the loop was offloaded to Metal, whose
  ! `float` elements are half the width the host writes.
  implicit none

  double precision, allocatable :: d(:)

  allocate(d(5))
  d = 0.0d0
  call fill(d)
  if (abs(sum(d) - 10.0d0) > 1.0d-12) error stop "allocatable dp dummy"

  call scale_it(d)
  if (abs(sum(d) - 30.0d0) > 1.0d-12) error stop "allocatable dp dummy scale"

  print *, "PASS"

contains

  subroutine fill(a)
    double precision, allocatable, intent(inout) :: a(:)
    integer :: i
    do concurrent (i = 1:5)
      a(i) = 2.0d0
    end do
  end subroutine fill

  subroutine scale_it(a)
    double precision, allocatable, intent(inout) :: a(:)
    integer :: i
    do concurrent (i = 1:5)
      a(i) = a(i) * 3.0d0
    end do
  end subroutine scale_it

end program gpu_metal_201
