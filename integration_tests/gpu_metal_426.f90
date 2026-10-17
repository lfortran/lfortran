! A name bound to a section that is not contiguous, handed by a routine the
! kernel calls to a procedure of the program, which hands its dummy on to
! another procedure and to matmul. Each of them is given the elements of the
! section, not those that follow its first element.
module gpu_metal_426_m
  implicit none
contains
  pure function weigh(a) result(r)
    real, intent(in) :: a(:,:)
    real :: r
    r = a(1, 1) + 10 * a(2, 1) + 100 * a(1, 2) + 1000 * a(2, 2)
  end function

  pure function weigh_more(a) result(r)
    real, intent(in) :: a(:,:)
    real :: r
    r = weigh(a) + sum(matmul(a, a(:, 1)))
  end function

  pure function weighed(w, k) result(r)
    real, intent(in) :: w(:,:,:)
    integer, intent(in) :: k
    real :: r
    associate (z => w(1:2, 2:3, k))
      r = weigh_more(z)
    end associate
  end function
end module

program gpu_metal_426
  use gpu_metal_426_m
  implicit none
  real :: w(3, 3, 2), s(3)
  integer :: i

  w = reshape([(real(i), i = 1, 18)], [3, 3, 2])
  do concurrent (i = 1:3)
    s(i) = weighed(w, 2)
  end do
  print *, s
  if (any(abs(s - weigh_more(w(1:2, 2:3, 2))) > 1e-1)) error stop 1
  if (abs(s(1) - 19566.0) > 1e-1) error stop 2
end program
