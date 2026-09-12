program gpu_metal_211
  ! Test: a procedure containing a `block` construct (including a nested
  ! `block`) called from the body of a `do concurrent`
  implicit none

  integer :: d(4), e(4), i

  d = 0
  e = 0

  do concurrent (i = 1:4)
    d(i) = shifted(i)
  end do

  do concurrent (i = 1:4)
    e(i) = nested_sum(i)
  end do

  if (any(d /= [2, 3, 4, 5])) error stop "wrong result from block"
  if (any(e /= [11, 22, 33, 44])) error stop "wrong result from nested block"
  print *, "PASS"

contains

  pure function shifted(x) result(y)
    integer, intent(in) :: x
    integer :: y
    block
      integer :: row
      row = 1
      y = x + row
    end block
  end function shifted

  pure function nested_sum(x) result(y)
    integer, intent(in) :: x
    integer :: y
    block
      integer :: outer
      outer = 10 * x
      block
        integer :: inner
        inner = x
        y = outer + inner
      end block
    end block
  end function nested_sum

end program gpu_metal_211
