module arrays_114_mod
  contains

  subroutine sort_ascending(dx)

    real, dimension(*), intent(inout) :: dx

    call quicksort()

  contains
    subroutine quicksort()

      if (dx(2) < dx(1)) then
      end if

    end subroutine quicksort
  end subroutine sort_ascending

  subroutine dsort(Dx)
    implicit none
    real, dimension(*), intent(inout) :: Dx
    call sort_ascending(Dx)
  end subroutine dsort

  subroutine dfc (w)
    real :: w(*)
    call dfcmn(w(5))
  end subroutine dfc

  subroutine dfcmn (xtemp)
    real :: xtemp(*)
    call dsort (xtemp)
  end subroutine dfcmn

end module

program arrays_114
  use arrays_114_mod
  implicit none

  real :: arr(5) = [3.0, 1.0, 4.0, 1.0, 5.0]

  call dsort(arr)

  print *, arr

end program arrays_114
