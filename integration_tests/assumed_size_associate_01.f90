module test_mod
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

  subroutine dfc(w)
    real :: w(*)
    call dfcmn(w(5))
  end subroutine dfc

  subroutine dfcmn(xtemp)
    real :: xtemp(*)
    call dsort(xtemp)
  end subroutine dfcmn

end module

program name
  use test_mod
  implicit none
end program name
