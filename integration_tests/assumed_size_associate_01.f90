module assumed_size_associate_01_mod
  implicit none
  contains

  subroutine sort_ascending(dx)
    real, dimension(*), intent(inout) :: dx
    call quicksort()
  contains
    subroutine quicksort()
      real :: temp
      if (dx(2) < dx(1)) then
        temp = dx(1)
        dx(1) = dx(2)
        dx(2) = temp
      end if
    end subroutine quicksort
  end subroutine sort_ascending

  subroutine dsort(dx)
    real, dimension(*), intent(inout) :: dx
    call sort_ascending(dx)
  end subroutine dsort

  subroutine dfc(w)
    real, intent(inout) :: w(*)
    call dfcmn(w(1))
  end subroutine dfc

  subroutine dfcmn(xtemp)
    real, intent(inout) :: xtemp(*)
    call dsort(xtemp)
  end subroutine dfcmn

end module

program assumed_size_associate_01
  use assumed_size_associate_01_mod
  implicit none
  print *, "PASS"
end program assumed_size_associate_01