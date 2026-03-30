module legacy_array_sections_18_mod
  implicit none
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

  subroutine dsort(dx)
    implicit none
    real, dimension(*), intent(inout) :: dx
    call sort_ascending(dx)
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

program legacy_array_sections_18
  use legacy_array_sections_18_mod
  implicit none
end program
