module legacy_array_sections_36_mod
  implicit none
contains

  subroutine sort_ascending(dx)
    real, dimension(*), intent(inout) :: dx

    call quicksort()

  contains
    ! host-associated assumed-size dummy used in an internal procedure
    subroutine quicksort()
      real :: t
      if (dx(2) < dx(1)) then
        t = dx(1)
        dx(1) = dx(2)
        dx(2) = t
      end if
    end subroutine quicksort
  end subroutine sort_ascending

  subroutine dsort(Dx)
    real, dimension(*), intent(inout) :: Dx
    call sort_ascending(Dx)
  end subroutine dsort

  subroutine dfc(w)
    real :: w(*)
    ! array element passed to an assumed-size dummy
    call dfcmn(w(5))
  end subroutine dfc

  subroutine dfcmn(xtemp)
    real :: xtemp(*)
    call dsort(xtemp)
  end subroutine dfcmn

end module legacy_array_sections_36_mod

program legacy_array_sections_36
  use legacy_array_sections_36_mod
  implicit none
  real :: w(8)
  w = [1.0, 2.0, 3.0, 4.0, 9.0, 5.0, 7.0, 8.0]
  call dfc(w)
  print *, w
  if (any(w /= [1.0, 2.0, 3.0, 4.0, 5.0, 9.0, 7.0, 8.0])) error stop
end program legacy_array_sections_36
