program select_rank_19
  implicit none
  real, allocatable :: x(:,:)

  allocate(x(3, 4))
  x = 1.0
  call allocate_if_necessary(x)
  if (allocated(x)) error stop
  print *, "OK"

contains

  subroutine allocate_if_necessary(array)
    real, allocatable, intent(inout) :: array(..)

    select rank(array)
      rank(2)
        if (allocated(array)) then
          deallocate(array)
        end if
    end select

  end subroutine

end program
