program coarrays_38
  implicit none

  call test_allocatable_coarray

  contains
    subroutine test_allocatable_coarray()
      implicit none
      integer, save, allocatable :: aca_int_1[:]
      ALLOCATE(aca_int_1[*])
    end subroutine
end program coarrays_38