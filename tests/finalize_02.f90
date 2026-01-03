program finalize_02
    type t
      integer :: inn
    end type t
    
    type tt
      real :: rr
      type(t) :: t_instance
    end type 
  
    call ss(200)
    
    contains 
    subroutine ss(nang)
      type(tt), allocatable :: arr_01(:)
      integer,allocatable :: arr_02(:)
      integer, allocatable :: arr_03
      character(:), allocatable :: arr_04(:)
      character(20) :: arr_05(5)
      integer, allocatable :: arr_06(:)
      character(:), allocatable :: arr_07
      character(:), allocatable :: arr_08(:)
      integer :: nang
      integer :: arr_09(nang)
    end subroutine  
  end program