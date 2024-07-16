! program intrinsics_30
!     integer, parameter :: ar1 = sum([1, 21, 13])
! end program

! program intrinsics_69
!     integer, parameter :: a3 = radix([1, 2, 3])  
! end program intrinsics_69

! program intrinsics_08
!     real, parameter :: x=tiny([1.0, 2.0])  
! end program

! program intrinsics_27
!     integer :: a(1)
!     print *, shape(a)             ! (/ 3, 4 /)
! end program


! program intrinsics_57
!     implicit none
    
!     character(len=:), allocatable :: shorts(:)
!     character(len=:), allocatable :: long

!     allocate(character(len=6) :: shorts(10))
!     allocate(character(len=5) :: long)
!     shorts(1:10:2) = "shorts"
!     long = "long"
!     print *, merge(1, 2, shorts == long)    
! end program
    