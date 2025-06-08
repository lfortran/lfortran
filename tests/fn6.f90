! module fn6
! interface str
!    module procedure msg_scalar
! end interface str

! contains

! function msg_scalar(generic0, generic1, generic2, generic3, &
!                   & generica, genericb, genericc, genericd, &
!                   & sep)
! implicit none

!    class(*), intent(in), optional  :: generic0, generic1, generic2, generic3
!    class(*), intent(in), optional  :: generica, genericb, genericc, genericd
!    character(len=*), intent(in), optional :: sep
!    character(len=:), allocatable :: sep_local
!    character(len=:), allocatable :: msg_scalar
!    character(len=4096) :: line
!    integer :: istart
!    integer :: increment

!    if( present(sep) ) then
!       sep_local = sep
!       increment = len(sep_local)+1
!    else
!       sep_local = ' '
!       increment = 2
!    end if

!    istart = 1
!    line = ''
!    if(present(generic0))call print_generic(generic0)
!    if(present(generic1))call print_generic(generic1)
!    if(present(generic2))call print_generic(generic2)
!    if(present(generic3))call print_generic(generic3)
!    if(present(generica))call print_generic(generica)
!    if(present(genericb))call print_generic(genericb)
!    if(present(genericc))call print_generic(genericc)
!    if(present(genericd))call print_generic(genericd)
!    msg_scalar = trim(line)

! contains

! subroutine print_generic(generic)
!    class(*), intent(in) :: generic
!    select type(generic)
!       type is (integer(kind=1)); write(line(istart:),'(i0)') generic
!       type is (integer(kind=2)); write(line(istart:),'(i0)') generic
!       type is (integer(kind=4)); write(line(istart:),'(i0)') generic
!       type is (integer(kind=8)); write(line(istart:),'(i0)') generic
!       type is (real(kind=4)); write(line(istart:),'(1pg0)') generic
!       type is (real(kind=8))
!          write(line(istart:), '(1pg0)') generic
!       type is (logical)
!          write(line(istart:), '(l1)') generic
!       type is (character(len=*))
!          write(line(istart:), '(a)') trim(generic)
!       type is (complex); write(line(istart:), '("(",1pg0,",",1pg0,")")') generic
!    end select

!    istart = len_trim(line)+increment
!    line = trim(line)//sep_local
! end subroutine print_generic

! end function msg_scalar

! subroutine journal(where, g0, g1, g2, g3, ga, gb, gc, gd, sep)
! implicit none

! character(len=*), intent(in) :: where
! class(*), intent(in) :: g0
! class(*), intent(in), optional :: g1, g2, g3
! class(*), intent(in), optional  :: ga, gb, gc, gd
! character(len=*), intent(in), optional :: sep
! write(*,'(a)') str(g0, g1, g2, g3, ga, gb, gc, gd, sep)
! end subroutine journal

! end module fn6
