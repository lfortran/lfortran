! NOTE: the commented out code can be used to test
! the GenericWrite ASR node end to end
module write7_mod
    type :: person
        ! character(len=20) :: name
        ! integer :: age

        contains

        procedure, private :: pwf, pwunf
        generic :: write(formatted) => pwf
        generic :: write(unformatted) => pwunf
    end type person

    contains

    subroutine pwf(dtv, unit, iotype, vlist, iostat, iomsg)
        class(person), intent(in) :: dtv
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        character(len=9) :: pfmt
        ! WRITE(pfmt,'(A,I2,A,I2,A)') '(A', vlist(1), ',I', vlist(2), ')'
        ! WRITE(unit, FMT=pfmt, IOSTAT=iostat) dtv%name, dtv%age
    end subroutine

    subroutine pwunf(dtv, unit, iostat, iomsg)
        class(person), intent(in) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        ! write(unit, IOSTAT=iostat, IOMSG=iomsg) dtv%name, dtv%age
        ! if (iostat /= 0) then
        !     ! Optionally set iomsg if an error occurs
        !     iomsg = 'Error writing unformatted person data'
        ! end if
    end subroutine
end module

! program write7
!     use write7_mod
!     integer :: id, members
!     type(person) :: chairman

!     id = 1
!     members = 10
!     chairman%name = "John Doe"
!     chairman%age = 45

!     WRITE(6, FMT="(I2, DT (15,6), I5)") id, chairman, members
! end program
