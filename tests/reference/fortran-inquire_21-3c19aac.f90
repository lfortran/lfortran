module lfortran_intrinsic_custom
implicit none
interface newunit
    module procedure newunit_int_1, newunit_int_2, newunit_int_4, newunit_int_8
end interface newunit

contains

integer(4) function get_valid_newunit() result(unit)
    integer(4) :: lun
    integer(4), parameter :: lun_max = -10
    integer(4), parameter :: lun_min = -1000
    logical(4) :: opened
    do lun = lun_max, lun_min, -1
        inquire(unit=lun, opened=opened)
        if (.not. opened) then
            unit = lun
            return
        end if
    end do
    print *, "All unit numbers are utilized"
    error stop
end function get_valid_newunit

subroutine newunit_int_1(unit)
    integer(1), intent(out) :: unit
    integer(4) :: valid_unit
    valid_unit = get_valid_newunit()
    if (valid_unit < (-128)) then
        print *, "integer(KIND=1) is has small limit. Use larger kind for the unit number"
        error stop
    end if
    unit = Int(valid_unit, 1)
end subroutine newunit_int_1

subroutine newunit_int_2(unit)
    integer(2), intent(out) :: unit
    unit = Int(get_valid_newunit(), 2)
end subroutine newunit_int_2

subroutine newunit_int_4(unit)
    integer(4), intent(out) :: unit
    unit = Int(get_valid_newunit())
end subroutine newunit_int_4

subroutine newunit_int_8(unit)
    integer(8), intent(out) :: unit
    unit = Int(get_valid_newunit(), 8)
end subroutine newunit_int_8

end module lfortran_intrinsic_custom

program inquire_21
use lfortran_intrinsic_custom, only: newunit_int_1
use lfortran_intrinsic_custom, only: newunit_int_2
use lfortran_intrinsic_custom, only: newunit_int_4
use lfortran_intrinsic_custom, only: newunit_int_8
implicit none
character(len=32, kind=1) :: access
character(len=32, kind=1) :: action
character(len=32, kind=1) :: asynchronous
character(len=32, kind=1) :: blank
character(len=32, kind=1) :: decimal
character(len=32, kind=1) :: delim
character(len=32, kind=1) :: direct
character(len=32, kind=1) :: encoding
logical(4) :: exist
character(len=32, kind=1) :: form
character(len=32, kind=1) :: formatted
integer(4) :: i4
integer(4) :: iol
character(len=64, kind=1) :: iomsg
integer(4) :: ios
character(len=0, kind=1) :: lfortran_iomsg
character(len=0, kind=1) :: lfortran_iomsg1
character(len=64, kind=1) :: name
logical(4) :: named
integer(4) :: nextrec
integer(4) :: number
logical(4) :: opened
character(len=32, kind=1) :: pad
logical(4) :: pending
integer(4) :: pos4
integer(8) :: pos8
character(len=32, kind=1) :: position
real(4) :: r4
character(len=32, kind=1) :: read
character(len=32, kind=1) :: readwrite
integer(4) :: recl
character(len=32, kind=1) :: round
character(len=32, kind=1) :: sequential
character(len=32, kind=1) :: sign
integer(4) :: size4
integer(8) :: size8
character(len=32, kind=1) :: stream
integer(4) :: u
character(len=32, kind=1) :: unformatted
integer(4) :: v
character(len=32, kind=1) :: write
call newunit_int_4(u)
open(u, file="inquire_21_data.txt", status="replace", form="formatted")
write(u, "(a)", iomsg=lfortran_iomsg) "hello"
inquire(unit=u, iostat=ios, exist=exist, opened=opened, number=number, named=named, name=name, access=access,&
         sequential=sequential, direct=direct, form=form, formatted=formatted, unformatted=unformatted, recl=recl,&
         nextrec=nextrec, blank=blank, position=position, action=action, read=read, write=write, readwrite=readwrite,&
         delim=delim, pad=pad, decimal=decimal, sign=sign, encoding=encoding, iomsg=iomsg, round=round, pending=pending,&
         asynchronous=asynchronous)
if (.not. exist) then
    error stop
end if
if (.not. opened) then
    error stop
end if
if (number /= u) then
    error stop
end if
if (.not. named) then
    error stop
end if
if (trim(name) /= "inquire_21_data.txt") then
    error stop
end if
if (trim(access) /= "SEQUENTIAL") then
    error stop
end if
if (trim(sequential) /= "YES") then
    error stop
end if
if (trim(direct) /= "NO") then
    error stop
end if
if (trim(form) /= "FORMATTED") then
    error stop
end if
if (trim(formatted) /= "YES") then
    error stop
end if
if (trim(unformatted) /= "NO") then
    error stop
end if
if (trim(blank) /= "NULL") then
    error stop
end if
if (trim(action) /= "READWRITE") then
    error stop
end if
if (trim(read) /= "YES") then
    error stop
end if
if (trim(write) /= "YES") then
    error stop
end if
if (trim(readwrite) /= "YES") then
    error stop
end if
if (trim(delim) /= "QUOTE") then
    error stop
end if
if (trim(pad) /= "YES") then
    error stop
end if
if (trim(decimal) /= "POINT") then
    error stop
end if
if (trim(encoding) /= "DEFAULT") then
    error stop
end if
if (trim(round) /= "PROCESSOR_DEFINED") then
    error stop
end if
if (pending) then
    error stop
end if
if (trim(asynchronous) /= "NO") then
    error stop
end if
if (ios /= 0) then
    error stop
end if
close(u)
inquire(file="inquire_21_data.txt", iostat=ios, exist=exist, opened=opened, number=number, named=named, name=name,&
         access=access, sequential=sequential, direct=direct, form=form, formatted=formatted, unformatted=unformatted,&
         recl=recl, blank=blank, action=action, read=read, write=write, readwrite=readwrite, delim=delim, pad=pad,&
         decimal=decimal, sign=sign, encoding=encoding, iomsg=iomsg, round=round)
if (exist) then
    error stop
end if
if (opened) then
    error stop
end if
call newunit_int_4(v)
open(v, file="inquire_21_stream.txt", status="replace", form="formatted")
write(v, "(a)", iomsg=lfortran_iomsg1) "abcdef"
inquire(unit=v, size=size8, pos=pos8, stream=stream)
if (size8 /= 7_8) then
    error stop
end if
if (pos8 /= 8_8) then
    error stop
end if
if (trim(stream) /= "YES") then
    error stop
end if
__lfortran_inquire : block
    integer(8) :: __lfortran_inquire_pos
    integer(8) :: __lfortran_inquire_size
    inquire(unit=v, size=__lfortran_inquire_size, pos=__lfortran_inquire_pos)
    size4 = int(__lfortran_inquire_size, kind=4)
    pos4 = int(__lfortran_inquire_pos, kind=4)
end block __lfortran_inquire
if (size4 /= 7) then
    error stop
end if
if (pos4 /= 8) then
    error stop
end if
close(v)
inquire(iolength=iol) i4
if (iol /= 4) then
    error stop
end if
inquire(iolength=iol) i4, r4
if (iol /= 8) then
    error stop
end if
print *, "ok"
end program inquire_21
