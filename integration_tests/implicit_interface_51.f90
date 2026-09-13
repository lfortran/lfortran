! A character *expression* - a concatenation with a runtime-length operand, a
! substring with computed bounds - passed to a procedure with an implicit
! interface (the callee lives in implicit_interface_51b.f90, compiled
! separately). The dummy synthesized for the call must be assumed length
! (`character(len=*)`), the form the callee declares, not deferred length,
! which is only legal for an allocatable or a pointer. This mirrors
! MODFLOW-2005's `CALL USTOP('Invalid '//trim(adjustl(text))//...)` reaching
! `CHARACTER STOPMESS*(*)` through an implicit interface.
subroutine caller(text, line, istart, istop)
  implicit none
  character(len=16) :: text
  character(len=200) :: line
  character(len=8) :: words(3)
  integer :: istart, istop
  words(1) = 'ab'
  words(2) = 'cd'
  words(3) = 'ef'
  call check('Invalid '//trim(text), 'Invalid hello')
  call check('Invalid '//trim(adjustl(text))//' option: '//line(istart:istop), &
             'Invalid hello option: OPT')
  call check(line(istart:istop)//' ', 'OPT ')
  call check(trim(text)//trim(line), 'hello--OPT--')
  ! The array of strings exercises the Array-wrapped form of the synthesized
  ! dummy; the callee checks the scalar and only has to be reached with the
  ! array.
  call check_with_array(trim(text)//' ', words//trim(text), 'hello ')
end subroutine caller

program implicit_interface_51
  implicit none
  character(len=16) :: text
  character(len=200) :: line
  text = '  hello'
  line = '--OPT--'
  call caller(adjustl(text), line, 3, 5)
  print *, "ok"
end program implicit_interface_51
