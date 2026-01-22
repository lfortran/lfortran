program inquiries
   implicit none

! Test Fortran-77-ish subset of INQUIRE capabilities

   integer :: unit, unit_no
   logical :: exists, opened, named
   character(32) :: name, access, seq, direct, blank
   character(32) :: form, formatted, unformatted
   integer :: recl, nextrec
   integer :: i

   character(*), parameter :: testfn = 'inqtest.dat'
   character(*), parameter :: uninitc = '(uninit)'
   print *, unit
! Sequential formatted by filename

   print *, 'sequential formatted by filename'
   open (newunit=unit, file=testfn, form='formatted', status='unknown')
   write (unit, '(i4)') 42
   rewind (unit)

   call init_test ()
   inquire (file=testfn, exist=exists, opened=opened, number=unit_no,  &
      named=named, name=name, access=access, sequential=seq, direct=direct,  &
      form=form, formatted=formatted, unformatted=unformatted, blank=blank)
   print *, 'exist      = ', exists, ' :', pf (exists)
   print *, 'opened     = ', opened, ' :', pf (opened)
   print *, 'number     = ', unit_no, ' :', pf (unit == unit_no)
   print *, 'named      = ', named, ' :', pf (named)
   print *, 'name       = ', trim (name), ' :', pf (name == testfn)
   print *, 'access     = ', trim (access), ' :', pf (access == 'SEQUENTIAL')
   print *, 'sequential = ', trim (seq), ' :', pf (seq == 'YES')
   print *, 'direct     = ', trim (direct), ' :', pf (direct == 'NO')
   print *, 'form       = ', trim (form), ' :', pf (form == 'FORMATTED')
   print *, 'formatted  = ', trim (formatted), ' :', pf (formatted == 'YES')
   print *, 'unformatted =', trim (unformatted), ' :', pf (unformatted == 'NO')
   print *, 'blank      = ', trim (blank), ' :', pf (blank == 'NULL')
   close (unit, status='keep')

! Sequential formatted by unitno

   print *; print *, 'sequential formatted by unitno'
   open (newunit=unit, file=testfn, form='formatted', status='old', blank='zero')
   write (unit, '(i4)') 42
   rewind (unit)

   call init_test ()
   inquire (unit=unit, exist=exists, opened=opened, number=unit_no,  &
      named=named, name=name, access=access, sequential=seq, direct=direct,  &
      form=form, formatted=formatted, unformatted=unformatted, blank=blank)
   print *, 'exist      = ', exists, ' :', pf (exists)
   print *, 'opened     = ', opened, ' :', pf (opened)
   print *, 'number     = ', unit_no, ' :', pf (unit == unit_no)
   print *, 'named      = ', named, ' :', pf (named)
   print *, 'name       = ', trim (name), ' :', pf (name == testfn)
   print *, 'access     = ', trim (access), ' :', pf (access == 'SEQUENTIAL')
   print *, 'sequential = ', trim (seq), ' :', pf (seq == 'YES')
   print *, 'direct     = ', trim (direct), ' :', pf (direct == 'NO')
   print *, 'form       = ', trim (form), ' :', pf (form == 'FORMATTED')
   print *, 'formatted  = ', trim (formatted), ' :', pf (formatted == 'YES')
   print *, 'unformatted =', trim (unformatted), ' :', pf (unformatted == 'NO')
   print *, 'blank      = ', trim (blank), ' :', pf (blank == 'ZERO')
   close (unit, status='delete')

! Sequential unformatted by filename

   print *; print *, 'sequential formatted by filename'
   open (newunit=unit, file=testfn, form='unformatted', status='new')
   write (unit) 42
   rewind (unit)

   call init_test ()
   inquire (file=testfn, exist=exists, opened=opened, number=unit_no,  &
      named=named, name=name, access=access, sequential=seq, direct=direct,  &
      form=form, formatted=formatted, unformatted=unformatted, blank=blank)
   print *, 'exist      = ', exists, ' :', pf (exists)
   print *, 'opened     = ', opened, ' :', pf (opened)
   print *, 'number     = ', unit_no, ' :', pf (unit == unit_no)
   print *, 'named      = ', named, ' :', pf (named)
   print *, 'name       = ', trim (name), ' :', pf (name == testfn)
   print *, 'access     = ', trim (access), ' :', pf (access == 'SEQUENTIAL')
   print *, 'sequential = ', trim (seq), ' :', pf (seq == 'YES')
   print *, 'direct     = ', trim (direct), ' :', pf (direct == 'NO')
   print *, 'form       = ', trim (form), ' :', pf (form == 'UNFORMATTED')
   print *, 'formatted  = ', trim (formatted), ' :', pf (formatted == 'NO')
   print *, 'unformatted =', trim (unformatted), ' :', pf (unformatted == 'YES')
   print *, 'blank      = ', trim (blank), ' :', pf (blank == 'UNDEFINED')
   close (unit, status='keep')

! Sequential unformatted by unitno

   print *; print *, 'sequential formatted by unitno'
   open (newunit=unit, file=testfn, form='unformatted', status='old')
   write (unit) 42
   rewind (unit)

   call init_test ()
   inquire (unit=unit, exist=exists, opened=opened, number=unit_no,  &
      named=named, name=name, access=access, sequential=seq, direct=direct,  &
      form=form, formatted=formatted, unformatted=unformatted, blank=blank)
   print *, 'exist      = ', exists, ' :', pf (exists)
   print *, 'opened     = ', opened, ' :', pf (opened)
   print *, 'number     = ', unit_no, ' :', pf (unit == unit_no)
   print *, 'named      = ', named, ' :', pf (named)
   print *, 'name       = ', trim (name), ' :', pf (name == testfn)
   print *, 'access     = ', trim (access), ' :', pf (access == 'SEQUENTIAL')
   print *, 'sequential = ', trim (seq), ' :', pf (seq == 'YES')
   print *, 'direct     = ', trim (direct), ' :', pf (direct == 'NO')
   print *, 'form       = ', trim (form), ' :', pf (form == 'UNFORMATTED')
   print *, 'formatted  = ', trim (formatted), ' :', pf (formatted == 'NO')
   print *, 'unformatted =', trim (unformatted), ' :', pf (unformatted == 'YES')
   print *, 'blank      = ', trim (blank), ' :', pf (blank == 'UNDEFINED')
   close (unit, status='delete')

! Record length inquiry

   inquire (iolength=recl) 42, 42.0, 'xyzzy'
   print *; print *, 'record length =', recl

! Direct unformatted by filename

   print *; print *, 'direct formatted by filename'
   open (newunit=unit, file=testfn, access='direct', recl=recl, form='unformatted', status='new')
   do, i=1, 5
      write (unit, rec=i) 42+i, 42.0+i, 'xyzzy'
   end do

   call init_test ()
   inquire (file=testfn, exist=exists, opened=opened, number=unit_no,  &
      named=named, name=name, access=access, sequential=seq, direct=direct,  &
      form=form, formatted=formatted, unformatted=unformatted,  &
      recl=recl, nextrec=nextrec)
   print *, 'exist      = ', exists, ' :', pf (exists)
   print *, 'opened     = ', opened, ' :', pf (opened)
   print *, 'number     = ', unit_no, ' :', pf (unit == unit_no)
   print *, 'named      = ', named, ' :', pf (named)
   print *, 'name       = ', trim (name), ' :', pf (name == testfn)
   print *, 'access     = ', trim (access), ' :', pf (access == 'DIRECT')
   print *, 'sequential = ', trim (seq), ' :', pf (seq == 'NO')
   print *, 'direct     = ', trim (direct), ' :', pf (direct == 'YES')
   print *, 'form       = ', trim (form), ' :', pf (form == 'UNFORMATTED')
   print *, 'formatted  = ', trim (formatted), ' :', pf (formatted == 'NO')
   print *, 'unformatted =', trim (unformatted), ' :', pf (unformatted == 'YES')
   print *, 'recl       = ', recl, ' :', pf (recl == 13)
   ! print *, 'nextrec    = ', nextrec, ' :', pf (nextrec == 6)
   close (unit, status='keep')

! Direct unformatted by unitno

   print *; print *, 'direct formatted by unitno'
   open (newunit=unit, file=testfn, access='direct', recl=recl, form='unformatted', status='old')
   do, i=1, 5
      write (unit, rec=i) 42+i, 42.0+i, 'xyzzy'
   end do

   call init_test ()
   inquire (unit=unit, exist=exists, opened=opened, number=unit_no,  &
      named=named, name=name, access=access, sequential=seq, direct=direct,  &
      form=form, formatted=formatted, unformatted=unformatted,  &
      recl=recl, nextrec=nextrec)
   print *, 'exist      = ', exists, ' :', pf (exists)
   print *, 'opened     = ', opened, ' :', pf (opened)
   print *, 'number     = ', unit_no, ' :', pf (unit == unit_no)
   print *, 'named      = ', named, ' :', pf (named)
   print *, 'name       = ', trim (name), ' :', pf (name == testfn)
   print *, 'access     = ', trim (access), ' :', pf (access == 'DIRECT')
   print *, 'sequential = ', trim (seq), ' :', pf (seq == 'NO')
   print *, 'direct     = ', trim (direct), ' :', pf (direct == 'YES')
   print *, 'form       = ', trim (form), ' :', pf (form == 'UNFORMATTED')
   print *, 'formatted  = ', trim (formatted), ' :', pf (formatted == 'NO')
   print *, 'unformatted =', trim (unformatted), ' :', pf (unformatted == 'YES')
   print *, 'recl       = ', recl, ' :', pf (recl == 13)
   ! print *, 'nextrec    = ', nextrec, ' :', pf (nextrec == 6)
   close (unit, status='delete')

contains

   subroutine init_test ()

      exists = .false.; opened = .false.; unit_no = -99999
      named = .false.; name = uninitc
      access = uninitc; seq = uninitc; direct = uninitc
      form = uninitc; formatted = uninitc; unformatted = uninitc
      blank = uninitc
      recl = -99999; nextrec = -99999

   end subroutine

   function pf (l)
      logical, intent(in) :: l
      character(4) :: pf
      pf = merge ('pass', 'FAIL', l)
      if (.not. l) error stop
   end function
end program
