! A NOPASS binding of a derived type declared in a main program must name a
! module procedure or an external procedure with an explicit interface
! (#12720).
program main
  type mt
   contains
    procedure, nopass :: g  ! {Error} 'g' must be a module procedure or an external procedure with an explicit interface
  end type
end
subroutine s
  forall (i=1:2)
  end forall
end subroutine s
