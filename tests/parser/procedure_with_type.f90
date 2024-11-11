program procedure_with_type
procedure(), pointer :: x
procedure(double precision), pointer :: y
procedure(doubleprecision), pointer :: yb
procedure(integer), pointer :: z
procedure(real), pointer :: a
procedure(complex), pointer :: b
procedure(real(8)), pointer :: c
integer, parameter :: dp = kind(0.d0)
procedure(real(dp)), pointer :: d
procedure(character(1, 1)), pointer :: e
end program
