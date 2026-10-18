! A dummy argument may appear in only one type declaration statement,
! also when implicit typing is enabled.
subroutine implicit_typing6(x)
    type(*), dimension(..) :: x
    real :: x
end subroutine
