module M_CLI2_20
implicit none

contains

elemental impure function specified(key)
    character(len=*),intent(in) :: key
    logical                     :: specified
    specified=.false.
end function specified
end module M_CLI2_20
