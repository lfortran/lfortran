module halvestring_mod
contains
    pure function half(s1) result(s2)
        character(*), intent(in) :: s1
        character(len(s1)/2) :: s2
        s2 = s1
    end function half
end module halvestring_mod

program character_24
use halvestring_mod, only: half

block
    character(*), parameter :: blk_str = 'abcdeedcba'
    character(len(half(blk_str))) :: blk_stringb
    if (len(blk_stringb) /= 5) error stop
end block

call f()

contains

    subroutine f()
    character(*), parameter :: spec_str = 'abcdeedcba'
    character(len(half(spec_str))) :: spec_stringb
    if (len(spec_stringb) /= 5) error stop
    print *, len(spec_stringb)
    end subroutine

end program
