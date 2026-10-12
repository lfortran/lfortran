! A typed external dummy procedure is a function, so a subroutine actual
! argument does not match it during generic resolution.
module generic_typed_external_subroutine_01_mod
implicit none
interface gen
    module procedure s
end interface
contains
subroutine s(f)
    real(8), external :: f
    print *, "in s"
end subroutine
subroutine mysub(z)
    real(8), intent(in) :: z
end subroutine
end module

program generic_typed_external_subroutine_01
use generic_typed_external_subroutine_01_mod
implicit none
call gen(mysub)
end program
