! Continue-compilation diagnostics that need `--implicit-interface`. Append new
! cases at the end of this file.

! A typed external dummy procedure is a function, so a subroutine actual
! argument does not match it during generic resolution.
module cc_ii_generic_typed_external_mod
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

subroutine generic_typed_external_subroutine()
    use cc_ii_generic_typed_external_mod
    implicit none
    call gen(mysub)  ! {Error} Arguments do not match for any generic procedure, gen
end subroutine
