! The intrinsic module `generics` (`use, intrinsic :: generics`): predefined
! traits for the generics constraints of the traits proposal. `INumeric` is
! the closed type set of every integer, real and complex type of every kind
! that LFortran supports.
module lfortran_intrinsic_generics
implicit none
private
public :: INumeric

abstract interface :: INumeric
    integer(*) | real(*) | complex(*)
end interface INumeric

end module
