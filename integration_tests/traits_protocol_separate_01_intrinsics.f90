! Acknowledges the intrinsic conformances the driver uses. The provider does
! not use this module, so no numeric conformance is visible to it.
module traits_protocol_separate_01_intrinsics_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_separate_01_contracts_m, only: INumeric
    implicit none

    implements INumeric :: integer
    end implements integer

    implements INumeric :: real(real64)
    end implements real(real64)
end module traits_protocol_separate_01_intrinsics_m
