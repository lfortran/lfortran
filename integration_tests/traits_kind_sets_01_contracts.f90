! Contracts of traits_kind_sets_01. A kind wildcard stands for every kind of
! its type that LFortran supports: the kinds of ISO_FORTRAN_ENV's
! INTEGER_KINDS and REAL_KINDS, and the real kinds that have a complex type.
module traits_kind_sets_01_contracts
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, &
        real32, real64, real128
    implicit none
    private
    public :: INumeric, IExplicit, IOrdered, ISum, ICount, IMax

    ! traits.tex, "Assumed kind parameters".
    abstract interface :: INumeric
        integer(*) | real(*) | complex(*)
    end interface INumeric

    ! The same members, written out.
    abstract interface :: IExplicit
        integer(int8) | integer(int16) | integer(int32) | integer(int64) | &
            real(real32) | real(real64) | real(real128) | complex(real32) | complex(real64)
    end interface IExplicit

    abstract interface :: IOrdered
        integer(kind=*) | real(kind=*)
    end interface IOrdered

    ! One closed runtime slot per member of IOrdered.
    abstract interface :: ISum
        function sum{IOrdered :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    ! One closed runtime slot per member of INumeric, complex included,
    ! because the result is an integer.
    abstract interface :: ICount
        function nonzero{INumeric :: T}(x) result(n)
            type(T), intent(in) :: x(:)
            integer             :: n
        end function nonzero
    end interface ICount

    abstract interface :: IMax
        function largest{IOrdered :: T}(x) result(m)
            type(T), intent(in) :: x(:)
            type(T)             :: m
        end function largest
    end interface IMax
end module traits_kind_sets_01_contracts
