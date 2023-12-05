module stdlib_kinds
    use iso_fortran_env, only: int64
    implicit none
    private
    public :: int64

end module stdlib_kinds

module stdlib_hashmaps
    use stdlib_kinds, only: int64

    implicit none
    private

    integer, parameter, public :: int_calls = int64

end module stdlib_hashmaps
