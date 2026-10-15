! Zero extent of an explicit-shape result through an interface body, for a
! caller in another translation unit: a separate module procedure and an
! external procedure.
module separate_compilation_67a
implicit none
interface
    module function mseq(n) result(r)
        integer, intent(in) :: n
        real :: r(n)
    end function
end interface
end module

submodule (separate_compilation_67a) separate_compilation_67a_s
implicit none
contains
    module function mseq(n) result(r)
        integer, intent(in) :: n
        real :: r(n)
        r = 7
    end function
end submodule

function separate_compilation_67_ext(n) result(r)
    implicit none
    integer, intent(in) :: n
    real :: r(n)
    r = 7
end function
