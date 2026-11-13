module integer_kind_16_mod
    implicit none
    integer(16), parameter :: mod_huge = 170141183460469231731687303715884105727_16
    integer(16), parameter :: mod_tagged = 4611686018427387904_16
    integer(16), parameter :: mod_arr(2) = [mod_huge, -1180591620717411303424_16]
end module integer_kind_16_mod
