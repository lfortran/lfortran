! Re-exports the COMMON variables of common_52_left under other names, so
! that the program reaches them through the modfile of this module.
module common_52_relay
    use common_52_left, only: first => n, rest => a
    implicit none
end module common_52_relay
