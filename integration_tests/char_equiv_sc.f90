module c4
    implicit none
    character(len=8) :: s
    character(len=2) :: t(4)
    equivalence (s, t)
end module
