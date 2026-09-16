! A parameter with null procedure-pointer components, declared in one file,
! initializes entities in a module and in a program compiled separately.
program structure_constructor_args_08
    use structure_constructor_args_08_a, only: o_t, z, z2
    use structure_constructor_args_08_b, only: mv, mv2, extra, holder_t, local_x
    implicit none
    type(o_t) :: w = z
    type(o_t) :: w2 = z2
    type(holder_t) :: hh
    if (mv%x /= 5 .or. associated(mv%fp) .or. associated(mv%gp)) error stop 1
    if (mv2%x /= 7 .or. associated(mv2%fp) .or. associated(mv2%gp)) error stop 2
    if (hh%h%x /= 7 .or. associated(hh%h%fp) .or. associated(hh%h%gp)) error stop 3
    if (local_x() /= 5) error stop 4
    if (w%x /= 5 .or. associated(w%fp) .or. associated(w%gp)) error stop 5
    if (w2%x /= 7 .or. associated(w2%fp) .or. associated(w2%gp)) error stop 6
    if (associated(extra)) error stop 7
    print *, "ok"
end program
