! An external dummy called as a subroutine and then passed on to a
! subroutine defined later, whose own external dummy is implicitly typed.
subroutine implicit_interface_66_de(f)
    external f
    call f(1)
    call implicit_interface_66_step(f)
end subroutine

subroutine implicit_interface_66_step(f)
    external f
    call f(2)
end subroutine

subroutine implicit_interface_66_s(i)
    common /c66/ k
    k = k + i
end subroutine

program implicit_interface_66
    external implicit_interface_66_s
    common /c66/ k
    k = 0
    call implicit_interface_66_de(implicit_interface_66_s)
    print *, k
    if (k /= 3) error stop
end program
