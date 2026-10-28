program submodule_59
    use submodule_59_parent, only: value, descendant_value
    use submodule_59_other, only: other_value => value, other_descendant => descendant_value
    implicit none
    if (value() /= 3) error stop
    if (descendant_value() /= 6) error stop
    if (other_value() /= 5) error stop
    if (other_descendant() /= 10) error stop
end program
