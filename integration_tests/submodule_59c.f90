submodule (submodule_59_parent) submodule_59_parent
contains
    module procedure value
        value = seed
    end procedure
end submodule

! A submodule name is local to its ancestor module, not to the source file.
submodule (submodule_59_other) submodule_59_parent
contains
    module procedure value
        value = seed
    end procedure
end submodule
