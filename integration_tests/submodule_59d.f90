submodule (submodule_59_parent:submodule_59_parent) submodule_59_leaf
contains
    module procedure descendant_value
        descendant_value = value() + seed
    end procedure
end submodule

submodule (submodule_59_other:submodule_59_parent) submodule_59_leaf
contains
    module procedure descendant_value
        descendant_value = value() + seed
    end procedure
end submodule
