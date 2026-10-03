submodule(submodule_30_mod:submodule_30_child) submodule_30_grandchild
    implicit none
contains
    module procedure compute
        y = x * 7
    end procedure
end submodule
