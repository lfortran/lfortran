submodule (modules_95_m) modules_95_s
contains
    module subroutine touch(me)
        type(t), intent(inout) :: me
        me%k1 = me%k1
    end subroutine
end submodule
