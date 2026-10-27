submodule (submodule_61_m) submodule_61_s
    use submodule_61_r
    implicit none
contains
    module procedure s_value
        v = r_next()
        v = v + 100*r_count
        if (r_objs(2)%nm /= "wxyz" .or. r_objs(1)%k /= 5) v = -1
        if (r_count == 1) then
            if (associated(r_obj%q)) v = -2
            allocate(r_obj%q(3))
            r_obj%q = 5
        else if (sum(r_obj%q) /= 15) then
            v = -3
        end if
        v = v + 15
    end procedure

    module procedure s_tag
        t = r_tag
    end procedure
end submodule
