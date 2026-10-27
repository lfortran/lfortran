! A submodule and the program that uses it in one file.
submodule (submodule_59_m) submodule_59_s
    implicit none
contains
    module procedure shout
        r = greeting // "!"
    end procedure
end submodule

program submodule_59
    use submodule_59_m
    implicit none
    if (shout() /= "hello!") error stop 1
    greeting = "world"
    if (shout() /= "world!") error stop 2
    print *, "ok"
end program
