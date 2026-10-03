program data_21
    type ty
        character::t1 = 'aa'
    end type
    type :: ty2
        integer :: x
        character, pointer :: ptr2
        character, pointer :: ptr4
    end type ty2
    integer,pointer::ptr1
    character,pointer::ptr2, ptr3
    integer::k,l
    character,target::t1="a"
    type(ty),target,save ::obj
    data k,ptr1,ptr2,l,ptr3 /10,NULL(), t1,20, obj%t1/
    if(ptr2 /= "a") error stop
    if(ptr3 /= "a") error stop
    if(associated(ptr1) .neqv. .false.) error stop
    if(k /= 10) error stop
    if(l /= 20) error stop
    call s1()
    print*,"PASS"

contains

    subroutine s1()

        character, pointer :: ptr
        integer,   pointer :: ptr3
        character, pointer :: ptr6
        character, pointer :: ptr7

        type(ty2) :: obj2

        character, target, save :: t1 = "a"
        integer,   target, save :: t2 = 20

        integer :: k

        data ptr        / NULL() /
        data ptr3       / t2     /
        data obj2%ptr2  / t1     /
        data ptr6       / t1     /
        data k          / 4      /
        data obj2%ptr4  / t1     /
        data obj2%x     / 23     /
        data ptr7       / t1     /

        if (k /= 4) error stop
        if (obj2%x /= 23) error stop
        if (associated(ptr)      .neqv. .false.) error stop "102"
        if (associated(ptr3)     .neqv. .true.)  error stop "103"
        if (associated(obj2%ptr2).neqv. .true.)  error stop "104"
        if (associated(ptr6)     .neqv. .true.)  error stop "105"
        if (associated(obj2%ptr4).neqv. .true.)  error stop "106"
        if (associated(ptr7)     .neqv. .true.)  error stop "107"
        if (ptr3        /= 20)   error stop "108"
        if (obj2%ptr2   /= "a")  error stop "109"
        if (obj2%ptr4   /= "a")  error stop "110"
        if (ptr6        /= "a")  error stop "111"
        if (ptr7        /= "a")  error stop "112"
    end subroutine s1

end program