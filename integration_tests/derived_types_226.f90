module derived_types_226_mod
    implicit none
    type config_type
        character :: names(2) = ['', '']
        character :: extra = ''
    end type
    type words_type
        character(len=3) :: words(3) = ['ab ', 'cde', 'f  ']
        integer :: n = 7
        character(len=2) :: tag = 'q'
    end type
    type(config_type) :: config
    type(words_type) :: w
end module

program derived_types_226
    use derived_types_226_mod
    implicit none
    if (len(config%names) /= 1 .or. size(config%names) /= 2) error stop 1
    if (iachar(config%names(1)) /= 32 .or. iachar(config%names(2)) /= 32) error stop 2
    if (len(config%extra) /= 1 .or. iachar(config%extra) /= 32) error stop 3
    if (w%words(1) /= 'ab' .or. w%words(2) /= 'cde' .or. w%words(3) /= 'f') error stop 4
    if (w%n /= 7 .or. w%tag /= 'q ' .or. len(w%tag) /= 2) error stop 5
    config%names(2) = 'z'
    w%words(2) = 'xyz'
    if (config%names(1) /= ' ' .or. config%names(2) /= 'z') error stop 6
    if (w%words(1) /= 'ab' .or. w%words(2) /= 'xyz' .or. w%words(3) /= 'f') error stop 7
    print *, "[", config%names, "][", config%extra, "]"
    print *, w%words, w%n, w%tag
end program
