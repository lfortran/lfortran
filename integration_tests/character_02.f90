subroutine dgetv0 ( bmat ) 
    character  bmat(5)
    print *, bmat
end
subroutine dnaitr ( bmat )
    character  bmat(5)
    call dgetv0 ( bmat)    
end
program character_02
    character bmat(5)
    bmat = 't'
    call dnaitr ( bmat )
end
