SUBMODULE (nopf_mod) nopf_exec
    CONTAINS
    MODULE PROCEDURE nopf
    USE simul_mod
    INTEGER :: i,j,k,s,error
    REAL, ALLOCATABLE, DIMENSION(:,:) :: a
    REAL, ALLOCATABLE, DIMENSION(:) :: b

    ALLOCATE(a(n+1,n+1))
    ALLOCATE(b(n+1))
    s = SIZE(x)
    a = 0.
    b = 0.
    DO i=1,n+1
        DO j=i,n+1
            DO k=1,s
                a(i,j) = a(i,j)+x(k)**(i+j-2)
                a(j,i) = a(i,j)
            END DO
        END DO
    END DO
    DO i=1,n+1
        DO k=1,s
            b(i) = b(i)+x(k)**(i-1)*y(k)
        END DO
    END DO
    CALL simul(a,b,c,error)
    END PROCEDURE
END SUBMODULE
                