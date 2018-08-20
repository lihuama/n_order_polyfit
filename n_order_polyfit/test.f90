PROGRAM test_nopf
USE nopf_mod
IMPLICIT NONE
    INTEGER, PARAMETER :: MAX_SIZE = 1000
    REAL, ALLOCATABLE, DIMENSION(:) :: x,y,c,yf
    REAL, DIMENSION(MAX_SIZE) :: xt,yt
    INTEGER :: n,i,j,k,istat
    CHARACTER(len=20) :: filename
    CHARACTER(len=80) :: msg
    
    WRITE (*,*) 'Enter the filename:'
    READ (*,'(A20)') filename
    
    OPEN( UNIT=12, FILE=filename, STATUS='OLD', ACTION='READ', IOSTAT=istat, IOMSG=msg)
    IF( istat==0 ) THEN
        i = 1
        DO
            READ (12,*,IOSTAT=istat) xt(i),yt(i)
            IF(istat/=0) EXIT
            i = i+1
        END DO
!        IF(i>MAX_SIZE) WRITE (*,*) 'data overflowed'
        i = i-1
        x = xt(1:i)
        y = yt(1:i)
        
        WRITE (*,'(2X,A,3X,A,6X,A)') 'count','x','y'
        k = 1
        DO j=1,i
            WRITE (*,'(I7,2F7.3)') k,x(j),y(j)
            k = k+1
        END DO
        
        WRITE (*,*) 'Enter the order n:'
        READ (*,*) n

        CALL nopf(x,y,c,n)
        DO k=1,n+1
            WRITE (*,100) 'c(',k-1,')',c(k)
        END DO
100     FORMAT(/,2X,A2,I0,A,3X,F8.3)   
        
        ALLOCATE(yf(i))
        yf = 0.
        DO j=1,i
            DO k=1,n+1
                yf(j) = yf(j)+c(k)*x(j)**(k-1)
            END DO
        END DO
        
        WRITE (*,101) 'x','y','yf','delta y'
101     FORMAT(/,3(4X,A2,4X),1X,A8)
        DO j=1,i
            WRITE (*,102) x(j),y(j),yf(j),ABS(y(j)-yf(j))
        END DO
102     FORMAT(3(F10.4),F10.4)
        
    ELSE
        WRITE (*,*) TRIM(msg)
    END IF
    
END PROGRAM