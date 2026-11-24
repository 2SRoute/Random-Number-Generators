program test
    use rng
    implicit none
    real(kind = srk), parameter :: A = 1570, B = 2864, M = 2**32 - 1
    real(kind = srk) :: X = 2007
    integer, parameter :: N = 10000
    integer :: i

    do i = 1, N
        X = lin_cong(X, A, B, M)
    end do
end program