module rng
    implicit none
    integer, parameter :: srk = selected_real_kind(15, 300)

contains
function lin_cong(X, A, B, M)
    implicit none
    real(kind = srk), intent(inout) :: X ! seed value
    real(kind = srk), intent(in) :: A, B, M ! A, B = for determining equation of line of numbers
    real(kind = srk) :: lin_cong

    lin_cong = mod(A*X + B, M) ! Returns the 'remainder' of the number divided by M
end function