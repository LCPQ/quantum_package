subroutine sym_apply_inversion(point_in, point_out)
  implicit none
  double precision, intent(in)   :: point_in(3)
  double precision, intent(out)  :: point_out(3)
  point_out(:) = -point_in(:)
end

subroutine sym_apply_reflexion(iaxis,point_in,point_out)
  implicit none
  integer, intent(in)            :: iaxis
  double precision, intent(in)   :: point_in(3)
  double precision, intent(out)  :: point_out(3)
  point_out(:) = point_in(:)
  point_out(iaxis) = -point_in(iaxis)
  
end

subroutine sym_apply_diagonal_reflexion(angle,iaxis,point_in,point_out)
  implicit none
  integer, intent(in)            :: iaxis
  double precision, intent(in)   :: point_in(3), angle
  double precision, intent(out)  :: point_out(3)
  double precision :: point_tmp1(3), point_tmp2(3)
  integer :: iaxis2
  iaxis2 = mod(iaxis,3)+1
!  iaxis2 = mod(iaxis2,3)+1
  call sym_apply_rotation(-angle,iaxis,point_in,point_tmp1)
  call sym_apply_reflexion(iaxis2,point_tmp1,point_tmp2)
  call sym_apply_rotation(angle,iaxis,point_tmp2,point_out)
end

subroutine sym_apply_rotation(angle,iaxis,point_in,point_out)
  implicit none
  double precision, intent(in)   :: angle
  integer, intent(in)            :: iaxis
  double precision, intent(in)   :: point_in(3)
  double precision, intent(out)  :: point_out(3)
  double precision               :: theta, sin_t, cos_t
  double precision, parameter    :: pi = dacos(-1.d0)
  theta = 2.d0*pi/angle
  sin_t = dsin(theta)
  cos_t = dcos(theta)
  if (iaxis==1) then
    point_out(1) = point_in(1)
    point_out(2) = point_in(2)*cos_t - point_in(3)*sin_t
    point_out(3) = point_in(2)*sin_t + point_in(3)*cos_t
  else if (iaxis==2) then
    point_out(1) = point_in(1)*cos_t - point_in(3)*sin_t
    point_out(2) = point_in(2)
    point_out(3) = point_in(1)*sin_t + point_in(3)*cos_t
  endif
  if (iaxis==3) then
    point_out(1) = point_in(1)*cos_t - point_in(2)*sin_t
    point_out(2) = point_in(1)*sin_t + point_in(2)*cos_t
    point_out(3) = point_in(3)
  endif
end

subroutine sym_apply_improper_rotation(angle,iaxis,point_in,point_out)
  implicit none
  double precision, intent(in)   :: angle
  integer, intent(in)            :: iaxis
  double precision, intent(in)   :: point_in(3)
  double precision, intent(out)  :: point_out(3)
  double precision               :: point_tmp(3)
  call sym_apply_rotation(angle,iaxis,point_in,point_tmp)
  call sym_apply_reflexion(iaxis,point_tmp,point_out)
end


