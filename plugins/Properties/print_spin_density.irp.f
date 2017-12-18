program print_sd
 implicit none
 read_wf = .True.
 touch read_wf
 call routine

end

subroutine routine
 implicit none
 integer :: i,j,k
 double precision :: z
 double precision :: r(3),accu,accu_alpha,accu_beta,tmp
 double precision, allocatable :: aos_array(:)
 allocate(aos_array(ao_num))
 r = 0.d0
 r(spin_dens_coord) = z_min
 do i = 1, N_z_pts
  call give_all_aos_at_r(r,aos_array)
  accu = 0.d0
  accu_alpha = 0.d0
  accu_beta  = 0.d0
  do j = 1, ao_num
   do k = 1, ao_num
    tmp =  aos_array(k) * aos_array(j)
    accu += one_body_spin_density_ao(k,j) * tmp 
    accu_alpha += one_body_dm_ao_alpha(k,j) * tmp 
    accu_beta  += one_body_dm_ao_beta(k,j) * tmp
   enddo
  enddo
  r(spin_dens_coord) += delta_z
  write(33,'(100(f16.10,X))')r(spin_dens_coord),accu,accu_alpha,accu_beta
 enddo
 

end
