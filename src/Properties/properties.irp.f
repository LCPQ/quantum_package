BEGIN_PROVIDER [double precision, average_position,(3)]
 implicit none
 BEGIN_DOC
 ! average_position(1) = <psi_det|X|psi_det>
 ! average_position(2) = <psi_det|Y|psi_det>
 ! average_position(3) = <psi_det|Z|psi_det>
 END_DOC
 integer :: i,j
 average_position = 0.d0
 !$OMP PARALLEL DO DEFAULT(none) &
 !$OMP PRIVATE(i,j) &
 !$OMP SHARED(mo_tot_num,mo_dipole_x,mo_dipole_y,mo_dipole_z,one_body_dm_mo) &
 !$OMP REDUCTION(+:average_position)
 do i = 1, mo_tot_num
  do j = 1, mo_tot_num
   average_position(1) += one_body_dm_mo(j,i) * mo_dipole_x(j,i)
   average_position(2) += one_body_dm_mo(j,i) * mo_dipole_y(j,i)
   average_position(3) += one_body_dm_mo(j,i) * mo_dipole_z(j,i)
  enddo
 enddo
 !$OMP END PARALLEL DO
!call test_average_value(mo_dipole_z,average_position(3))
 
END_PROVIDER


BEGIN_PROVIDER [double precision, average_spread,(3)]
 implicit none
 BEGIN_DOC
 ! average_spread(1) = <psi_det|X^2|psi_det>
 ! average_spread(2) = <psi_det|Y^2|psi_det>
 ! average_spread(3) = <psi_det|Z^2|psi_det>
 END_DOC
 integer :: i,j
 average_spread = 0.d0
 !$OMP PARALLEL DO DEFAULT(none) &
 !$OMP PRIVATE(i,j) &
 !$OMP SHARED(mo_tot_num,mo_spread_x,mo_spread_y,mo_spread_z,one_body_dm_mo) &
 !$OMP REDUCTION(+:average_spread)
 do i = 1, mo_tot_num
  do j = 1, mo_tot_num
   average_spread(1) += one_body_dm_mo(j,i) * mo_spread_x(j,i)
   average_spread(2) += one_body_dm_mo(j,i) * mo_spread_y(j,i)
   average_spread(3) += one_body_dm_mo(j,i) * mo_spread_z(j,i)
  enddo
 enddo
 !$OMP END PARALLEL DO
!call test_average_value(mo_spread_z,average_spread(3))
 
END_PROVIDER

