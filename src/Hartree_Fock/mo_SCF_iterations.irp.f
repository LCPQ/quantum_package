program scf_iteration
  use bitmasks
  implicit none
  double precision               :: SCF_energy_before,SCF_energy_after,diag_H_mat_elem,get_mo_bielec_integral
  double precision               :: E0
  integer                        :: i_it
  
  E0 = HF_energy 
  i_it = 0
  n_it_scf_max = 100
  SCF_energy_before = huge(1.d0)
  SCF_energy_after = E0
  print *,  E0
  mo_label = "Canonical"
  thresh_SCF = 1.d-10
  do while (i_it < 10 .or. dabs(SCF_energy_before - SCF_energy_after) > thresh_SCF)
    SCF_energy_before = SCF_energy_after
    mo_coef = eigenvectors_Fock_matrix_mo
    TOUCH mo_coef mo_label
    call clear_mo_map
    SCF_energy_after = HF_energy
    print*,SCF_energy_after
    i_it +=1
    if(i_it > n_it_scf_max)exit
  enddo
  
  if (i_it == n_it_scf_max) then
    stop 'Failed'
  endif
  if (SCF_energy_after - E0 > thresh_SCF) then
    stop 'Failed'
  endif
  mo_label = "Canonical"
  TOUCH mo_label mo_coef
  call save_mos
  
end
