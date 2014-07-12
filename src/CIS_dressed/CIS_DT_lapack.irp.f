program CIS_DT
 implicit none
 integer :: i
 print*,'MP2_dresssing=',mp2_dressing
 print*,'standard_doubles=',standard_doubles
 print*,'n_state_CIS=',n_state_CIS
 print*,'n_core_cis=',n_core_cis
 print*,'n_act_cis=',n_act_cis
 print*,''
 print*,'nuc repulsion E=',nuclear_repulsion
if (mp2_dressing==.true.) then
 print*,'correlation E=',MP2_corr_energy
else
 print*,'correlation E=',EN2_corr_energy
endif
 do i = 1,n_state_CIS
   print*,''
   print*,'i = ',i
   print*,'CIS     = ',eigenvalues_CIS(i)
   print*,'CIS(DdT)= ',eigenvalues_CIS_dress_D_dt(i)
   print*,'s2(DdT)    = ',s_2_CIS_dress_D_dt(i)
   print*,'<x>   = ',CIS_states_properties(1,i)
   print*,'<y>   = ',CIS_states_properties(2,i)
   print*,'<z>   = ',CIS_states_properties(3,i)
   print*,'<xx>  = ',CIS_states_properties(4,i)
   print*,'<yy>  = ',CIS_states_properties(5,i)
   print*,'<zz>  = ',CIS_states_properties(6,i)
   print*,''
 enddo
 double precision :: delta_E_CIS,delta_E_CIS_DT,convert

 convert = 1.d0
 print*,'Excitation energies :      CIS           CIS_DT         (Hartree)' 
 do i = 2, n_state_CIS
  delta_E_CIS = eigenvalues_CIS(i) - eigenvalues_CIS(1)
  delta_E_CIS_DT = eigenvalues_CIS_dress_D_dt(i) - eigenvalues_CIS_dress_D_dt(1)
  write(*,'(I3,xxxxxxxxxxxxxxxx,5(F16.5,x))')i,delta_E_CIS*convert,delta_E_CIS_DT*convert
 enddo

 convert = 27.2114d0
 print*,'Excitation energies :      CIS           CIS_DT         (eV)' 
 do i = 2, n_state_CIS
  delta_E_CIS = eigenvalues_CIS(i) - eigenvalues_CIS(1)
  delta_E_CIS_DT = eigenvalues_CIS_dress_D_dt(i) - eigenvalues_CIS_dress_D_dt(1)
  write(*,'(I3,xxxxxxxxxxxxxxxx,5(F16.6,x))')i,delta_E_CIS*convert,delta_E_CIS_DT*convert
 enddo


 convert = 219475d0
 print*,'Excitation energies :      CIS           CIS_DT         (cm-1)' 
 do i = 2, n_state_CIS
  delta_E_CIS = eigenvalues_CIS(i) - eigenvalues_CIS(1)
  delta_E_CIS_DT = eigenvalues_CIS_dress_D_dt(i) - eigenvalues_CIS_dress_D_dt(1)
  write(*,'(I3,xxxxxxxxxxxxxxxx,5(F16.1,x))')i,delta_E_CIS*convert,delta_E_CIS_DT*convert
 enddo

 convert = 627.51d0
 print*,'Excitation energies :      CIS           CIS_DT         (Kcal/mol)' 
 do i = 2, n_state_CIS
  delta_E_CIS = eigenvalues_CIS(i) - eigenvalues_CIS(1)
  delta_E_CIS_DT = eigenvalues_CIS_dress_D_dt(i) - eigenvalues_CIS_dress_D_dt(1)
  write(*,'(I3,xxxxxxxxxxxxxxxx,5(F16.5,x))')i,delta_E_CIS*convert,delta_E_CIS_DT*convert
 enddo

!if(save_all_dm_cis)then
! call save_all_density_matrix
!endif
end
