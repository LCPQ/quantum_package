program save_natorb
  read_wf = .True.
  touch read_wf
  call save_natural_mos
  call save_ref_determinant
  call ezfio_set_integrals_bielec_disk_access_mo_integrals('None')
  call ezfio_set_integrals_monoelec_disk_access_mo_one_integrals('None')
end

