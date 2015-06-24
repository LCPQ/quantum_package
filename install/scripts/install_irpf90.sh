#!/bin/bash -x

TARGET=irpf90
function _install()
{
  cd ..
  QP_ROOT=$PWD
  cd -

  make -C ${BUILD} || return 1
  rm -rf -- ./irpf90 
  mv ${BUILD} .  || return 1
  [[ -x ./irpf90/bin/irpf90 ]] || return 1
  [[ -x ./irpf90/bin/irpman ]] || return 1
  rm -rf -- ../bin/irpf90 ../bin/irpman
  cat << EOF > ../bin/irpf90 || return 1
#!/bin/bash
exec \${QP_ROOT}/install/irpf90/bin/irpf90 \$@
EOF

  
  cat << EOF > ../bin/irpman || return 1
#!/bin/bash
exec \${QP_ROOT}/install/irpf90/bin/irpman \$@
EOF
  chmod +x ../bin/irpf90 ../bin/irpman || return 1
  return 0
}

source scripts/build.sh


