__iscfpc_tool_complete() {
  typeset -a commands
  commands+=(
    'doctor[Show information about the installed tooling.]'
    'clean[Delete the lib/ and $arch/ directories.]'
    'build[Build executable apps and libraries.]'
    'create[Create a new iSyscore FPC project.]'
    'upgrade[Upgrade the "iscbase" dependencies for your app.]'
    'ocean[Show or download fpc sample code in Code Ocean.]'
  )
  if (( CURRENT == 2 )); then
    # explain go commands
    _values 'iscfpc commands' ${commands[@]}
    return
  fi

  case ${words[2]} in
  doctor)
    if (( CURRENT == 3 )); then
      _values "iscfpc doctor" "fix[Fix VSCode config problem]"
      return
    fi
    ;;
  build)
    if (( CURRENT == 3 )); then
      _values "iscfpc build" \
        "ALPINE[Build executable apps and libraries work for Alpine, (Linux host only)]" \
        $(ls *.lpi)
      return
    fi
    ;;
  create)
    if (( CURRENT == 3 )); then
      _values "iscfpc create" \
        "web[Web application based on FPWeb]" \
        "console[simple console application]" \
        "lib[simple shared library]" \
        "jni[shared library for JVM using]"
      return
    fi
    _values "iscfpc create project" \
      ".[current directory]" \
      $(ls -d */)
    ;;
  esac
}

compdef __iscfpc_tool_complete iscfpc

