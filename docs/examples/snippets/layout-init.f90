call bar%initialize(template='{prefix} {percent:yellow} [{bar:20}] {count} files, {eta:blue} left', &
                    prefix_string='load', max_value=real(n, R8P))
