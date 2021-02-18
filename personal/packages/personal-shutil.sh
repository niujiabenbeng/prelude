#! /bin/bash

### used for `personal-neotree-get-binary-files-sh`
### $1: the root neotree directory
### $2: list of regexps separated by whitespace
### example: elisp_get_binary_files "/home/chenli/.emacs.d" "elpa anaconda-mode"
elisp_get_binary_files() {
    local root=$1
    local patterns=$2
    for name in $(ls ${root}); do
        for pattern in $(echo ${patterns}); do
            if [[ ${name} =~ ${pattern} ]]; then
                continue 2
            fi
        done
        if [[ -d ${root}/${name} ]]; then
            elisp_get_binary_files ${root}/${name} "${2}"
        elif [[ ! $(file -b --mime-type ${root}/${name}) = "text"* ]]; then
            echo ${name}
        fi
    done
}
