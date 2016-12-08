# `init_download_cmd` attempts to define a shell function `download` that
# downloads a file from a given URL.  `init_download_cmd` itself does not
# accept any arguments, although it does use the global variable `UNAME_S`.
#
# Providing `UNAME_S` is optional.  If provided, it must contain the cached
# result of `uname -s`.
#
# The `download` function accepts a single argument, the URL, and yields the
# downloaded file through stdout.
#
# If `download` is already a valid command (or function) then nothing happens.
#
init_download_cmd() {
    if type download >/dev/null 2>&1; then
        return 0
    elif type curl >/dev/null 2>&1; then
        download() {
            curl -fLsS "$1"
        }
    elif type wget >/dev/null 2>&1; then
        download() {
            wget -nv -O - "$1"
        }
    else
        UNAME_S=${UNAME_S-`uname -s || :`}
        if [ x"${UNAME_S}" = xOpenBSD ] && type ftp >/dev/null 2>&1; then
            download() {
                ftp -o - "$1"
            }
        else
            printf >&2 "%s%s\n" "error: unable to download files (can't " \
                'find "curl", "wget" or OpenBSD "ftp")'
            return 1
        fi
    fi
}

# perform a one-sided exponential search to find the
# largest value at which the trial function succeeds
#
# inputs:
#   - 1:    initial value, which must be a power of 2
#           and defines the smallest possible value
#   - try:  trial function that accepts one parameter
#
# output:
#   - ret:  threshold before which the try function fails
#
exp_search() {

    # find a strict upper bound
    _1=$1
    _3=$_1
    _2=$_1
    while try "$_1"
    do
        _3=$_2
        _2=$_1
        _1=`expr "$_1" + "$_2"`
    done

    # not strictly needed, but this optimizes for the special case where the
    # boundary lies on powers of 2 at expense of more general cases
    _1=`expr "$_2" + 1`
    try "$_1" || {
        ret=$_2
        return
    }

    # use binary search to refine the exact boundary
    _1=$_2
    _2=$_3
    _3=$_1
    _1=`expr "$_1" + "$_2"`
    while :
    do
        _2=`expr "$_2" / 2`
        if try "$_1"
        then
            _3=$_1
            _1=`expr "$_1" + "$_2"`
        else
            _1=`expr "$_3" + "$_2"`
        fi
        if [ "$_2" -eq 1 ]
        then
            break
        fi
    done
    if try "$_1"
    then
        ret=$_1
    else
        ret=$_3
    fi
}

_test_exp_search() {
    try() {
        test "$1" -le $max
    }
    max=8
    while [ $max -lt 120 ]
    do
        exp_search 8
        if [ $ret = $max ]
        then :
        else
            printf '%s\n' "** FAIL [got $k, expected $max]"
            exit 1
        fi
        max=`expr $max + 1`
    done
    echo passed
}

# clone `repo` into `dir`, additionally setting `myrepo` as the `source`
# remote if provided
#
#     git_clone <repo> <dir> <myrepo>
#
git_clone() {
    [ -d "$2" ] ||
        git clone -- "$1" "$2"
    [ -z "${3-}" ] ||
        git -C "$2" remote add source "${3-}" || :
    git -C "$2" fetch --all --prune || :
    git -C "$2" submodule update --init --recursive
    [ -z "${3-}" ] ||
        git -C "$2" checkout local || :
}

# check whether `stringB` is not a prefix of `stringA`
#
# inputs:
#   - 1:    `stringA`
#   - 2:    `stringB`
#
not_prefix_of() {
    case $1 in
        "$2"*) return 1;;
        *)     return 0;;
    esac
}

# remove `stringB` from the beginning of `stringA`; if `stringB` is not a
# prefix of `stringA`, `stringA` is returned instead
#
# this is a workaround for the absence of `${var##prefix}` or `${var#prefix}`
# in older shells; note however that this function does not allow patterns
#
# inputs:
#   - 1:    `stringA`
#   - 2:    `stringB`
#
# output:
#   - ret:  `stringA` with possibly `stringB` removed from the beginning
#
strip_prefix() {
    # if the pattern is not a prefix of the original string, return unchanged
    if not_prefix_of "$1" "$2"
    then
        ret=$1
    else
        # obtain substring
        ret=`printf "%s" "$1" | wc -c`
        ret=`expr "$ret" + 1`
        ret=`printf '%s' "$1" | tail -c +"$ret"`
    fi
}

# use more optimal form if supported
if ( _1=abcd && [ ${_1#ab} = cd ] ) >/dev/null 2>&1
then
    strip_prefix() {
        ret=${1#$2}
    }
fi

_test_strip_prefix() {
    check() {
        strip_prefix "$1" "$2"
        exp=`bash -c 'echo "${1##$2}"' _ "$1" "$2"`
        [ "$ret" = "$exp" ] || {
            printf '%s' "fail: [$1, $2] => got [$ret], expected [$exp]"
            exit 1
        }
    }

    check '' ''

    check 1 ''
    check 12 ''
    check 123 ''

    check '' 1
    check 1 1
    check 2 1
    check 12 1
    check 22 1
    check 123 1
    check 223 1

    check '' 12
    check 1 12
    check 2 12
    check 12 12
    check 22 12
    check 13 12
    check 23 12
    check 123 12
    check 223 12
    check 133 12
    check 433 12

    check '' 123
    check 1 123
    check 2 123
    check 12 123
    check 13 123
    check 23 123
    check 123 123
    check 124 123
    check 134 123
    check 234 123
    check 1234 123
    check 1235 123
    check 1245 123
    check 1345 123
    check 2345 123
    echo passed
}

# pack files into a simple shell archive
#
# inputs:
#   - @: paths to files and/or directories (must not contain newlines)
#   - PACK_COMMENT: extra text to be inserted just after first line
#
# note: file modes are not fully preserved
#
shar_pack() {

    # escape paths that begin with hyphen
    i=0
    while [ "$i" -lt "$#" ]
    do  i=`expr "$i" + 1`
        x=$1
        case $x in
            -*) x=./$x;;
        esac
        set -- "$@" "$x"
        shift
    done

    echo '#!/bin/sh'
    printf "%s" "$PACK_COMMENT"
    find "$@" -type f | LC_ALL=C sort |
    while read fn
    do
        dir=`dirname "$fn"`
        patt="s/'/'\\\\''/g"
        escfn=\'`printf "%s" "$fn" | sed "$patt"`\'
        escdir=\'`printf "%s" "$dir" | sed "$patt"`\'

        # find out if there is a trailing newline
        nl=`echo; echo x`
        eofchar=`tail -c 1 "$fn"; echo x`

        # determine an approximate file mode
        # (there's no easy way to get the full mode portably)
        if [ -x "$fn" ]
        then mode=755
        else mode=644
        fi

        # create the directory if necessary
        printf '\nmkdir -p %s\n' "$escdir"

        # dump the file as a heredoc, prefixing each nonempty line with space;
        # if there is no trailing newline, we add one and remove it later
        printf 'LC_ALL=C sed "s/^ //" <<"EOF"'
        if [ "$eofchar" != "$nl" ]
        then
            len=`wc -c <"$fn"`
            printf ' | dd ibs=1 count=%s 2>/dev/null' "$len"
        fi
        printf ' >%s\n' "$escfn"
        LC_ALL=C sed 's/^\(.\)/ \1/' "$fn"
        if [ "$eofchar" != "$nl" ]
        then echo
        fi

        # set the file mode
        printf 'EOF\nchmod %s %s\n' "$mode" "$escfn"
    done
}

# shell-escape the given variable
#
# inputs:
#   - 1: name of the variable
#
# output:
#   - the escaped string is stored in a variable of the same name
#     but suffixed with an underscore
#
shell_escape() {
    eval '_1=$'"$1"
    _2="s/'/'\\\\''/g"
    _1=\'`printf "%s" "$_1" | sed "$_2" && echo "'"`
    eval "$1"'_=$_1'
}
