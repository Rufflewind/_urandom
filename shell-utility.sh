# download a file from the given URL
#
# inputs:
#   - 1:            URL
#
# output:
#   - /dev/stdout:  data of downloaded file
#
download_tool=
download() {
    if [ $# -ne 1 ]
    then
        echo >&2 "download: expected one argument"
        return 1
    fi

    # figure out which tool we should use to download files
    [ "$download_tool" ] || {
        if command >/dev/null 2>&1 -v curl
        then
            download_tool=curl
        elif command >/dev/null 2>&1 -v wget
        then
            download_tool=wget
        else
            download_tool=none
        fi
    }

    # call the download tool
    case $download_tool in
        curl) curl -fsLS    -- "$1";;
        wget) wget -nv -O - -- "$1";;
        *)    echo >&2 "download: either `curl` or `wget` must be installed"
              return 1;;
    esac
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

# check whether `stringB` is a prefix of `stringA` and
# obtain the length of `stringB`
#
# inputs:
#   - 1:    `stringA`
#   - 2:    `stringB`
#
# output:
#   - ret:  length of `stringB`
#
not_prefix_of() {
    _1=`printf '%s' "$1" | wc -c`
    ret=`printf '%s' "$2" | wc -c`
    # if stringB is longer, return unchanged
    if [ "$_1" -lt "$ret" ]
    then
        return
    fi
    # check whether stringB is a prefix of stringA
    _1=`printf '%s' "$1" | dd 2>/dev/null bs=1 count="$ret"`
    [ "$_1" != "$2" ]
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
        ret="$1"
    else
        # obtain substring
        ret=`expr "$ret" + 1`
        ret=`printf '%s' "$1" | tail -c +"$ret"`
    fi
}

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
        printf 'sed "s/^ //" <<"EOF"'
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
    _1=\'`printf "%s" "$_1" | sed "$_2"`\'
    eval "$1"'_=$_1'
}
