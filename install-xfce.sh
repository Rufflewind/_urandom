#!/bin/sh
# Locally install XFCE4 on x86-64 Debian Jessie
set -eux

# feel free to customize these parameters
CFLAGS="-O2 -pipe"; export CFLAGS
MAKEFLAGS="-j4"; export MAKEFLAGS
prefix=$HOME/.local/opt/xfce-4.12

# don't touch these parameters
PATH=$prefix/bin:$PATH
PKG_CONFIG_PATH=$prefix/lib/x86_64-linux-gnu/pkgconfig:$prefix/lib/pkgconfig${PKG_CONFIG_PATH+:}${PKG_CONFIG_PATH-}; export PKG_CONFIG_PATH

mkdir -p "$prefix"
cd "$prefix"

curl -fL http://archive.xfce.org/xfce/4.12/fat_tarballs/xfce-4.12.tar.bz2 | bunzip2 | tar x
cd src

# workaround lack of libudev-dev package
curl -fL http://http.us.debian.org/debian/pool/main/s/systemd/libudev-dev_215-17+deb8u7_amd64.deb | dpkg -x /dev/stdin libudev-dev_215-17+deb8u7_amd64
cp -pr libudev-dev_215-17+deb8u7_amd64/usr/* "$prefix"
sed -i.bak 's:^prefix=.*:prefix='"$prefix"':;s:^includedir=.*:includedir=${prefix}/include:;s:^libdir=.*:libdir=${prefix}/lib/x86_64-linux-gnu:' "$prefix/lib/x86_64-linux-gnu/pkgconfig/libudev.pc"

# workaround lack of libdbus-1-dev package
curl -fL http://http.us.debian.org/debian/pool/main/d/dbus/libdbus-1-dev_1.8.22-0+deb8u1_amd64.deb | dpkg -x /dev/stdin libdbus-1-dev_1.8.22-0+deb8u1_amd64
cp -pr libdbus-1-dev_1.8.22-0+deb8u1_amd64/usr/* "$prefix"
sed -i.bak 's:^prefix=.*:prefix='"$prefix"':;s:^libdir=.*:libdir=${prefix}/lib/x86_64-linux-gnu:' "$prefix/lib/x86_64-linux-gnu/pkgconfig/dbus-1.pc"

curl -fL https://launchpad.net/intltool/trunk/0.51.0/+download/intltool-0.51.0.tar.gz | gunzip | tar x
curl -fL https://dbus.freedesktop.org/releases/dbus-glib/dbus-glib-0.108.tar.gz | gunzip | tar x
curl -fL https://download.gnome.org/sources/libgudev/232/libgudev-232.tar.xz | unxz | tar x
curl -fL https://download.gnome.org/sources/libwnck/2.31/libwnck-2.31.0.tar.xz | unxz | tar x

for pkg in intltool dbus-glib libwnck libgudev libxfce4util xfconf libxfce4ui garcon exo xfce4-panel thunar xfce4-settings xfce4-session xfdesktop xfwm4 xfce4-appfinder; do
    for tarball in "$pkg"-*.tar.bz2; do
        if [ -f "$tarball" ]; then
            bunzip2 <"$tarball" | tar x
            rm "$tarball"
        fi
    done
    (
        cd "$pkg"-*
        case $pkg in
            libgudev) ./configure --prefix="$prefix" --disable-umockdev;;
            xfce4-session) ./configure --prefix="$prefix" --with-xsession-prefix="$prefix/share/xsessions";;
            *) ./configure --prefix="$prefix";;
        esac
        make
        make install
    )
done
