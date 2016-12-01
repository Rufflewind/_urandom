# Using a file system via the loop device

Before doing any of these steps, make sure the `loop` kernel module can be successfully loaded:

    sudo modprobe loop && echo OK

## Initial creation

 1. Create a blank file with the correct size:

        dd if=/dev/zero of=<image-file> bs=1M count=<number-of-mebibytes>

    Note: the command can be dangerous if done incorrectly.  Double-check!

 2. Attach the file to an available loop device:

        sudo losetup --show -f <image-file>

    The command will also print the name of the loop device in the form of `/dev/loop<N>` where `<N>` is some number.

 3. Format the loop device:

        sudo mkfs.ext4 /dev/loop<N>

    You can replace `ext4` with whatever you like.

 4. Detach the file from the loop device:

        sudo losetup -d /dev/loop<N>

Continue onto “Mounting” for instructions on how to mount the image file.

## Mounting

    sudo mount -o loop <image-file> <mount-path>

where `<mount-path>` should be an existing, empty directory.  The `-o loop` flag will automatically attach the `<image-file>` to an available loop device.

## Unmounting

    sudo umount <mount-path>

If `-o loop` was specified earlier, then it will automatically detach the image file from the loop device as well.
