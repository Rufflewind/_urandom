import os

'''A file object to `/dev/null`.'''
DEV_NULL = open(os.devnull, "r+")

def read_file(filename):
    '''Read a file and return its contents as a string.'''
    with open(filename) as f:
        return f.read()

def write_file(filename, contents, only_if_different=False, safe=True):
    '''Write a file with the given string as contents.

    If `safe` is `True`, the operation is done "as atomically as possible": if
    the filesystem supports atomic rename then it will be performed
    atomically; if the filesystem does not, it will, in the worst case, leave
    behind a temporary file with a similar name that contains the new
    contents.'''

    # encode `contents` into bytes, but only on Python 3
    contents_encode = getattr(contents, "encode", None)
    if contents_encode and getattr(contents, "decode", None):
        import locale
        contents = contents_encode(locale.getpreferredencoding(False))

    # write to temporary file first
    import os, tempfile
    name, ext = os.path.splitext(os.path.basename(filename))
    with tempfile.NamedTemporaryFile(dir=os.path.dirname(filename),
                                     prefix=name + ".",
                                     suffix=ext + ".tmp",
                                     delete=False) as tmp:
        tmp_fn = tmp.name
        try:
            tmp.file.write(contents)
        except:
            os.remove(tmp_fn)
            raise

    # overwrite target file with temporary file
    try:
        os.rename(tmp_fn, filename)
    except OSError:
        import shutil
        os.remove(filename)
        os.rename(tmp_fn, filename)

class WorkDir(object):
    '''A context for changing to a different working directory.  The original
    working directory is restored upon exiting the context.'''

    def __init__(self, path):
        '''Initialize a context for changing the working directory.'''
        self.path = path

    def __enter__(self):
        '''Change the working directory and return the path to the previous
        working directory.'''
        import os
        self.prevdir = os.getcwd()
        os.chdir(self.path)
        return self.prevdir

    def __exit__(self, type, value, traceback):
        '''Restore the the temporary directory.'''
        import os
        os.chdir(self.prevdir)

class FileLock(object):
    '''A context for holding an exclusive file lock, which is released upon
    exiting the context.  (Unix only.)'''

    def __init__(self, path, block=True):
        '''Initialize a context for holding an exclusive file lock.'''
        self.path  = path
        self.block = block

    def __enter__(self):
        '''Acquire the lock.  Returns nothing.  If `block` was set to true,
        then an `IOError` is raised if the file is already locked.'''
        import fcntl
        flags = fcntl.LOCK_EX
        if not self.block:
            flags |= fnctl.LOCK_NB
        self.lockfile = open(self.path, "w")
        try:
            fcntl.lockf(self.lockfile, flags)
        except:
            self.lockfile.close()
            raise

    def __exit__(self, type, value, traceback):
        '''Release the lock.'''
        self.lockfile.close()
