import os, shutil

'''A file object to `/dev/null`.'''
DEV_NULL = open(os.devnull, "r+")

def read_file(filename):
    '''Reads a file and returns its contents as a string.'''
    with open(filename) as f:
        return f.read()

def write_file(filename, contents, only_if_different=False):
    '''Writes a file with the given string as contents.'''
    if only_if_different:
        try:
            old_contents = read_file(filename)
        except Exception:
            old_contents = None
        if contents == old_contents:
            return
    with open(filename, "w") as f:
        f.write(contents)

class TmpFile(object):
    '''A context object representing a temporary file, which is deleted upon
    exiting the context.'''

    def __init__(self, contents, suffix=""):
        '''Represents a temporary file with the given string as contents.'''
        self.contents = contents
        self.suffix   = suffix

    def __enter__(self):
        '''Create the temporary file and return this object: the `fd` member
        contains the file descriptor and `filename` member contains the path
        to the temporary file.'''
        import tempfile
        self.fd, self.filename = tempfile.mkstemp(suffix=self.suffix)
        with os.fdopen(self.fd, "w") as f:
            f.write(self.contents)
        return self

    def __exit__(self, type, value, traceback):
        '''Remove the temporary file.'''
        try:
            os.remove(self.filename)
        except:
            pass

class TmpDir(object):
    '''A context representing a temporary directory, which is deleted upon
    exiting the context.'''

    def __init__(self, *args, **kwargs):
        '''Represents a temporary directory.  The parameters of this function is
        identical to that of `tempfile.mkdtemp`.'''
        self.args   = args
        self.kwargs = kwargs

    def __enter__(self):
        '''Create a temporary directory and return its path.'''
        import tempfile
        self.dirname = tempfile.mkdtemp(*self.args, **self.kwargs)
        return self.dirname

    def __exit__(self, type, value, traceback):
        '''Remove the temporary directory.'''
        shutil.rmtree(self.dirname, ignore_errors=True)

class WorkDir(object):
    '''A context for changing to a different working directory.  The original
    working directory is restored upon exiting the context.'''

    def __init__(self, path):
        '''Represents a working directory.'''
        self.path = path

    def __enter__(self):
        '''Change the working directory and return the path to the previous
        working directory.'''
        self.prevdir = os.getcwd()
        os.chdir(self.path)
        return self.prevdir

    def __exit__(self, type, value, traceback):
        '''Restore the the temporary directory.'''
        os.chdir(self.prevdir)

class FileLock(object):
    '''A context for holding an exclusive file lock, which is released upon
    exiting the context.  (Unix only.)'''

    def __init__(self, path, block=True):
        '''Represents a file lock.'''
        self.path  = path
        self.block = block

    def __enter__(self):
        '''Grabs the file lock.  Returns nothing.  If `block` was set to true,
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
        '''Release the file lock.'''
        self.lockfile.close()
