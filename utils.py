import os

'''A file object to `/dev/null`.'''
DEV_NULL = open(os.devnull, "r+")

# The use of this function is questionable.
# It might be deprecated in the future.
ensure_str_encoding = []
def ensure_str(string):
    '''Ensure that the argument is in fact a Unicode string.  If it isn't,
    then:

      - on Python 2, it will be decoded using the preferred encoding;
      - on Python 3, it will trigger a `TypeError`.
    '''
    # Python 2
    if getattr(str, "decode", None) and getattr(str, "encode", None):
        if isinstance(string, unicode):
            return string
        if not ensure_str_encoding:
            import locale
            ensure_str_encoding.append(locale.getpreferredencoding(False))
        return string.decode(ensure_str_encoding[0])
    # Python 3
    if isinstance(string, str):
        return string
    raise TypeError("not an instance of 'str': " + repr(string))

#@snip/TemporarySaveFile[
#@requires: rename try_remove wrapped_open
class TemporarySaveFile(object):
    '''A context manager for a saving files atomically.  The context manager
    creates a temporary file to which data may be written.  If the body of the
    `with` statement succeeds, the temporary file is renamed to the target
    filename, overwriting any existing file.  Otherwise, the temporary file is
    deleted.'''

    def __init__(self, filename, mode="w", suffix=None, prefix=None, **kwargs):
        import os
        self._fn = filename
        kwargs = dict(kwargs)
        kwargs.update({
            "mode": mode,
            "suffix": ".tmpsave~" if suffix is None else suffix,
            "prefix": (".#" + os.path.basename(filename)).rstrip(".") + "."
                      if prefix is None else prefix,
            "dir": os.path.dirname(filename),
            "delete": False,
        })
        self._kwargs = kwargs

    def __enter__(self):
        import shutil, tempfile
        if hasattr(self, "_stream"):
            raise ValueError("attempted to __enter__ twice")
        stream = wrapped_open(tempfile.NamedTemporaryFile, **self._kwargs)
        try:
            shutil.copystat(self._fn, stream.name)
        except:
            try:
                stream.close()
            finally:
                try_remove(stream.name)
            raise
        self._stream = stream
        return stream

    def __exit__(self, exc_type, exc_value, traceback):
        try:
            self._stream.close()
            if exc_type is None and exc_value is None and traceback is None:
                rename(self._stream.name, self._fn)
            else:
                try_remove(self._stream.name)
        except:
            try_remove(self._stream.name)
            raise
        finally:
            del self._stream
#@]

#@snip/rename[
def rename(src, dest):
    '''Rename a file (allows overwrites on Windows).'''
    import os
    if os.name == "nt":
        import ctypes, ctypes.wintypes
        MoveFileExW = ctypes.windll.kernel32.MoveFileExW
        MoveFileExW.restype = ctypes.wintypes.BOOL
        MOVEFILE_REPLACE_EXISTING = ctypes.wintypes.DWORD(0x1)
        success = MoveFileExW(ctypes.wintypes.LPCWSTR(src),
                              ctypes.wintypes.LPCWSTR(dest),
                              MOVEFILE_REPLACE_EXISTING)
        if not success:
            raise ctypes.WinError()
    else:
        os.rename(src, dest)
#@]

#@snip/try_remove[
def try_remove(path):
    import os
    try:
        os.remove(path)
    except OSError:
        return False
    return True
#@]

#@snip/wrapped_open[
def wrapped_open(open, mode="r", encoding=None,
                 errors=None, newline=None, **kwargs):
    '''Enhance an `open`-like function to accept some additional arguments for
    controlling the text processing.  This is mainly done for compatibility
    with Python 2, where these additional arguments are often not accepted.'''
    if "b" in mode:
        if encoding is not None:
            raise Exception("'encoding' argument not supported in binary mode")
        if errors is not None:
            raise Exception("'errors' argument not supported in binary mode")
        if newline is not None:
            raise Exception("'newline' argument not supported in binary mode")
        return open(mode=mode, **kwargs)
    else:
        import io
        mode = mode.replace("t", "") + "b"
        stream = open(mode=mode, **kwargs)
        try:
            return io.TextIOWrapper(stream, encoding=encoding,
                                    errors=errors, newline=newline)
        except:
            stream.close()
            raise
#@]

#@snip/safe_open[
#@requires: TemporarySaveFile
def safe_open(filename, mode="rt", encoding=None,
              errors=None, newline=None, safe=True):
    truncated_write = "w" in mode and "+" not in mode
    if safe and truncated_write and not isinstance(filename, int):
        open_file = TemporarySaveFile
    else:
        from io import open as open_file
    return open_file(filename, mode, encoding=encoding,
                     errors=errors, newline=newline)
#@]

#@snip/load_file[
def load_file(filename, binary=False, encoding=None,
              errors=None, newline=None):
    '''Read the contents of a file.'''
    from io import open
    mode = "r" + ("b" if binary else "")
    with open(filename, mode, encoding=encoding,
              errors=errors, newline=newline) as stream:
        return stream.read()
#@]

#@snip/save_file[
#@requires: safe_open
def save_file(filename, contents, binary=False, encoding=None,
              errors=None, newline=None, safe=True):
    '''Write the contents to a file.  If `safe` is true, it is performed by
    first writing into a temporary file and then replacing the original file
    with the temporary file.  This ensures that the file will not end up in a
    half-written state.  Note that there is a small possibility that the
    temporary file might remain if the program crashes while writing.'''
    mode = "w" + ("b" if binary else "")
    with safe_open(filename, mode, encoding=encoding,
                   errors=errors, newline=newline, safe=safe) as stream:
        stream.write(contents)
#@]

#@snip/save_json_file[
#@requires: safe_open
def save_json_file(filename, contents, encoding=None,
                   errors=None, newline=None, safe=True):
    import json
    with safe_open(filename, "wt", encoding=encoding,
                   errors=errors, newline=newline, safe=safe) as stream:
        json.dump(contents, stream)
#@]

#@snip/load_json_file[
def load_json_file(filename, encoding=None, errors=None, newline=None):
    import json
    from io import open
    with open(filename, "rt", encoding=encoding,
              errors=errors, newline=newline) as stream:
        return json.load(stream)
#@]

#@snip/WorkDir[
class WorkDir(object):
    '''A context manager for changing to a different working directory.  The
    original working directory is restored upon exiting the context.'''

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
#@]

#@snip/FileLock[
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
#@]

#@snip/LookaheadIterator[
class LookaheadIterator(object):
    '''This serves a similar purpose to `itertools.tee` but is less prone to
    space leakage.  It acts much like the original iterator, except that you
    are allowed to peek ahead by an arbitrary amount.'''

    def __init__(self, iterable):
        '''Construct a `LookaheadIterator` from an existing iterator.  Once
        created, one should generally avoid using the original iterator, as it
        could cause `LookaheadIterator` to miss out on some elements.'''
        self.iterator = iter(iterable)
        self.buffer = []

    def __iter__(self):
        return self

    def __next__(self):
        if self.buffer:
            return self.buffer.pop()
        return next(self.iterator)

    def next(self):
        return self.__next__()

    def peek(self, n=1):
        '''Peek up to `n` elements.  The function may return fewer items if
        there aren't enough elements.'''
        import itertools
        unbuffered = n - len(self.buffer)
        results = list(itertools.chain(
            reversed(self.buffer[max(0, -unbuffered):]),
            itertools.islice(self.iterator, max(0, unbuffered)),
        ))
        if unbuffered >= 0:
            self.buffer = list(reversed(results))
        return results
#@]

#@test/LookaheadIterator[
def LookaheadIterator_test():
    success = [True]

    def compare(x, y):
        if isinstance(x, list):
            x = tuple(x)
        if isinstance(y, list):
            y = tuple(y)
        if x != y:
            print("***error***", x, y)
            success[0] = False

    l = [1,2,3,4,5,6,7]
    i = LookaheadIterator(iter(l))
    compare(i.peek(), [1])
    compare(i.peek(), [1])
    compare(i.next(), 1)
    compare(i.peek(), [2])
    compare(i.peek(), [2])
    compare(i.peek(2), [2, 3])
    compare(i.peek(2), [2, 3])
    compare(i.peek(), [2])
    compare(i.peek(3), [2, 3, 4])
    compare(i.next(), 2)
    compare(i.next(), 3)
    compare(i.peek(3), [4, 5, 6])
    compare(i.peek(9), [4, 5, 6, 7])
    compare(i.peek(9), [4, 5, 6, 7])
    compare(i.peek(3), [4, 5, 6])
    compare(i.peek(3), [4, 5, 6])
    compare(i.peek(9), [4, 5, 6, 7])
    compare(i.next(), 4)
    compare(i.next(), 5)
    compare(i.next(), 6)
    compare(i.next(), 7)

    assert success[0]
#@]

#@snip/combine_surrogate_pair[
def combine_surrogate_pair(l, r):
    '''Example: chr(combine_surrogate_pair(55357, 56832))
    (use 'unichr' on Python 2)'''
    return ((l - 0xd800) << 10) + r - 0xdc00 + 0x010000
#@]
