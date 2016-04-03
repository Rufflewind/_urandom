#@slot/imports[
import ctypes, errno, io, json, locale, os, shutil, subprocess, tempfile
if os.name == "nt":
    import ctypes.wintypes
#@]

JSON_ARGS = {
    "indent": 4,
    "separators": (",", ": "),
    "sort_keys": True,
}

#@slot/variables[
#@snip/ensure_str[
#@requires: mod:locale
PREFERREDENCODING = locale.getpreferredencoding(True)
#@]
#@]

# The use of this function is questionable.
# It might be deprecated in the future.
#@slot/functions[
#@snip/ensure_str[
#@requires: PREFERREDENCODING
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
        return string.decode(PREFERREDENCODING)
    # Python 3
    if isinstance(string, str):
        return string
    raise TypeError("not an instance of 'str': " + repr(string))
#@]

#@snip/TemporarySaveFile[
#@requires: mod:errno mod:os mod:shutil mod:tempfile rename try_remove wrapped_open
class TemporarySaveFile(object):
    '''A context manager for a saving files atomically.  The context manager
    creates a temporary file to which data may be written.  If the body of the
    `with` statement succeeds, the temporary file is renamed to the target
    filename, overwriting any existing file.  Otherwise, the temporary file is
    deleted.'''

    def __init__(self, filename, mode="w", suffix=None, prefix=None, **kwargs):
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
        if hasattr(self, "_stream"):
            raise ValueError("attempted to __enter__ twice")
        stream = wrapped_open(tempfile.NamedTemporaryFile, **self._kwargs)
        try:
            shutil.copymode(self._fn, stream.name)
        except BaseException as e:
            if not (isinstance(e, OSError) and e.errno == errno.ENOENT):
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
#@requires: mod:os mod:ctypes mod:ctypes.wintypes
def rename(src, dest):
    '''Rename a file (allows overwrites on Windows).'''
    if os.name == "nt":
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
#@requires: mod:os
def try_remove(path):
    try:
        os.remove(path)
    except OSError:
        return False
    return True
#@]

#@snip/wrapped_open[
#@requires: mod:io
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
#@requires: mod:io TemporarySaveFile
def safe_open(filename, mode="rt", encoding=None,
              errors=None, newline=None, safe=True):
    truncated_write = "w" in mode and "+" not in mode
    if safe and truncated_write and not isinstance(filename, int):
        open_file = TemporarySaveFile
    else:
        open_file = io.open
    return open_file(filename, mode, encoding=encoding,
                     errors=errors, newline=newline)
#@]

#@snip/load_file[
#@requires: mod:io
def load_file(filename, binary=False, encoding=None,
              errors=None, newline=None):
    '''Read the contents of a file.'''
    mode = "r" + ("b" if binary else "")
    with io.open(filename, mode, encoding=encoding,
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
#@requires: mod:json safe_open
def save_json_file(filename, contents, encoding=None,
                   errors=None, newline=None, safe=True, json_args={}):
    json_args = dict(json_args)
    with safe_open(filename, "wt", encoding=encoding,
                   errors=errors, newline=newline, safe=safe) as stream:
        json.dump(contents, stream, **json_args)
        if json_args.get("indent", None) is not None:
            stream.write("\n")
#@]

#@snip/load_json_file[
#@requires: mod:io mod:json
def load_json_file(filename, encoding=None, errors=None, newline=None):
    with io.open(filename, "rt", encoding=encoding,
                 errors=errors, newline=newline) as stream:
        return json.load(stream)
#@]

#@snip/WorkDir[
#@requires: mod:os
class WorkDir(object):
    '''A context manager for changing to a different working directory.  The
    original working directory is restored upon exiting the context.'''

    def __init__(self, path):
        '''Initialize a context for changing the working directory.'''
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
#@]

#@snip/FileLock[
#@requires: mod:fcntl
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
#@requires: mod:itertools
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
    (use `unichr` on Python 2)'''
    return ((l - 0xd800) << 10) + r - 0xdc00 + 0x010000
#@]

#@snip/snormpath[
#@requires: mod:re
def snormpath(path):
    sep = "/"
    m = re.match(re.escape(sep) + "*", path)
    num_leading_slashes = len(m.group(0))
    if num_leading_slashes > 2:
        num_leading_slashes = 1
    return (sep * num_leading_slashes +
            sep.join(s for s in path.split(sep)
                     if not re.match(r"\.?$", s))) or "."
#@]
#@]

#@snip/null_context_manager[
#@provides: null_context_manager
class NullContextManager(object):

    def __enter__(self):
        pass

    def __exit__(self, exc_type, exc_val, exc_tb):
        pass

null_context_manager = NullContextManager()
#@]

#@snip/subprocess_shim[
#@provides: CompletedProcess Popen check_output run
#@requires: mod:os mod:subprocess null_context_manager
DEVNULL = -3

def run(*args, input=None, check=False, **kwargs):
    '''Mimics the API of 'run' in Python 3.5 but does not support 'timeout'.'''
    if input is not None:
        if "stdin" in kwargs:
            raise ValueError("stdin and input arguments may not both be used.")
        kwargs["stdin"] = subprocess.PIPE
    proc = Popen(*args, **kwargs)
    try:
        out, err = proc.communicate(input)
    except:
        proc.kill()
        proc.wait()
        raise
    result = CompletedProcess(proc.args, proc.returncode,
                              stdout=out, stderr=err)
    if check:
        result.check_returncode()
    return result

def check_output(*args, **kwargs):
    return run(*args, check=True, stdout=subprocess.PIPE, **kwargs).stdout

def Popen(args, stdin=None, stdout=None, stderr=None, **kwargs):
    '''A variant of Popen that accepts 'DEVNULL' for standard streams.'''
    devnull = None
    open_devnull = lambda: devnull or open(os.devnull, "r+b")
    if stdin == DEVNULL:
        devnull = open_devnull()
        stdin = devnull
    if stdout == DEVNULL:
        devnull = open_devnull()
        stdout = devnull
    if stderr == DEVNULL:
        devnull = open_devnull()
        stderr = devnull
    with devnull or NullContextManager():
        return subprocess.Popen(args, stdin=stdin, stdout=stdout,
                                stderr=stderr, **kwargs)

class CompletedProcess(object):

    def __init__(self, args, returncode, stdout=None, stderr=None):
        self.args = args
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr

    def check_returncode(self):
        CalledProcessError = subprocess.CalledProcessError
        if not self.returncode:
            return
        # older versions of Python did not support output and/or stderr arguments
        try:
            raise CalledProcessError(
                self.returncode,
                self.args,
                output=self.stdout,
                stderr=self.stderr,
            )
        except TypeError:
            pass
        try:
            raise CalledProcessError(
                self.returncode,
                self.args,
                output=self.stdout,
            )
        except TypeError:
            pass
        raise CalledProcessError(
            self.returncode,
            self.args,
        )

    def __repr__(self):
        s = "CompletedProcess(args=" + repr(self.args)
        s += ", returncode=" + repr(self.returncode)
        if self.stdout is not None:
            s += ", stdout=" + repr(self.stdout)
        if self.stderr is not None:
            s += ", stderr=" + repr(self.stderr)
        s += ")"
        return s
#@]
