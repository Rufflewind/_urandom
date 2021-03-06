#@imports[
import ctypes
import errno
import fcntl
import heapq
import io
import itertools
import json
import locale
import os
import re
import shutil
import signal
import subprocess
import tempfile
import threading
#@]

#@snips[
#@DEVNULL[
DEVNULL = -3
#@]

#@JSON_CANONICAL[
JSON_CANONICAL = {
    "ensure_ascii": False,
    "separators": (",", ":"),
    "sort_keys": True,
}
#@]

#@JSON_PRETTY[
JSON_PRETTY = {
    "ensure_ascii": False,
    "indent": 4,
    "separators": (",", ": "),
    "sort_keys": True,
}
#@]

#@pretty_json[
#@requires: JSON_PRETTY
def pretty_json(j):
    return json.dumps(j, **JSON_PRETTY)
#@]

#@Identity[
class Identity(object):
    '''Allows hashing by identity if hashing is not otherwise available.'''
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return "Identity({!r})".format(self.value)

    def __hash__(self):
        __hash__ = getattr(self.value, "__hash__", None)
        if __hash__ is not None:
            return __hash__()
        return hash(id(self.value))

    def __eq__(self, other):
        __hash__ = getattr(self.value, "__hash__", None)
        if __hash__ is not None:
            return self.value == other
        return id(self.value) == id(other.value)
#@]

#@combine_surrogate_pair[
def combine_surrogate_pair(l, r):
    '''Example: chr(combine_surrogate_pair(55357, 56832))
    (use `unichr` on Python 2)'''
    return ((l - 0xd800) << 10) + r - 0xdc00 + 0x010000
#@]

#@do_nothing[
def do_nothing(*args, **kwargs):
    pass
#@]

#@hash_file[
def hash_file(hasher, file, block_size=(1 << 20)):
    if isinstance(file, str):
        with open(file, "rb") as f:
            return hash_file(hasher, f, block_size=block_size)
    h = hasher()
    for block in iter(lambda: file.read(block_size), b""):
        h.update(block)
    return h
#@]

#@merge_sets[
def merge_sets(*sets):
    s0 = set()
    for s in sets:
        s0.update(s)
    return s0
#@]

#@new_OrdWrapper[
def new_OrdWrapper(key=lambda x: x, reverse=False):
    '''Create a wrapper class that allows a new total ordering to be defined
    on objects of another type.

    'key' is expected to be a key function: it should return a value of some
    totally ordered type that defines the desired ordering.  'reverse'
    determines if the ordering should be reversed.

    The wrapper class defines:

    - a simple constructor that accepts an arbitrary value that can be later
      accessed using '.value', and
    - all 6 of the rich comparison methods.

    '''
    def init_func(self, value):
        self.value = value
        self.key = key(value)
    def repr_func(self):
        return "OrdWrapper({!r})".format(self.value)
    if reverse:
        class OrdWrapper(object):
            __init__ = init_func
            __repr__ = repr_func
            def __lt__(self, other):
                return self.key > other.key
            def __le__(self, other):
                return self.key >= other.key
            def __eq__(self, other):
                return self.key == other.key
            def __ne__(self, other):
                return self.key != other.key
            def __gt__(self, other):
                return self.key < other.key
            def __ge__(self, other):
                return self.key <= other.key
    else:
        class OrdWrapper(object):
            __init__ = init_func
            __repr__ = repr_func
            def __lt__(self, other):
                return self.key < other.key
            def __le__(self, other):
                return self.key <= other.key
            def __eq__(self, other):
                return self.key == other.key
            def __ne__(self, other):
                return self.key != other.key
            def __gt__(self, other):
                return self.key > other.key
            def __ge__(self, other):
                return self.key >= other.key
    return OrdWrapper
#@]

#@read_lines[
def read_lines(stream):
    '''Workaround for Python 2, which blocks until EOF when iterating over a
    file object.'''
    nl = None
    while True:
        l = stream.readline()
        if l:
            yield l
        if not nl:
            nl = b"\n" if isinstance(l, bytes) else "\n"
        if not l.endswith(nl):
            break
#@]

#@update_dict[
def update_dict(d0, *dicts, **kwargs):
    merger = kwargs.pop("merger", None)
    for k in kwargs:
        raise TypeError("got an unexpected keyword argument {0}".format(k))
    for d in dicts:
        if merger:
            for k, v in d.items():
                exists = False
                try:
                    v0 = d0[k]
                    exists = True
                except KeyError:
                    pass
                if exists:
                    d0[k] = merger((k, v0), (k, v))[1]
                else:
                    d0[k] = v
        else:
            d0.update(d)
#@]

#@merge_dicts[
#@requires: update_dict
def merge_dicts(*dicts, **kwargs):
    '''Merge multiple dictionaries.  OrderedDict is also supported.'''
    merger = kwargs.pop("merger", None)
    for k in kwargs:
        raise TypeError("got an unexpected keyword argument {0}".format(k))
    dicts = iter(dicts)
    try:
        d0 = next(dicts)
    except StopIteration:
        return {}
    d0 = type(d0)(d0)
    update_dict(d0, *dicts, merger=merger)
    return d0
#@]

#@exclusive_merge[
def exclusive_merge(arg0, *args):
    '''Return one of the arguments if all of them are equal.  Fails with
    `ValueError` otherwise.'''
    for arg in args:
        if arg0 != arg:
            raise ValueError("conflicting values: {0!r} vs {1!r}"
                             .format(arg0, arg))
    return arg0
#@]

#@FileLock[
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

#@LookaheadIterator[
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

#@WorkDir[
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

#@PREFERREDENCODING[
#@requires: mod:locale
PREFERREDENCODING = locale.getpreferredencoding(True)
#@]

#@ensure_str[
#@requires: PREFERREDENCODING
# The use of this function is questionable.
# It might be deprecated in the future.
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

#@load_file[
#@requires: mod:io
def load_file(filename, binary=False, encoding=None,
              errors=None, newline=None):
    '''Read the contents of a file.'''
    if not binary and encoding is None:
        raise ValueError("if not binary, encoding must be given")
    mode = "r" + ("b" if binary else "")
    with io.open(filename, mode, encoding=encoding,
                 errors=errors, newline=newline) as stream:
        return stream.read()
#@]

#@try_remove[
#@requires: mod:os
def try_remove(path):
    try:
        os.remove(path)
    except OSError:
        return False
    return True
#@]

#@makedirs[
#@requires: mod:os
def makedirs(name, mode=0o777, exist_ok=False):
    try:
        os.makedirs(name, mode=mode)
    except OSError:
        if not (exist_ok and path.isdir(name)):
            raise
#@]

#@wrapped_open[
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

#@ctypes.wintypes[
if os.name == "nt":
    import ctypes.wintypes
#@]

#@rename[
#@requires: mod:os mod:ctypes ctypes.wintypes
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

#@TemporarySaveFile[
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

#@safe_open[
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

#@save_file[
#@requires: safe_open
def save_file(filename, contents, binary=False, encoding=None,
              errors=None, newline=None, safe=True):
    '''Write the contents to a file.  If `safe` is true, it is performed by
    first writing into a temporary file and then replacing the original file
    with the temporary file.  This ensures that the file will not end up in a
    half-written state.  Note that there is a small possibility that the
    temporary file might remain if the program crashes while writing.'''
    if not binary and encoding is None:
        raise ValueError("if not binary, encoding must be given")
    mode = "w" + ("b" if binary else "")
    with safe_open(filename, mode, encoding=encoding,
                   errors=errors, newline=newline, safe=safe) as stream:
        stream.write(contents)
        stream.flush()
#@]

#@snormpath[
#@requires: mod:re
def snormpath(path):
    '''Like 'normpath', but does not expand '..' to avoid altering the
    meaning of a path (e.g. due to '..' inside a symlinked directory).'''
    sep = "/"
    m = re.match(re.escape(sep) + "*", path)
    num_leading_slashes = len(m.group(0))
    if num_leading_slashes > 2:
        num_leading_slashes = 1
    return (sep * num_leading_slashes +
            sep.join(s for s in path.split(sep)
                     if not re.match(r"\.?$", s))) or "."
#@]

#@toposort[
#@requires: mod:heapq new_OrdWrapper
def toposort(graph, key=lambda x: x, reverse=False, flip=False):
    '''Topologically sort a directed acyclic graph deterministically (see
    caveats below), ensuring that the arrows always point right, unless 'flip'
    is 'True', in which case the arrows always point left.

        <graph> = {<vertex>: [<vertex>, ...], ...}

    The `graph` is represented as a dictionary, where each dictionary key
    uniquely identifies the vertex and its associated value contains a list of
    direct successors for that vertex.  For example:

        graph = {0: [1, 2], 1: [2], 2: [], 3: []}

    This represents a graph with four vertices and three edges:

        0 -> 1
        0 -> 2
        1 -> 2

    Vertex '3' does not have any edges.

    The ordering is determined by the 'key' function, which receives a vertex
    key and produces some totally ordered value.  The ordering can be reversed
    by setting 'reverse' to 'True'.

    The sorted result is always lexicographically minimized, which means it is
    deterministic if and only if the output of 'key' applied to the vertices
    results in distinct values.

    If you originally had an arbitrarily ordered sequence of items and want to
    preserve the original ordering of the elements as much as possible,
    consider using a key function that maps each element to its original
    position.
    '''

    # note: the ordering of the edges is immaterial
    if flip:
        # we must explicitly perform the flip: the lexicographical ordering
        # requirement prevents us from using tricks to avoid this, which would
        # get screwed up by reversing the output of this algorithm
        new_graph = dict((v, set()) for v in graph)
        for v1, vs in graph.items():
            for v2 in vs:
                new_graph[v2].add(v1)
        graph = new_graph
    else:
        graph = dict((v, frozenset(vs)) for v, vs in graph.items())

    # count the number of dependents and extract the roots
    indegree = {}
    for v, deps in graph.items():
        for dep in deps:
            indegree[dep] = indegree.get(dep, 0) + 1
    roots = []
    for v in graph:
        if v not in indegree:
            indegree[v] = 0
            roots.append(v)

    OrdWrapper = new_OrdWrapper(key=key, reverse=reverse)
    roots = [OrdWrapper(v) for v in roots]

    # keep roots sorted to ensure a deterministic result
    heapq.heapify(roots)

    # Kahn's algorithm
    # (note: this will alter indegree and roots)
    result = []
    while roots:
        v1 = heapq.heappop(roots).value
        result.append(v1)
        for v2 in graph[v1]:
            indegree[v2] -= 1
            if not indegree[v2]:
                heapq.heappush(roots, OrdWrapper(v2))
    if len(result) != len(graph):
        raise ValueError("graph is cyclic")

    return result
#@]

#@exception_to_signal[
#@requires: mod:signal
def exception_to_signal(exception):
    '''Obtains the signal associated with the given exception, if any.  If the
    exception is `KeyboardInterrupt`, then it returns `signal.SIGINT`.
    Otherwise, it returns the value of the `signal` attribute of the
    exception, if any.  If the attribute is not found, `None` is returned,
    indicating that there is no signal associated with the exception.'''
    if isinstance(exception, KeyboardInterrupt):
        return signal.SIGINT
    return getattr(exception, "signal", None)
#@]

#@ChildProcess[
#@requires: mod:signal exception_to_signal
class ChildProcess(object):

    def __init__(self, proc, default_signal=signal.SIGTERM):
        '''The `default_signal` specifies the signal that is sent to the child
        process if the context exits due to something else other than an
        exception with an associated signal (see: `exception_to_signal`).  In
        this case, if the `default_signal` is `None`, the process is killed.
        Otherwise, it will receive the given signal.'''
        self._proc = proc
        self._default_signal = default_signal

    def __enter__(self):
        return self._proc

    def __exit__(self, exc_type, exc_val, exc_tb):
        sig = exception_to_signal(exc_val)
        if self._proc.stdout:
            self._proc.stdout.close()
        if self._proc.stderr:
            self._proc.stderr.close()
        try:
            if self._proc.stdin:
                self._proc.stdin.close()
        finally:
            if sig is None:
                sig = self._default_signal
            if sig is None:
                self._proc.kill()
            else:
                self._proc.send_signal(sig)
            # must wait to avoid zombies
            self._proc.wait()
#@]

#@SIGNAL_NAME[
#@requires: mod:re mod:signal
SIGNAL_NAME = dict(
    (sig, name)
    for name, sig in signal.__dict__.items()
    if re.match("SIG[A-Z]+$", name)
)
#@]

#@signal_name[
#@requires: SIGNAL_NAME
def signal_name(sig):
    try:
        return "{0} ({1})".format(SIGNAL_NAME[sig], sig)
    except KeyError:
        return "signal {0}".format(sig)
#@]

#@CompletedProcess[
#@requires: mod:subprocess
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
        # older versions of Python didn't support `output` and/or `stderr`
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
        s = [
            "CompletedProcess(args=",
            repr(self.args),
            ", returncode=",
            repr(self.returncode),
        ]
        if self.stdout is not None:
            s.extend([", stdout=", repr(self.stdout)])
        if self.stderr is not None:
            s.extend([", stderr=", repr(self.stderr)])
        s.append(")")
        return "".join(s)
#@]

#@NullContextManager[
class NullContextManager(object):

    def __enter__(self):
        pass

    def __exit__(self, exc_type, exc_val, exc_tb):
        pass
#@]

#@null_context_manager[
#@requires: NullContextManager
null_context_manager = NullContextManager()
#@]

#@Popen[
#@requires: mod:os mod:subprocess null_context_manager
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
    with devnull or null_context_manager:
        return subprocess.Popen(args, stdin=stdin, stdout=stdout,
                                stderr=stderr, **kwargs)
#@]

#@run[
#@requires: mod:subprocess CompletedProcess Popen
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
#@]

#@check_output[
#@requires: mod:subprocess run
def check_output(*args, **kwargs):
    return run(*args, check=True, stdout=subprocess.PIPE, **kwargs).stdout
#@]

#@load_json_file[
#@requires: mod:io mod:json
def load_json_file(filename, encoding=None, errors=None, newline=None):
    with io.open(filename, "rt", encoding=encoding,
                 errors=errors, newline=newline) as stream:
        return json.load(stream)
#@]

#@LazilyMappedDict[
class LazilyMappedDict(object):
    '''Maps over the values of a dictionary.  The mapping is only performed
    on-demand and its results are cached for future uses.  Note that if the
    source dictionary changes, the behavior of this wrapper class is
    unspecified.'''

    def __init__(self, func, dict):
        self._dict = dict
        self._func = func
        self._cache = {}

    def __getitem__(self, key):
        try:
            return self._cache[key]
        except KeyError:
            pass
        value = self._func(self._dict[key])
        self._cache[key] = value
        return value
#@]

#@shell_quote[
#@requires: mod:re
def shell_quote(string):
    # we try to be conservative here because some shells have more special
    # characters than others (`!` and `^` are not safe); we require empty
    # strings to be quoted
    if re.match("[]0-9A-Za-z/@:.,_+-]+$", string):
        return string
    return "'{0}'".format(string.replace("'", "'\\''"))
#@]

#@shell_quote_arg[
#@requires: mod:re shell_quote
def shell_quote_arg(string):
    # allow `=` since it's safe as an argument
    if re.match("[]0-9A-Za-z/@:.,_=+-]+$", string):
        return string
    return shell_quote(string)
#@]

#@map_shell_quote[
#@requires: LazilyMappedDict shell_quote
def map_shell_quote(dict):
    '''Useful for mapping over the values of `locals()`, as often used in
    `str.format`.'''
    return LazilyMappedDict(shell_quote, dict)
#@]

#@save_json_file[
#@requires: mod:json safe_open
def save_json_file(filename, contents, encoding=None,
                   errors=None, newline=None, safe=True, json_kwargs={}):
    json_kwargs = dict(json_kwargs)
    with safe_open(filename, "wt", encoding=encoding,
                   errors=errors, newline=newline, safe=safe) as stream:
        json.dump(contents, stream, **json_kwargs)
        if json_kwargs.get("indent", None) is not None:
            stream.write("\n")
        stream.flush()
#@]

#@Signal[
#@requires: signal_name
class Signal(BaseException):
    def __init__(self, signal, *args):
        self.signal = signal
        super(Signal, self).__init__(signal, *args)
    def __str__(self):
        return signal_name(self.signal)
#@]

#@is_main_thread[
#@requires: mod:threading
def is_main_thread():
    '''Return whether the current thread is the main thread.'''
    get_main_thread = getattr(threading, "main_thread", None)
    if not get_main_thread:             # for Python 2 compatibility
        return isinstance(threading.current_thread(), threading._MainThread)
    return threading.current_thread() == get_main_thread()
#@]

#@SignalsToExceptions[
#@requires: mod:os mod:signal Signal is_main_thread
class SignalsToExceptions(object):

    def __init__(self, signals=["SIGHUP", "SIGINT", "SIGTERM"]):
        '''The `signals` argument can be an iterable of either strings or
        signal values (or a mixture of them).  When specified as a string,
        a signal that isn't supported is ignored.'''
        self._signals = signals

    def __enter__(self):
        if not is_main_thread():
            return
        self._prev_handlers = {}
        for sig in self._signals:
            try:
                sig = getattr(signal, sig)
            except AttributeError:      # signal not supported
                continue
            except TypeError:           # not a string; try using it directly
                pass
            prev_handler = signal.signal(sig, self._handle)
            self._prev_handlers[sig] = prev_handler

    def __exit__(self, exc_type, exc_val, exc_tb):
        if not is_main_thread():
            return
        for sig, handler in self._prev_handlers.items():
            signal.signal(sig, handler)
        sig = getattr(exc_val, "signal", None)
        if sig is not None:
            os.kill(os.getpid(), sig)
            return True

    def _handle(self, sig, frame):
        raise Signal(sig)
#@]
#@]

#@tests[
#@LookaheadIterator[
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
#@]
