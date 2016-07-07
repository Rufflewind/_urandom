import os
from util import DEVNULL, PREFERREDENCODING, check_output, run

DEFAULT_CACHE_DIR = os.path.expanduser("~/.cache/snep/git")
HASH_ALG = hashlib.sha512
HASH_LEN = 32

def hash_str(string):
    '''Compute a hash, base64-encoded and valid as a filename.'''
    import base64, hashlib
    return base64.b64encode(HASH_ALG(string.encode("utf8")).digest(),
                            b"-_").decode("ascii").strip("=")[:HASH_LEN]

def get_repo_path(cache_dir, remote):
    return os.path.join(cache_dir, hash_str(remote))

def init_repo(cache_dir, remote, fetch=True):
    repo = get_repo_path(cache_dir, hash_str(remote))
    if not is_git_dir(repo):
        run(["git", "clone", "--bare", "-q", "--", remote, repo],
            stdin=DEVNULL, check=True)
    elif fetch:
        run(["git", "-C", repo, "fetch", "--all", "-p", "-q"],
            stdin=DEVNULL, check=True)
    return repo

def validate_rev(rev):
    if rev.startswith("-"):
        raise ValueError("not a valid revision: " + rev)

def is_git_dir(repo):
    return not run(["git", "-C", repo, "rev-parse", "--git-dir"],
                   stdin=DEVNULL, stdout=DEVNULL, stderr=DEVNULL).returncode

def get_rev(repo, rev):
    validate_rev(rev)
    return check_output(["git", "-C", repo, "rev-parse", rev],
                        stdin=DEVNULL).decode(PREFERREDENCODING).strip()

def get_file_contents(repo, rev, path):
    validate_rev(rev)
    return check_output(["git", "-C", repo, "show", rev + ":" + path],
                        stdin=DEVNULL)

class GitCache(object):

    def __init__(self, cache_dir, fetch=True):
        self._cache_dir = cache_dir
        self._fetch = fetch
        self.get_repo = Cache(self.init_repo).get

    @property
    def cache_dir(self):
        return self._cache_dir

    def init_repo(self, remote):
        return init_repo(self.cache_dir, remote, fetch=self._fetch)

    def get_file(self, remote, rev, path):
        repo = self.get_repo(remote)
        return get_file_contents(repo, rev, path)

class Cache(object):

    def __init__(self, acquire):
        self._cached = {}
        self._acquire = acquire

    def get(self, key):
        try:
            return self._cached[key]
        except KeyError:
            pass
        value = self._acquire(key)
        self._cached[key] = value
        return value
