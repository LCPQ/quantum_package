from functools import wraps

def cache(func):
    """
    A decorator for lazy evaluation off true function
    """
    saved = {}

    @wraps(func)
    def newfunc(*args):
        if args in saved:
            return saved[args]

        result = func(*args)
        saved[args] = result
        return result
    return newfunc


class classproperty(object):

    def __init__(self, fget):
        self.fget = fget

    def __get__(self, owner_self, owner_cls):
        return self.fget(owner_cls)
