from typing import Type, TypeVar

T = TypeVar('T')

def to(ty: Type[T], value: object) -> T:
    if not isinstance(value, ty):
        raise TypeError(f'expected {ty.__qualname__}, got {value!r}: '
                        f'{type(value).__qualname__}')
    return value
