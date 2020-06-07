from typing import Type, TypeVar

T = TypeVar('T')

def to(ty: Type[T], value: object) -> T:
    if not isinstance(value, ty):
        raise TypeError(f'expected {ty!r}, got {value!r}')
    return value
