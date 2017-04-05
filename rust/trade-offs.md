# Trade-offs in Rust

## `&'a [u8]`: Shared slice reference

  - `░░░` Cannot modify individual bytes nor length
  - `███` Can share at negligible cost (`Copy`)
  - `░░░` Restricted to lifetime `'a`
  - `███` Thread-safe (`Send` and `Sync`)
  - `███` No synchronization

## `&'a mut [u8]`: Exclusive slice reference

  - `█░░` Can modify individual bytes but not length
  - `░░░` Cannot share (not `Copy`)
  - `░░░` Restricted to lifetime `'a`
  - `███` Thread-safe (`Send` and `Sync`)
  - `███` No synchronization

## `Vec<u8>`: Owned vector

  - `██░` Can modify individual bytes and length
  - `░░░` Cannot share except through references (not `Copy`, and `Clone` is expensive)
  - `███` No lifetime restrictions
  - `███` Thread-safe (`Send` and `Sync`)
  - `███` No synchronization

## `RefCell<Vec<u8>>`: Owned vector with thread-unsafe interior mutability

  - `███` Can modify individual bytes and length, even through shared references
  - `░░░` Cannot share except through references (not `Copy`, and `Clone` is expensive)
  - `███` No lifetime restrictions
  - `█░░` References are not thread-safe (`Send` but not `Sync`)
  - `█░░` Requires dynamic borrowing to read or modify (low cost)

## `Mutex<Vec<u8>>`: Owned vector with thread-safe interior mutability

  - `███` Can modify individual bytes and length, even through shared references
  - `░░░` Cannot share except through references (not `Copy`, and `Clone` is expensive)
  - `███` No lifetime restrictions
  - `███` Thread-safe (`Send` and `Sync`)
  - `░░░` Requires synchronization through mutex to read or modify (moderate cost)

## `Rc<Vec<u8>>`: Thread-unsafe shared owned vector

  - `░░░` Cannot modify individual bytes nor length
  - `██░` Can share at low cost (~3ns) (`Clone`)
  - `███` No lifetime restrictions
  - `░░░` Not thread-safe (neither `Send` nor `Sync`)
  - `███` No synchronization

## `Arc<Vec<u8>>`: Thread-safe shared owned vector

  - `░░░` Cannot modify individual bytes nor length
  - `█░░` Can share at moderate cost (~10ns) (`Clone`)
  - `███` No lifetime restrictions
  - `███` Thread-safe (`Send` and `Sync`)
  - `███` No synchronization

## `Rc<RefCell<Vec<u8>>>`: Thread-unsafe shared owned vector with interior mutability

  - `███` Can modify individual bytes and length, even through shared references
  - `██░` Can share at low cost (~3ns) (`Clone`)
  - `███` No lifetime restrictions
  - `░░░` Not thread-safe (neither `Send` nor `Sync`)
  - `█░░` Requires dynamic borrowing to read or modify (low cost)

## `Arc<Mutex<Vec<u8>>>`: Thread-safe shared owned vector with interior mutability

  - `███` Can modify individual bytes and length, even through shared references
  - `█░░` Can share at moderate cost (~10ns) (`Clone`)
  - `███` No lifetime restrictions
  - `███` Thread-safe (`Send` and `Sync`)
  - `░░░` Requires synchronization through mutex to read or modify (moderate cost)
