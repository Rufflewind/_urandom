pub mod quote {
    pub fn shell(s: &str) -> String {
        "'".to_owned() + &s.replace("'", "'\\''") + "'"
    }

    pub fn yaml(s: &str) -> String {
        "'".to_owned() + &s.replace("'", "'''") + "'"
    }
}

pub fn strip_prefix<'a>(prefix: &str, target: &'a str) -> Option<&'a str> {
    if target.starts_with(prefix) {
        Some(&target[prefix.len() ..])
    } else {
        None
    }
}
