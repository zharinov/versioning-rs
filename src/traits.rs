pub trait Version: PartialEq + PartialOrd {
    // For the most version schemes it's always `true`.
    //
    // However, if there are different "equivalence classes"
    // of versions, then `is_compatible` must be implemented.
    //
    // Example: in Docker ecosystem, we want to distinguish
    // `node:20.12.2` from `node:20.12.2-alpine` images.
    // These are same versions, but are different "lines".
    fn is_compatible(&self, _other: &Self) -> bool {
        true
    }
}
