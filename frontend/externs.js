// This 'Externs' file contains the names of external js libraries, so that
// the closure compiler does not rename these symbols, and
// furthermore does not create new symbols with the names below.
//
// For more info see https://developers.google.com/closure/compiler/docs/externs-and-exports

// The ace editor defines a global ace variable.
// This line prevents the closure compiler from renaming someting to 'ace'.
const ace = { };
