(lang dune 3.12)

(name oframl)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name oframl)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune cohttp-lwt-unix yojson ppx_deriving_yojson)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
