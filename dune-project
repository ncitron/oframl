(lang dune 3.12)

(name oframl)

(generate_opam_files true)

(source
 (github ncitron/oframl))

(authors "Noah Citron")

(maintainers "Noah Citron")

(license MIT)

(homepage https://github.com/ncitron/oframl)
(documentation https://github.com/ncitron/oframl)
(bug_reports https://github.com/ncitron/oframl/issues)

(package
 (name oframl)
 (synopsis "Farcaster Frames in OCaml")
 (description "A framework for building Farcaster Frames in OCaml")
 (depends ocaml dune cohttp lwt cohttp-lwt-unix yojson ppx_deriving_yojson ppx_string dotenv)
 (tags
  (topics "Farcaster" "Frames")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
