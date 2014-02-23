$(fasldir)/vicare/formations.fasl: $(fasldir)/vicare/language-extensions/infix.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/getopts.fasl: $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/language-extensions/syntaxes.fasl
$(fasldir)/vicare/irregex.fasl: $(fasldir)/vicare/irregex/compat.fasl
$(fasldir)/vicare/parser-logic.fasl: $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/cre2.fasl: $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/readline.fasl: $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/iconv.fasl: $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/platform/constants.fasl $(fasldir)/vicare/unsafe/capi.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/linux.fasl: $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/platform/constants.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/arguments/general-c-buffers.fasl $(fasldir)/vicare/platform/words.fasl $(fasldir)/vicare/posix.fasl $(fasldir)/vicare/unsafe/capi.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/language-extensions/cond-expand.fasl $(fasldir)/vicare/platform/features.fasl $(fasldir)/vicare/language-extensions/cond-expand/helpers.fasl
$(fasldir)/vicare/gcc.fasl: $(fasldir)/vicare/ffi.fasl $(fasldir)/vicare/posix.fasl $(fasldir)/vicare/glibc.fasl $(fasldir)/vicare/platform/constants.fasl $(fasldir)/vicare/language-extensions/syntaxes.fasl
$(fasldir)/vicare/glibc.fasl: $(fasldir)/vicare/posix.fasl $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/platform/constants.fasl $(fasldir)/vicare/unsafe/capi.fasl $(fasldir)/vicare/platform/words.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/language-extensions/cond-expand.fasl $(fasldir)/vicare/platform/features.fasl $(fasldir)/vicare/language-extensions/cond-expand/helpers.fasl
$(fasldir)/srfi/%3a106.fasl: $(fasldir)/srfi/%3a106/socket.fasl
$(fasldir)/srfi/%3a106/socket.fasl: $(fasldir)/srfi/%3a106/compat.fasl
$(fasldir)/srfi/%3a106/compat.fasl: $(fasldir)/vicare/platform/constants.fasl $(fasldir)/vicare/posix.fasl
$(fasldir)/vicare/posix/tcp-server-sockets.fasl: $(fasldir)/vicare/posix.fasl $(fasldir)/vicare/platform/constants.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/posix/simple-event-loop.fasl: $(fasldir)/vicare/posix.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/platform/constants.fasl $(fasldir)/vicare/platform/utilities.fasl
$(fasldir)/vicare/posix/daemonisations.fasl: $(fasldir)/vicare/posix.fasl $(fasldir)/vicare/platform/constants.fasl
$(fasldir)/vicare/posix/log-files.fasl: $(fasldir)/vicare/posix.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/posix/lock-pid-files.fasl: $(fasldir)/vicare/posix.fasl $(fasldir)/vicare/platform/constants.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/posix/pid-files.fasl: $(fasldir)/vicare/posix.fasl
$(fasldir)/vicare/posix.fasl: $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/platform/constants.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/arguments/general-c-buffers.fasl $(fasldir)/vicare/unsafe/capi.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/platform/words.fasl $(fasldir)/vicare/language-extensions/cond-expand.fasl $(fasldir)/vicare/containers/weak-hashtables.fasl $(fasldir)/vicare/platform/features.fasl $(fasldir)/vicare/language-extensions/cond-expand/helpers.fasl
$(fasldir)/vicare/ffi/foreign-pointer-wrapper.fasl: $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/ffi.fasl: $(fasldir)/vicare/platform/errno.fasl
$(fasldir)/nausicaa/mehve.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/mehve/language/numerics/predicates.fasl $(fasldir)/nausicaa/mehve/language/numerics/arithmetics.fasl $(fasldir)/nausicaa/mehve/language/numerics/parts.fasl $(fasldir)/nausicaa/mehve/language/numerics/transcendental.fasl $(fasldir)/nausicaa/mehve/language/infix.fasl $(fasldir)/nausicaa/mehve/language/input-output.fasl
$(fasldir)/nausicaa/mehve/language/input-output.fasl: $(fasldir)/nausicaa.fasl
$(fasldir)/nausicaa/mehve/language/infix.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/language-extensions/infix/parser-utils.fasl $(fasldir)/vicare/language-extensions/infix/tokens.fasl $(fasldir)/nausicaa/mehve/language/numerics/predicates.fasl $(fasldir)/nausicaa/mehve/language/numerics/arithmetics.fasl $(fasldir)/nausicaa/mehve/language/numerics/parts.fasl $(fasldir)/nausicaa/mehve/language/numerics/transcendental.fasl
$(fasldir)/nausicaa/mehve/language/numerics/transcendental.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/mehve/language/numerics/parts.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/mehve/language/numerics/arithmetics.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/mehve/language/numerics/predicates.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/uri/pathnames.fasl: $(fasldir)/nausicaa/uri/pathnames/unix.fasl
$(fasldir)/nausicaa/uri/pathnames/unix.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/nausicaa/uri/pathnames/abstract.fasl $(fasldir)/vicare/parser-tools/unix-pathnames.fasl
$(fasldir)/nausicaa/uri/pathnames/abstract.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/parser-tools/uri/utilities.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/uri.fasl $(fasldir)/nausicaa/uri.fasl
$(fasldir)/nausicaa/uri.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/language-extensions/makers.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/language-extensions/ascii-chars.fasl $(fasldir)/nausicaa/uri/ip.fasl
$(fasldir)/nausicaa/uri/ip.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/language-extensions/keywords.fasl $(fasldir)/vicare/language-extensions/ascii-chars.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/parser-tools/uri.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/ipv4-addresses.fasl $(fasldir)/nausicaa/parser-tools/ipv6-addresses.fasl $(fasldir)/vicare/language-extensions/makers.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/language-extensions/ascii-chars.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/nausicaa/parser-tools/ipv6-addresses.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.fasl $(fasldir)/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.fasl $(fasldir)/vicare/language-extensions/makers.fasl $(fasldir)/vicare/parser-tools/silex/lexer.fasl $(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/parser-tools/ipv4-addresses.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.fasl $(fasldir)/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.fasl $(fasldir)/vicare/language-extensions/makers.fasl $(fasldir)/vicare/parser-tools/silex/lexer.fasl $(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/lalr/lr-driver.fasl $(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl
$(fasldir)/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/nausicaa/parser-tools/silex/default-error-handler.fasl $(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl
$(fasldir)/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/lalr/lr-driver.fasl $(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl
$(fasldir)/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/nausicaa/parser-tools/silex/default-error-handler.fasl $(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl
$(fasldir)/nausicaa/parser-tools/lalr.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl $(fasldir)/vicare/language-extensions/makers.fasl
$(fasldir)/nausicaa/parser-tools/lalr/glr-driver.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl
$(fasldir)/nausicaa/parser-tools/lalr/lr-driver.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl
$(fasldir)/nausicaa/parser-tools/silex/default-error-handler.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl $(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl
$(fasldir)/nausicaa/parser-tools/lexical-tokens.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/nausicaa/parser-tools/source-locations.fasl $(fasldir)/vicare/language-extensions/makers.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/nausicaa/parser-tools/source-locations.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/language-extensions/makers.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/nausicaa/containers/iterators.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/language-extensions/makers.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/containers/bitvectors.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/platform/words.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/containers/queues.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/containers/queues.fasl
$(fasldir)/nausicaa/containers/stacks.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/containers/stacks.fasl
$(fasldir)/nausicaa/containers/arrays.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/containers/strings.fasl $(fasldir)/vicare/containers/lists.fasl $(fasldir)/vicare/containers/vectors.fasl $(fasldir)/nausicaa/containers/lists.fasl $(fasldir)/nausicaa/containers/vectors.fasl $(fasldir)/nausicaa/containers/strings.fasl
$(fasldir)/nausicaa/containers/strings.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/containers/strings.fasl $(fasldir)/vicare/containers/strings/low.fasl
$(fasldir)/nausicaa/containers/vectors.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/containers/vectors.fasl
$(fasldir)/nausicaa/containers/lists.fasl: $(fasldir)/nausicaa.fasl $(fasldir)/vicare/containers/lists.fasl
$(fasldir)/nausicaa.fasl: $(fasldir)/nausicaa/language/oopp.fasl $(fasldir)/nausicaa/language/multimethods.fasl $(fasldir)/nausicaa/language/builtins.fasl $(fasldir)/nausicaa/language/conditions.fasl $(fasldir)/nausicaa/language/increments.fasl $(fasldir)/nausicaa/language/simple-match.fasl $(fasldir)/nausicaa/language/infix.fasl $(fasldir)/vicare/language-extensions/namespaces.fasl $(fasldir)/vicare/language-extensions/sentinels.fasl
$(fasldir)/nausicaa/language/infix.fasl: $(fasldir)/vicare/language-extensions/infix.fasl $(fasldir)/vicare/language-extensions/infix/parser-utils.fasl $(fasldir)/vicare/language-extensions/infix/tokens.fasl $(fasldir)/nausicaa/language/increments.fasl $(fasldir)/nausicaa/language/oopp.fasl
$(fasldir)/nausicaa/language/increments.fasl: $(fasldir)/nausicaa/language/oopp.fasl
$(fasldir)/nausicaa/language/simple-match.fasl: $(fasldir)/nausicaa/language/oopp.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/language/conditions.fasl: $(fasldir)/nausicaa/language/oopp.fasl $(fasldir)/nausicaa/language/builtins.fasl $(fasldir)/nausicaa/language/auxiliary-syntaxes.fasl
$(fasldir)/nausicaa/language/builtins.fasl: $(fasldir)/vicare/language-extensions/sentinels.fasl $(fasldir)/nausicaa/language/oopp.fasl $(fasldir)/nausicaa/language/multimethods.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/containers/bytevectors.fasl
$(fasldir)/nausicaa/language/multimethods.fasl: $(fasldir)/nausicaa/language/multimethods/types.fasl $(fasldir)/nausicaa/language/multimethods/methods-table.fasl $(fasldir)/nausicaa/language/auxiliary-syntaxes.fasl $(fasldir)/nausicaa/language/multimethods/auxiliary-syntaxes.fasl
$(fasldir)/nausicaa/language/multimethods/methods-table.fasl: $(fasldir)/nausicaa/language/symbols-tree.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/language/multimethods/types.fasl: $(fasldir)/nausicaa/language/oopp.fasl
$(fasldir)/nausicaa/language/oopp.fasl: $(fasldir)/nausicaa/language/oopp/auxiliary-syntaxes.fasl $(fasldir)/nausicaa/language/oopp/conditions.fasl $(fasldir)/nausicaa/language/oopp/oopp-syntax-helpers.fasl $(fasldir)/nausicaa/language/oopp/definition-parser-helpers.fasl $(fasldir)/nausicaa/language/oopp/configuration.fasl $(fasldir)/nausicaa/language/auxiliary-syntaxes.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/nausicaa/language/oopp/oopp-syntax-helpers.fasl: $(fasldir)/nausicaa/language/oopp/configuration.fasl $(fasldir)/nausicaa/language/oopp/auxiliary-syntaxes.fasl $(fasldir)/nausicaa/language/auxiliary-syntaxes.fasl $(fasldir)/nausicaa/language/oopp/conditions.fasl $(fasldir)/vicare/language-extensions/identifier-substitutions.fasl $(fasldir)/nausicaa/language/oopp/definition-parser-helpers.fasl
$(fasldir)/nausicaa/language/oopp/definition-parser-helpers.fasl: $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/language-extensions/identifier-substitutions.fasl $(fasldir)/nausicaa/language/oopp/configuration.fasl $(fasldir)/nausicaa/language/oopp/auxiliary-syntaxes.fasl $(fasldir)/nausicaa/language/auxiliary-syntaxes.fasl
$(fasldir)/srfi/%3a112.fasl: $(fasldir)/srfi/%3a112/environment-inquiry.fasl
$(fasldir)/srfi/%3a111.fasl: $(fasldir)/srfi/%3a111/boxes.fasl
$(fasldir)/srfi/%3a101/random-access-lists/equal.fasl: $(fasldir)/srfi/%3a101.fasl
$(fasldir)/srfi/%3a101/random-access-lists/syntax.fasl: $(fasldir)/srfi/%3a101.fasl
$(fasldir)/srfi/%3a101/random-access-lists/procedures.fasl: $(fasldir)/srfi/%3a101.fasl
$(fasldir)/srfi/%3a101/random-access-lists.fasl: $(fasldir)/srfi/%3a101.fasl
$(fasldir)/srfi/%3a99.fasl: $(fasldir)/srfi/%3a99/records.fasl
$(fasldir)/srfi/%3a99/records.fasl: $(fasldir)/srfi/%3a99/records/inspection.fasl $(fasldir)/srfi/%3a99/records/procedural.fasl $(fasldir)/srfi/%3a99/records/syntactic.fasl
$(fasldir)/srfi/%3a99/records/syntactic.fasl: $(fasldir)/srfi/%3a99/records/procedural.fasl
$(fasldir)/srfi/%3a99/records/procedural.fasl: $(fasldir)/srfi/%3a99/records/inspection.fasl
$(fasldir)/srfi/%3a99/records/inspection.fasl: $(fasldir)/srfi/%3a99/records/helper.fasl
$(fasldir)/srfi/%3a98.fasl: $(fasldir)/srfi/%3a98/os-environment-variables.fasl
$(fasldir)/srfi/%3a78.fasl: $(fasldir)/srfi/%3a78/lightweight-testing.fasl
$(fasldir)/srfi/%3a78/lightweight-testing.fasl: $(fasldir)/srfi/%3a39/parameters.fasl $(fasldir)/srfi/%3a42/eager-comprehensions.fasl
$(fasldir)/srfi/%3a69.fasl: $(fasldir)/srfi/%3a69/basic-hash-tables.fasl
$(fasldir)/srfi/%3a67.fasl: $(fasldir)/srfi/%3a67/compare-procedures.fasl
$(fasldir)/srfi/%3a67/compare-procedures.fasl: $(fasldir)/srfi/%3a27/random-bits.fasl
$(fasldir)/srfi/%3a64.fasl: $(fasldir)/srfi/%3a64/testing.fasl
$(fasldir)/srfi/%3a64/testing.fasl: $(fasldir)/srfi/%3a0/cond-expand.fasl $(fasldir)/srfi/%3a1/lists.fasl $(fasldir)/srfi/%3a6/basic-string-ports.fasl $(fasldir)/srfi/%3a9/records.fasl $(fasldir)/srfi/%3a39/parameters.fasl
$(fasldir)/srfi/%3a61.fasl: $(fasldir)/srfi/%3a61/cond.fasl
$(fasldir)/srfi/%3a48.fasl: $(fasldir)/srfi/%3a48/intermediate-format-strings.fasl
$(fasldir)/srfi/%3a48/intermediate-format-strings.fasl: $(fasldir)/srfi/%3a6/basic-string-ports.fasl $(fasldir)/srfi/%3a38/with-shared-structure.fasl
$(fasldir)/srfi/%3a45.fasl: $(fasldir)/srfi/%3a45/lazy.fasl
$(fasldir)/srfi/%3a43.fasl: $(fasldir)/srfi/%3a43/vectors.fasl
$(fasldir)/srfi/%3a43/vectors.fasl: $(fasldir)/srfi/%3a8/receive.fasl
$(fasldir)/srfi/%3a42.fasl: $(fasldir)/srfi/%3a42/eager-comprehensions.fasl
$(fasldir)/srfi/%3a42/eager-comprehensions.fasl: $(fasldir)/srfi/%3a39/parameters.fasl
$(fasldir)/srfi/%3a41.fasl: $(fasldir)/srfi/%3a41/streams.fasl
$(fasldir)/srfi/%3a41/streams.fasl: $(fasldir)/srfi/%3a41/streams/primitive.fasl $(fasldir)/srfi/%3a41/streams/derived.fasl
$(fasldir)/srfi/%3a41/streams/derived.fasl: $(fasldir)/srfi/%3a41/streams/primitive.fasl
$(fasldir)/srfi/%3a39.fasl: $(fasldir)/srfi/%3a39/parameters.fasl
$(fasldir)/srfi/%3a38.fasl: $(fasldir)/srfi/%3a38/with-shared-structure.fasl
$(fasldir)/srfi/%3a37.fasl: $(fasldir)/srfi/%3a37/args-fold.fasl
$(fasldir)/srfi/%3a31.fasl: $(fasldir)/srfi/%3a31/rec.fasl
$(fasldir)/srfi/%3a28.fasl: $(fasldir)/srfi/%3a28/basic-format-strings.fasl
$(fasldir)/srfi/%3a28/basic-format-strings.fasl: $(fasldir)/srfi/%3a6.fasl
$(fasldir)/srfi/%3a27.fasl: $(fasldir)/srfi/%3a27/random-bits.fasl
$(fasldir)/srfi/%3a27/random-bits.fasl: $(fasldir)/srfi/%3a19/time.fasl
$(fasldir)/srfi/%3a26.fasl: $(fasldir)/srfi/%3a26/cut.fasl
$(fasldir)/srfi/%3a25.fasl: $(fasldir)/srfi/%3a25/multi-dimensional-arrays.fasl
$(fasldir)/srfi/%3a25/multi-dimensional-arrays.fasl: $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/srfi/%3a23.fasl: $(fasldir)/srfi/%3a23/error.fasl
$(fasldir)/srfi/%3a19.fasl: $(fasldir)/srfi/%3a19/time.fasl
$(fasldir)/srfi/%3a19/time.fasl: $(fasldir)/srfi/%3a6/basic-string-ports.fasl
$(fasldir)/srfi/%3a16.fasl: $(fasldir)/srfi/%3a16/case-lambda.fasl
$(fasldir)/srfi/%3a14.fasl: $(fasldir)/srfi/%3a14/char-sets.fasl
$(fasldir)/srfi/%3a13.fasl: $(fasldir)/srfi/%3a13/strings.fasl
$(fasldir)/srfi/%3a13/strings.fasl: $(fasldir)/srfi/%3a14/char-sets.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/srfi/%3a14/char-sets.fasl: $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/srfi/%3a11.fasl: $(fasldir)/srfi/%3a11/let-values.fasl
$(fasldir)/srfi/%3a9.fasl: $(fasldir)/srfi/%3a9/records.fasl
$(fasldir)/srfi/%3a8.fasl: $(fasldir)/srfi/%3a8/receive.fasl
$(fasldir)/srfi/%3a6.fasl: $(fasldir)/srfi/%3a6/basic-string-ports.fasl
$(fasldir)/srfi/%3a2.fasl: $(fasldir)/srfi/%3a2/and-let%2a.fasl
$(fasldir)/srfi/%3a1.fasl: $(fasldir)/srfi/%3a1/lists.fasl
$(fasldir)/srfi/%3a1/lists.fasl: $(fasldir)/srfi/%3a8/receive.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/srfi/%3a0.fasl: $(fasldir)/srfi/%3a0/cond-expand.fasl
$(fasldir)/srfi/%3a0/cond-expand.fasl: $(fasldir)/vicare/language-extensions/cond-expand/registry.fasl
$(fasldir)/vicare/net/channels.fasl: $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/language-extensions/syntaxes.fasl
$(fasldir)/vicare/parser-tools/unix-pathnames.fasl: $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/parser-tools/silex.fasl: $(fasldir)/vicare/parser-tools/silex/lexer.fasl $(fasldir)/vicare/parser-tools/silex/semantic.fasl $(fasldir)/vicare/parser-tools/silex/action-l.fasl $(fasldir)/vicare/parser-tools/silex/class-l.fasl $(fasldir)/vicare/parser-tools/silex/macro-l.fasl $(fasldir)/vicare/parser-tools/silex/regexp-l.fasl $(fasldir)/vicare/parser-tools/silex/string-l.fasl $(fasldir)/vicare/parser-tools/silex/nested-comment-l.fasl $(fasldir)/vicare/language-extensions/makers.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/parser-tools/silex/nested-comment-l.fasl: $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/vicare/parser-tools/silex/semantic.fasl
$(fasldir)/vicare/parser-tools/silex/string-l.fasl: $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/vicare/parser-tools/silex/semantic.fasl
$(fasldir)/vicare/parser-tools/silex/regexp-l.fasl: $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/vicare/parser-tools/silex/semantic.fasl
$(fasldir)/vicare/parser-tools/silex/macro-l.fasl: $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/vicare/parser-tools/silex/semantic.fasl
$(fasldir)/vicare/parser-tools/silex/class-l.fasl: $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/vicare/parser-tools/silex/semantic.fasl
$(fasldir)/vicare/parser-tools/silex/action-l.fasl: $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/vicare/parser-tools/silex/semantic.fasl
$(fasldir)/vicare/parser-tools/silex/lexer.fasl: $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/vicare/parser-tools/silex/code-lexer-driver.fasl $(fasldir)/vicare/parser-tools/silex/tree-lexer-driver.fasl $(fasldir)/vicare/parser-tools/silex/char-lexer-driver.fasl
$(fasldir)/vicare/parser-tools/silex/char-lexer-driver.fasl: $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/vicare/parser-tools/silex/tree-lexer-driver.fasl
$(fasldir)/vicare/parser-tools/silex/tree-lexer-driver.fasl: $(fasldir)/vicare/parser-tools/silex/input-system.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/parser-tools/silex/input-system.fasl: $(fasldir)/vicare/language-extensions/makers.fasl
$(fasldir)/vicare/containers/queues.fasl: $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/containers/stacks.fasl: $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/containers/arrays.fasl: $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/containers/bytevectors/s8.fasl: $(fasldir)/vicare/language-extensions/ascii-chars.fasl $(fasldir)/vicare/containers/bytevectors/s8low.fasl $(fasldir)/vicare/containers/bytevectors/generic.fasl $(fasldir)/vicare/containers/auxiliary-syntaxes.fasl
$(fasldir)/vicare/containers/bytevectors/s8low.fasl: $(fasldir)/vicare/containers/bytevectors/generic-low.fasl $(fasldir)/vicare/containers/char-sets.fasl $(fasldir)/vicare/containers/knuth-morris-pratt.fasl
$(fasldir)/vicare/containers/bytevectors/u8.fasl: $(fasldir)/vicare/language-extensions/ascii-chars.fasl $(fasldir)/vicare/containers/bytevectors/u8low.fasl $(fasldir)/vicare/containers/bytevectors/generic.fasl $(fasldir)/vicare/containers/auxiliary-syntaxes.fasl
$(fasldir)/vicare/containers/bytevectors/generic.fasl: $(fasldir)/vicare/containers/auxiliary-syntaxes.fasl
$(fasldir)/vicare/containers/bytevectors/u8low.fasl: $(fasldir)/vicare/containers/bytevectors/generic-low.fasl $(fasldir)/vicare/containers/char-sets.fasl $(fasldir)/vicare/containers/knuth-morris-pratt.fasl
$(fasldir)/vicare/containers/bytevectors/generic-low.fasl: $(fasldir)/vicare/containers/auxiliary-syntaxes.fasl
$(fasldir)/vicare/containers/one-dimension-cc.fasl: $(fasldir)/vicare/containers/lists.fasl
$(fasldir)/vicare/containers/one-dimension-co.fasl: $(fasldir)/vicare/containers/lists.fasl
$(fasldir)/vicare/containers/strings/rabin-karp.fasl: $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/language-extensions/infix.fasl
$(fasldir)/vicare/containers/lists.fasl: $(fasldir)/vicare/containers/lists/stx.fasl $(fasldir)/vicare/containers/lists/low.fasl
$(fasldir)/vicare/containers/lists/stx.fasl: $(fasldir)/vicare/containers/lists/low.fasl
$(fasldir)/vicare/containers/char-sets/categories.fasl: $(fasldir)/vicare/containers/char-sets.fasl
$(fasldir)/vicare/containers/char-sets/blocks.fasl: $(fasldir)/vicare/containers/char-sets.fasl
$(fasldir)/vicare/containers/bytevector-compounds/unsafe.fasl: $(fasldir)/vicare/containers/bytevector-compounds/core.fasl
$(fasldir)/vicare/containers/bytevector-compounds.fasl: $(fasldir)/vicare/containers/bytevector-compounds/core.fasl
$(fasldir)/vicare/containers/bytevector-compounds/core.fasl: $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/containers/object-properties.fasl: $(fasldir)/vicare/containers/weak-hashtables.fasl $(fasldir)/vicare/language-extensions/sentinels.fasl
$(fasldir)/vicare/containers/weak-hashtables.fasl: $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/numerics/flonum-formatter.fasl: $(fasldir)/vicare/language-extensions/syntaxes.fasl
$(fasldir)/vicare/numerics/flonum-parser.fasl: $(fasldir)/vicare/language-extensions/syntaxes.fasl
$(fasldir)/vicare/crypto/randomisations/vectors.fasl: $(fasldir)/vicare/crypto/randomisations.fasl $(fasldir)/vicare/containers/vectors/low.fasl $(fasldir)/vicare/containers/vectors.fasl
$(fasldir)/vicare/containers/vectors.fasl: $(fasldir)/vicare/containers/vectors/low.fasl $(fasldir)/vicare/containers/auxiliary-syntaxes.fasl
$(fasldir)/vicare/containers/vectors/low.fasl: $(fasldir)/vicare/containers/knuth-morris-pratt.fasl
$(fasldir)/vicare/crypto/randomisations/strings.fasl: $(fasldir)/vicare/crypto/randomisations.fasl $(fasldir)/vicare/containers/strings/low.fasl $(fasldir)/vicare/containers/strings.fasl
$(fasldir)/vicare/containers/strings.fasl: $(fasldir)/vicare/containers/strings/low.fasl $(fasldir)/vicare/containers/auxiliary-syntaxes.fasl
$(fasldir)/vicare/containers/strings/low.fasl: $(fasldir)/vicare/containers/char-sets.fasl $(fasldir)/vicare/containers/knuth-morris-pratt.fasl
$(fasldir)/vicare/crypto/randomisations/mersenne.fasl: $(fasldir)/vicare/crypto/randomisations.fasl $(fasldir)/vicare/crypto/randomisations/low.fasl
$(fasldir)/vicare/crypto/randomisations/marsaglia.fasl: $(fasldir)/vicare/crypto/randomisations.fasl $(fasldir)/vicare/crypto/randomisations/low.fasl
$(fasldir)/vicare/crypto/randomisations/lists.fasl: $(fasldir)/vicare/crypto/randomisations.fasl
$(fasldir)/vicare/crypto/randomisations/distributions.fasl: $(fasldir)/vicare/crypto/randomisations.fasl
$(fasldir)/vicare/crypto/randomisations/cmrg.fasl: $(fasldir)/vicare/crypto/randomisations.fasl $(fasldir)/vicare/crypto/randomisations/low.fasl
$(fasldir)/vicare/crypto/randomisations/borosh.fasl: $(fasldir)/vicare/crypto/randomisations.fasl $(fasldir)/vicare/crypto/randomisations/low.fasl
$(fasldir)/vicare/crypto/randomisations/blum-blum-shub.fasl: $(fasldir)/vicare/crypto/randomisations.fasl $(fasldir)/vicare/crypto/randomisations/low.fasl
$(fasldir)/vicare/language-extensions/identifier-properties.fasl: $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/language-extensions/identifier-alists.fasl $(fasldir)/vicare/language-extensions/sentinels.fasl $(fasldir)/vicare/language-extensions/variables.fasl
$(fasldir)/vicare/language-extensions/callables.fasl: $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/language-extensions/hooks.fasl: $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/language-extensions/comparisons.fasl: $(fasldir)/vicare/crypto/randomisations.fasl
$(fasldir)/vicare/crypto/randomisations.fasl: $(fasldir)/vicare/crypto/randomisations/low.fasl
$(fasldir)/vicare/language-extensions/ascii-chars.fasl: $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/language-extensions/ascii-chars/syntaxes.fasl
$(fasldir)/vicare/language-extensions/ascii-chars/syntaxes.fasl: $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/language-extensions/custom-ports.fasl: $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/language-extensions/keywords.fasl: $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/language-extensions/infix.fasl: $(fasldir)/vicare/language-extensions/infix/parser-utils.fasl $(fasldir)/vicare/language-extensions/infix/auxiliary-syntaxes.fasl $(fasldir)/vicare/language-extensions/increments.fasl
$(fasldir)/vicare/language-extensions/infix/parser-utils.fasl: $(fasldir)/vicare/language-extensions/increments.fasl $(fasldir)/vicare/language-extensions/infix/auxiliary-syntaxes.fasl $(fasldir)/vicare/language-extensions/infix/tokens.fasl $(fasldir)/vicare/language-extensions/infix/parser-table.fasl
$(fasldir)/vicare/language-extensions/infix/parser-table.fasl: $(fasldir)/vicare/language-extensions/infix/lr-driver.fasl
$(fasldir)/vicare/language-extensions/infix/lr-driver.fasl: $(fasldir)/vicare/language-extensions/infix/tokens.fasl
$(fasldir)/vicare/language-extensions/coroutines.fasl: $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/language-extensions/simple-match.fasl: $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/language-extensions/amb.fasl: $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/arguments/general-c-buffers.fasl: $(fasldir)/vicare/language-extensions/syntaxes.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/language-extensions/syntaxes.fasl: $(fasldir)/vicare/platform/configuration.fasl $(fasldir)/vicare/arguments/validation.fasl $(fasldir)/vicare/unsafe/operations.fasl $(fasldir)/vicare/language-extensions/define-record-extended.fasl
$(fasldir)/vicare/language-extensions/define-record-extended.fasl: $(fasldir)/vicare/arguments/validation.fasl
$(fasldir)/vicare/arguments/validation.fasl: $(fasldir)/vicare/platform/configuration.fasl $(fasldir)/vicare/platform/constants.fasl $(fasldir)/vicare/platform/words.fasl $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/language-extensions/cond-expand/feature-cond.fasl: $(fasldir)/vicare/language-extensions/cond-expand/registry.fasl
$(fasldir)/vicare/language-extensions/cond-expand.fasl: $(fasldir)/vicare/language-extensions/cond-expand/registry.fasl
$(fasldir)/vicare/language-extensions/cond-expand/registry.fasl: $(fasldir)/vicare/language-extensions/cond-expand/platform-features.fasl $(fasldir)/vicare/language-extensions/cond-expand/configuration-features.fasl
$(fasldir)/vicare/language-extensions/cond-expand/platform-features.fasl: $(fasldir)/vicare/language-extensions/cond-expand/OS-id-features.fasl
$(fasldir)/vicare/unsafe/unicode.fasl: $(fasldir)/vicare/unsafe/operations.fasl
$(fasldir)/vicare/unsafe/operations.fasl: $(fasldir)/vicare/platform/configuration.fasl
$(fasldir)/vicare/platform/utilities.fasl: $(fasldir)/vicare/platform/constants.fasl
$(fasldir)/vicare/platform/constants.fasl: $(fasldir)/vicare/platform/errno.fasl
$(fasldir)/vicare/platform/words.fasl: $(fasldir)/vicare/platform/configuration.fasl
