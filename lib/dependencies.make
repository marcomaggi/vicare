## dependencies.make --
#
# Automatically built.

EXTRA_DIST +=  \
	lib/vicare/platform/configuration.sls.in \
	lib/nausicaa/uri/pathnames.sls.in \
	lib/vicare/platform/constants.sls.in \
	lib/vicare/platform/errno.sls.in \
	lib/vicare/platform/words.sls.in

lib/vicare/platform/configuration.fasl: \
		lib/vicare/platform/configuration.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_platform_configuration_fasldir = $(bundledlibsdir)/vicare/platform
lib_vicare_platform_configuration_slsdir  = $(bundledlibsdir)/vicare/platform
nodist_lib_vicare_platform_configuration_fasl_DATA = lib/vicare/platform/configuration.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_platform_configuration_sls_DATA = lib/vicare/platform/configuration.sls
endif
CLEANFILES += lib/vicare/platform/configuration.fasl

lib/vicare/platform/words.fasl: \
		lib/vicare/platform/words.sls \
		lib/vicare/platform/configuration.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_platform_words_fasldir = $(bundledlibsdir)/vicare/platform
lib_vicare_platform_words_slsdir  = $(bundledlibsdir)/vicare/platform
nodist_lib_vicare_platform_words_fasl_DATA = lib/vicare/platform/words.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_platform_words_sls_DATA = lib/vicare/platform/words.sls
endif
CLEANFILES += lib/vicare/platform/words.fasl

lib/vicare/platform/errno.fasl: \
		lib/vicare/platform/errno.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_platform_errno_fasldir = $(bundledlibsdir)/vicare/platform
lib_vicare_platform_errno_slsdir  = $(bundledlibsdir)/vicare/platform
nodist_lib_vicare_platform_errno_fasl_DATA = lib/vicare/platform/errno.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_platform_errno_sls_DATA = lib/vicare/platform/errno.sls
endif
CLEANFILES += lib/vicare/platform/errno.fasl

lib/vicare/platform/constants.fasl: \
		lib/vicare/platform/constants.sls \
		lib/vicare/platform/errno.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_platform_constants_fasldir = $(bundledlibsdir)/vicare/platform
lib_vicare_platform_constants_slsdir  = $(bundledlibsdir)/vicare/platform
nodist_lib_vicare_platform_constants_fasl_DATA = lib/vicare/platform/constants.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_platform_constants_sls_DATA = lib/vicare/platform/constants.sls
endif
CLEANFILES += lib/vicare/platform/constants.fasl

lib/vicare/platform/features.fasl: \
		lib/vicare/platform/features.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_platform_features_fasldir = $(bundledlibsdir)/vicare/platform
lib_vicare_platform_features_slsdir  = $(bundledlibsdir)/vicare/platform
nodist_lib_vicare_platform_features_fasl_DATA = lib/vicare/platform/features.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_platform_features_sls_DATA = lib/vicare/platform/features.sls
endif
CLEANFILES += lib/vicare/platform/features.fasl

lib/vicare/platform/utilities.fasl: \
		lib/vicare/platform/utilities.sls \
		lib/vicare/platform/constants.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_platform_utilities_fasldir = $(bundledlibsdir)/vicare/platform
lib_vicare_platform_utilities_slsdir  = $(bundledlibsdir)/vicare/platform
nodist_lib_vicare_platform_utilities_fasl_DATA = lib/vicare/platform/utilities.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_platform_utilities_sls_DATA = lib/vicare/platform/utilities.sls
endif
EXTRA_DIST += lib/vicare/platform/utilities.sls
CLEANFILES += lib/vicare/platform/utilities.fasl

lib/vicare/unsafe/capi.fasl: \
		lib/vicare/unsafe/capi.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_unsafe_capi_fasldir = $(bundledlibsdir)/vicare/unsafe
lib_vicare_unsafe_capi_slsdir  = $(bundledlibsdir)/vicare/unsafe
nodist_lib_vicare_unsafe_capi_fasl_DATA = lib/vicare/unsafe/capi.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_unsafe_capi_sls_DATA = lib/vicare/unsafe/capi.sls
endif
EXTRA_DIST += lib/vicare/unsafe/capi.sls
CLEANFILES += lib/vicare/unsafe/capi.fasl

lib/vicare/unsafe/operations.fasl: \
		lib/vicare/unsafe/operations.sls \
		lib/vicare/platform/configuration.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_unsafe_operations_fasldir = $(bundledlibsdir)/vicare/unsafe
lib_vicare_unsafe_operations_slsdir  = $(bundledlibsdir)/vicare/unsafe
nodist_lib_vicare_unsafe_operations_fasl_DATA = lib/vicare/unsafe/operations.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_unsafe_operations_sls_DATA = lib/vicare/unsafe/operations.sls
endif
EXTRA_DIST += lib/vicare/unsafe/operations.sls
CLEANFILES += lib/vicare/unsafe/operations.fasl

lib/vicare/unsafe/unicode.fasl: \
		lib/vicare/unsafe/unicode.sls \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_unsafe_unicode_fasldir = $(bundledlibsdir)/vicare/unsafe
lib_vicare_unsafe_unicode_slsdir  = $(bundledlibsdir)/vicare/unsafe
nodist_lib_vicare_unsafe_unicode_fasl_DATA = lib/vicare/unsafe/unicode.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_unsafe_unicode_sls_DATA = lib/vicare/unsafe/unicode.sls
endif
EXTRA_DIST += lib/vicare/unsafe/unicode.sls
CLEANFILES += lib/vicare/unsafe/unicode.fasl

lib/vicare/language-extensions/cond-expand.fasl: \
		lib/vicare/language-extensions/cond-expand.sls \
		lib/vicare/language-extensions/cond-expand/registry.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_cond_expand_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_cond_expand_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_cond_expand_fasl_DATA = lib/vicare/language-extensions/cond-expand.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_cond_expand_sls_DATA = lib/vicare/language-extensions/cond-expand.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/cond-expand.sls
CLEANFILES += lib/vicare/language-extensions/cond-expand.fasl

lib/vicare/language-extensions/cond-expand/registry.fasl: \
		lib/vicare/language-extensions/cond-expand/registry.sls \
		lib/vicare/language-extensions/cond-expand/platform-features.fasl \
		lib/vicare/language-extensions/cond-expand/configuration-features.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_cond_expand_registry_fasldir = $(bundledlibsdir)/vicare/language-extensions/cond-expand
lib_vicare_language_extensions_cond_expand_registry_slsdir  = $(bundledlibsdir)/vicare/language-extensions/cond-expand
nodist_lib_vicare_language_extensions_cond_expand_registry_fasl_DATA = lib/vicare/language-extensions/cond-expand/registry.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_cond_expand_registry_sls_DATA = lib/vicare/language-extensions/cond-expand/registry.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/cond-expand/registry.sls
CLEANFILES += lib/vicare/language-extensions/cond-expand/registry.fasl

lib/vicare/language-extensions/cond-expand/platform-features.fasl: \
		lib/vicare/language-extensions/cond-expand/platform-features.sls \
		lib/vicare/language-extensions/cond-expand/OS-id-features.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_cond_expand_platform_features_fasldir = $(bundledlibsdir)/vicare/language-extensions/cond-expand
lib_vicare_language_extensions_cond_expand_platform_features_slsdir  = $(bundledlibsdir)/vicare/language-extensions/cond-expand
nodist_lib_vicare_language_extensions_cond_expand_platform_features_fasl_DATA = lib/vicare/language-extensions/cond-expand/platform-features.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_cond_expand_platform_features_sls_DATA = lib/vicare/language-extensions/cond-expand/platform-features.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/cond-expand/platform-features.sls
CLEANFILES += lib/vicare/language-extensions/cond-expand/platform-features.fasl

lib/vicare/language-extensions/cond-expand/OS-id-features.fasl: \
		lib/vicare/language-extensions/cond-expand/OS-id-features.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_cond_expand_OS_id_features_fasldir = $(bundledlibsdir)/vicare/language-extensions/cond-expand
lib_vicare_language_extensions_cond_expand_OS_id_features_slsdir  = $(bundledlibsdir)/vicare/language-extensions/cond-expand
nodist_lib_vicare_language_extensions_cond_expand_OS_id_features_fasl_DATA = lib/vicare/language-extensions/cond-expand/OS-id-features.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_cond_expand_OS_id_features_sls_DATA = lib/vicare/language-extensions/cond-expand/OS-id-features.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/cond-expand/OS-id-features.sls
CLEANFILES += lib/vicare/language-extensions/cond-expand/OS-id-features.fasl

lib/vicare/language-extensions/cond-expand/configuration-features.fasl: \
		lib/vicare/language-extensions/cond-expand/configuration-features.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_cond_expand_configuration_features_fasldir = $(bundledlibsdir)/vicare/language-extensions/cond-expand
lib_vicare_language_extensions_cond_expand_configuration_features_slsdir  = $(bundledlibsdir)/vicare/language-extensions/cond-expand
nodist_lib_vicare_language_extensions_cond_expand_configuration_features_fasl_DATA = lib/vicare/language-extensions/cond-expand/configuration-features.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_cond_expand_configuration_features_sls_DATA = lib/vicare/language-extensions/cond-expand/configuration-features.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/cond-expand/configuration-features.sls
CLEANFILES += lib/vicare/language-extensions/cond-expand/configuration-features.fasl

lib/vicare/language-extensions/cond-expand/feature-cond.fasl: \
		lib/vicare/language-extensions/cond-expand/feature-cond.sls \
		lib/vicare/language-extensions/cond-expand/registry.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_cond_expand_feature_cond_fasldir = $(bundledlibsdir)/vicare/language-extensions/cond-expand
lib_vicare_language_extensions_cond_expand_feature_cond_slsdir  = $(bundledlibsdir)/vicare/language-extensions/cond-expand
nodist_lib_vicare_language_extensions_cond_expand_feature_cond_fasl_DATA = lib/vicare/language-extensions/cond-expand/feature-cond.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_cond_expand_feature_cond_sls_DATA = lib/vicare/language-extensions/cond-expand/feature-cond.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/cond-expand/feature-cond.sls
CLEANFILES += lib/vicare/language-extensions/cond-expand/feature-cond.fasl

lib/vicare/language-extensions/cond-expand/helpers.fasl: \
		lib/vicare/language-extensions/cond-expand/helpers.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_cond_expand_helpers_fasldir = $(bundledlibsdir)/vicare/language-extensions/cond-expand
lib_vicare_language_extensions_cond_expand_helpers_slsdir  = $(bundledlibsdir)/vicare/language-extensions/cond-expand
nodist_lib_vicare_language_extensions_cond_expand_helpers_fasl_DATA = lib/vicare/language-extensions/cond-expand/helpers.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_cond_expand_helpers_sls_DATA = lib/vicare/language-extensions/cond-expand/helpers.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/cond-expand/helpers.sls
CLEANFILES += lib/vicare/language-extensions/cond-expand/helpers.fasl

lib/vicare/arguments/validation.fasl: \
		lib/vicare/arguments/validation.sls \
		lib/vicare/platform/configuration.fasl \
		lib/vicare/platform/constants.fasl \
		lib/vicare/platform/words.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_arguments_validation_fasldir = $(bundledlibsdir)/vicare/arguments
lib_vicare_arguments_validation_slsdir  = $(bundledlibsdir)/vicare/arguments
nodist_lib_vicare_arguments_validation_fasl_DATA = lib/vicare/arguments/validation.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_arguments_validation_sls_DATA = lib/vicare/arguments/validation.sls
endif
EXTRA_DIST += lib/vicare/arguments/validation.sls
CLEANFILES += lib/vicare/arguments/validation.fasl

lib/vicare/arguments/general-c-buffers.fasl: \
		lib/vicare/arguments/general-c-buffers.sls \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_arguments_general_c_buffers_fasldir = $(bundledlibsdir)/vicare/arguments
lib_vicare_arguments_general_c_buffers_slsdir  = $(bundledlibsdir)/vicare/arguments
nodist_lib_vicare_arguments_general_c_buffers_fasl_DATA = lib/vicare/arguments/general-c-buffers.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_arguments_general_c_buffers_sls_DATA = lib/vicare/arguments/general-c-buffers.sls
endif
EXTRA_DIST += lib/vicare/arguments/general-c-buffers.sls
CLEANFILES += lib/vicare/arguments/general-c-buffers.fasl

lib/vicare/language-extensions/syntaxes.fasl: \
		lib/vicare/language-extensions/syntaxes.sls \
		lib/vicare/platform/configuration.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/language-extensions/define-record-extended.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_syntaxes_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_syntaxes_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_syntaxes_fasl_DATA = lib/vicare/language-extensions/syntaxes.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_syntaxes_sls_DATA = lib/vicare/language-extensions/syntaxes.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/syntaxes.sls
CLEANFILES += lib/vicare/language-extensions/syntaxes.fasl

lib/vicare/language-extensions/define-record-extended.fasl: \
		lib/vicare/language-extensions/define-record-extended.sls \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_define_record_extended_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_define_record_extended_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_define_record_extended_fasl_DATA = lib/vicare/language-extensions/define-record-extended.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_define_record_extended_sls_DATA = lib/vicare/language-extensions/define-record-extended.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/define-record-extended.sls
CLEANFILES += lib/vicare/language-extensions/define-record-extended.fasl

lib/vicare/language-extensions/amb.fasl: \
		lib/vicare/language-extensions/amb.sls \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_amb_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_amb_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_amb_fasl_DATA = lib/vicare/language-extensions/amb.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_amb_sls_DATA = lib/vicare/language-extensions/amb.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/amb.sls
CLEANFILES += lib/vicare/language-extensions/amb.fasl

lib/vicare/language-extensions/simple-match.fasl: \
		lib/vicare/language-extensions/simple-match.sls \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_simple_match_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_simple_match_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_simple_match_fasl_DATA = lib/vicare/language-extensions/simple-match.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_simple_match_sls_DATA = lib/vicare/language-extensions/simple-match.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/simple-match.sls
CLEANFILES += lib/vicare/language-extensions/simple-match.fasl

lib/vicare/language-extensions/keywords.fasl: \
		lib/vicare/language-extensions/keywords.sls \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_keywords_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_keywords_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_keywords_fasl_DATA = lib/vicare/language-extensions/keywords.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_keywords_sls_DATA = lib/vicare/language-extensions/keywords.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/keywords.sls
CLEANFILES += lib/vicare/language-extensions/keywords.fasl

lib/vicare/language-extensions/sentinels.fasl: \
		lib/vicare/language-extensions/sentinels.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_sentinels_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_sentinels_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_sentinels_fasl_DATA = lib/vicare/language-extensions/sentinels.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_sentinels_sls_DATA = lib/vicare/language-extensions/sentinels.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/sentinels.sls
CLEANFILES += lib/vicare/language-extensions/sentinels.fasl

lib/vicare/language-extensions/namespaces.fasl: \
		lib/vicare/language-extensions/namespaces.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_namespaces_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_namespaces_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_namespaces_fasl_DATA = lib/vicare/language-extensions/namespaces.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_namespaces_sls_DATA = lib/vicare/language-extensions/namespaces.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/namespaces.sls
CLEANFILES += lib/vicare/language-extensions/namespaces.fasl

lib/vicare/language-extensions/custom-ports.fasl: \
		lib/vicare/language-extensions/custom-ports.sls \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_custom_ports_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_custom_ports_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_custom_ports_fasl_DATA = lib/vicare/language-extensions/custom-ports.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_custom_ports_sls_DATA = lib/vicare/language-extensions/custom-ports.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/custom-ports.sls
CLEANFILES += lib/vicare/language-extensions/custom-ports.fasl

lib/vicare/language-extensions/variables.fasl: \
		lib/vicare/language-extensions/variables.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_variables_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_variables_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_variables_fasl_DATA = lib/vicare/language-extensions/variables.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_variables_sls_DATA = lib/vicare/language-extensions/variables.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/variables.sls
CLEANFILES += lib/vicare/language-extensions/variables.fasl

lib/vicare/language-extensions/streams.fasl: \
		lib/vicare/language-extensions/streams.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_streams_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_streams_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_streams_fasl_DATA = lib/vicare/language-extensions/streams.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_streams_sls_DATA = lib/vicare/language-extensions/streams.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/streams.sls
CLEANFILES += lib/vicare/language-extensions/streams.fasl

lib/vicare/language-extensions/loops.fasl: \
		lib/vicare/language-extensions/loops.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_loops_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_loops_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_loops_fasl_DATA = lib/vicare/language-extensions/loops.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_loops_sls_DATA = lib/vicare/language-extensions/loops.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/loops.sls
CLEANFILES += lib/vicare/language-extensions/loops.fasl

lib/vicare/language-extensions/ascii-chars.fasl: \
		lib/vicare/language-extensions/ascii-chars.sls \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/language-extensions/ascii-chars/syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_ascii_chars_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_ascii_chars_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_ascii_chars_fasl_DATA = lib/vicare/language-extensions/ascii-chars.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_ascii_chars_sls_DATA = lib/vicare/language-extensions/ascii-chars.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/ascii-chars.sls
CLEANFILES += lib/vicare/language-extensions/ascii-chars.fasl

lib/vicare/language-extensions/ascii-chars/syntaxes.fasl: \
		lib/vicare/language-extensions/ascii-chars/syntaxes.sls \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_ascii_chars_syntaxes_fasldir = $(bundledlibsdir)/vicare/language-extensions/ascii-chars
lib_vicare_language_extensions_ascii_chars_syntaxes_slsdir  = $(bundledlibsdir)/vicare/language-extensions/ascii-chars
nodist_lib_vicare_language_extensions_ascii_chars_syntaxes_fasl_DATA = lib/vicare/language-extensions/ascii-chars/syntaxes.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_ascii_chars_syntaxes_sls_DATA = lib/vicare/language-extensions/ascii-chars/syntaxes.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/ascii-chars/syntaxes.sls
CLEANFILES += lib/vicare/language-extensions/ascii-chars/syntaxes.fasl

lib/vicare/language-extensions/comparisons.fasl: \
		lib/vicare/language-extensions/comparisons.sls \
		lib/vicare/crypto/randomisations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_comparisons_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_comparisons_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_comparisons_fasl_DATA = lib/vicare/language-extensions/comparisons.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_comparisons_sls_DATA = lib/vicare/language-extensions/comparisons.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/comparisons.sls
CLEANFILES += lib/vicare/language-extensions/comparisons.fasl

lib/vicare/crypto/randomisations.fasl: \
		lib/vicare/crypto/randomisations.sls \
		lib/vicare/crypto/randomisations/low.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_fasldir = $(bundledlibsdir)/vicare/crypto
lib_vicare_crypto_randomisations_slsdir  = $(bundledlibsdir)/vicare/crypto
nodist_lib_vicare_crypto_randomisations_fasl_DATA = lib/vicare/crypto/randomisations.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_sls_DATA = lib/vicare/crypto/randomisations.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations.sls
CLEANFILES += lib/vicare/crypto/randomisations.fasl

lib/vicare/crypto/randomisations/low.fasl: \
		lib/vicare/crypto/randomisations/low.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_low_fasldir = $(bundledlibsdir)/vicare/crypto/randomisations
lib_vicare_crypto_randomisations_low_slsdir  = $(bundledlibsdir)/vicare/crypto/randomisations
nodist_lib_vicare_crypto_randomisations_low_fasl_DATA = lib/vicare/crypto/randomisations/low.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_low_sls_DATA = lib/vicare/crypto/randomisations/low.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations/low.sls
CLEANFILES += lib/vicare/crypto/randomisations/low.fasl

lib/vicare/language-extensions/hooks.fasl: \
		lib/vicare/language-extensions/hooks.sls \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_hooks_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_hooks_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_hooks_fasl_DATA = lib/vicare/language-extensions/hooks.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_hooks_sls_DATA = lib/vicare/language-extensions/hooks.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/hooks.sls
CLEANFILES += lib/vicare/language-extensions/hooks.fasl

lib/vicare/language-extensions/callables.fasl: \
		lib/vicare/language-extensions/callables.sls \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_callables_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_callables_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_callables_fasl_DATA = lib/vicare/language-extensions/callables.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_callables_sls_DATA = lib/vicare/language-extensions/callables.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/callables.sls
CLEANFILES += lib/vicare/language-extensions/callables.fasl

lib/vicare/language-extensions/c-enumerations.fasl: \
		lib/vicare/language-extensions/c-enumerations.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_c_enumerations_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_c_enumerations_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_c_enumerations_fasl_DATA = lib/vicare/language-extensions/c-enumerations.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_c_enumerations_sls_DATA = lib/vicare/language-extensions/c-enumerations.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/c-enumerations.sls
CLEANFILES += lib/vicare/language-extensions/c-enumerations.fasl

lib/vicare/language-extensions/identifier-substitutions.fasl: \
		lib/vicare/language-extensions/identifier-substitutions.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_identifier_substitutions_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_identifier_substitutions_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_identifier_substitutions_fasl_DATA = lib/vicare/language-extensions/identifier-substitutions.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_identifier_substitutions_sls_DATA = lib/vicare/language-extensions/identifier-substitutions.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/identifier-substitutions.sls
CLEANFILES += lib/vicare/language-extensions/identifier-substitutions.fasl

lib/vicare/language-extensions/makers.fasl: \
		lib/vicare/language-extensions/makers.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_makers_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_makers_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_makers_fasl_DATA = lib/vicare/language-extensions/makers.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_makers_sls_DATA = lib/vicare/language-extensions/makers.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/makers.sls
CLEANFILES += lib/vicare/language-extensions/makers.fasl

lib/vicare/language-extensions/ensure.fasl: \
		lib/vicare/language-extensions/ensure.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_ensure_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_ensure_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_ensure_fasl_DATA = lib/vicare/language-extensions/ensure.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_ensure_sls_DATA = lib/vicare/language-extensions/ensure.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/ensure.sls
CLEANFILES += lib/vicare/language-extensions/ensure.fasl

lib/vicare/language-extensions/conditions-and-restarts.fasl: \
		lib/vicare/language-extensions/conditions-and-restarts.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_language_extensions_conditions_and_restarts_fasldir = $(bundledlibsdir)/vicare/language-extensions
lib_vicare_language_extensions_conditions_and_restarts_slsdir  = $(bundledlibsdir)/vicare/language-extensions
nodist_lib_vicare_language_extensions_conditions_and_restarts_fasl_DATA = lib/vicare/language-extensions/conditions-and-restarts.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_language_extensions_conditions_and_restarts_sls_DATA = lib/vicare/language-extensions/conditions-and-restarts.sls
endif
EXTRA_DIST += lib/vicare/language-extensions/conditions-and-restarts.sls
CLEANFILES += lib/vicare/language-extensions/conditions-and-restarts.fasl

lib/vicare/build-tools/automake.fasl: \
		lib/vicare/build-tools/automake.sls \
		lib/vicare/posix.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_build_tools_automake_fasldir = $(bundledlibsdir)/vicare/build-tools
lib_vicare_build_tools_automake_slsdir  = $(bundledlibsdir)/vicare/build-tools
nodist_lib_vicare_build_tools_automake_fasl_DATA = lib/vicare/build-tools/automake.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_build_tools_automake_sls_DATA = lib/vicare/build-tools/automake.sls
endif
EXTRA_DIST += lib/vicare/build-tools/automake.sls
CLEANFILES += lib/vicare/build-tools/automake.fasl

lib/vicare/posix.fasl: \
		lib/vicare/posix.sls \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/platform/constants.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/arguments/general-c-buffers.fasl \
		lib/vicare/unsafe/capi.fasl \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/platform/words.fasl \
		lib/vicare/language-extensions/cond-expand.fasl \
		lib/vicare/containers/weak-hashtables.fasl \
		lib/vicare/platform/features.fasl \
		lib/vicare/language-extensions/cond-expand/helpers.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_posix_fasldir = $(bundledlibsdir)/vicare
lib_vicare_posix_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_posix_fasl_DATA = lib/vicare/posix.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_sls_DATA = lib/vicare/posix.sls
endif
EXTRA_DIST += lib/vicare/posix.sls
CLEANFILES += lib/vicare/posix.fasl

lib/vicare/containers/weak-hashtables.fasl: \
		lib/vicare/containers/weak-hashtables.sls \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_weak_hashtables_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_weak_hashtables_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_weak_hashtables_fasl_DATA = lib/vicare/containers/weak-hashtables.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_weak_hashtables_sls_DATA = lib/vicare/containers/weak-hashtables.sls
endif
EXTRA_DIST += lib/vicare/containers/weak-hashtables.sls
CLEANFILES += lib/vicare/containers/weak-hashtables.fasl

lib/vicare/checks.fasl: \
		lib/vicare/checks.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_checks_fasldir = $(bundledlibsdir)/vicare
lib_vicare_checks_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_checks_fasl_DATA = lib/vicare/checks.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_checks_sls_DATA = lib/vicare/checks.sls
endif
EXTRA_DIST += lib/vicare/checks.sls
CLEANFILES += lib/vicare/checks.fasl

lib/vicare/crypto/randomisations/blum-blum-shub.fasl: \
		lib/vicare/crypto/randomisations/blum-blum-shub.sls \
		lib/vicare/crypto/randomisations.fasl \
		lib/vicare/crypto/randomisations/low.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_blum_blum_shub_fasldir = $(bundledlibsdir)/vicare/crypto/randomisations
lib_vicare_crypto_randomisations_blum_blum_shub_slsdir  = $(bundledlibsdir)/vicare/crypto/randomisations
nodist_lib_vicare_crypto_randomisations_blum_blum_shub_fasl_DATA = lib/vicare/crypto/randomisations/blum-blum-shub.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_blum_blum_shub_sls_DATA = lib/vicare/crypto/randomisations/blum-blum-shub.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations/blum-blum-shub.sls
CLEANFILES += lib/vicare/crypto/randomisations/blum-blum-shub.fasl

lib/vicare/crypto/randomisations/borosh.fasl: \
		lib/vicare/crypto/randomisations/borosh.sls \
		lib/vicare/crypto/randomisations.fasl \
		lib/vicare/crypto/randomisations/low.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_borosh_fasldir = $(bundledlibsdir)/vicare/crypto/randomisations
lib_vicare_crypto_randomisations_borosh_slsdir  = $(bundledlibsdir)/vicare/crypto/randomisations
nodist_lib_vicare_crypto_randomisations_borosh_fasl_DATA = lib/vicare/crypto/randomisations/borosh.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_borosh_sls_DATA = lib/vicare/crypto/randomisations/borosh.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations/borosh.sls
CLEANFILES += lib/vicare/crypto/randomisations/borosh.fasl

lib/vicare/crypto/randomisations/cmrg.fasl: \
		lib/vicare/crypto/randomisations/cmrg.sls \
		lib/vicare/crypto/randomisations.fasl \
		lib/vicare/crypto/randomisations/low.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_cmrg_fasldir = $(bundledlibsdir)/vicare/crypto/randomisations
lib_vicare_crypto_randomisations_cmrg_slsdir  = $(bundledlibsdir)/vicare/crypto/randomisations
nodist_lib_vicare_crypto_randomisations_cmrg_fasl_DATA = lib/vicare/crypto/randomisations/cmrg.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_cmrg_sls_DATA = lib/vicare/crypto/randomisations/cmrg.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations/cmrg.sls
CLEANFILES += lib/vicare/crypto/randomisations/cmrg.fasl

lib/vicare/crypto/randomisations/distributions.fasl: \
		lib/vicare/crypto/randomisations/distributions.sls \
		lib/vicare/crypto/randomisations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_distributions_fasldir = $(bundledlibsdir)/vicare/crypto/randomisations
lib_vicare_crypto_randomisations_distributions_slsdir  = $(bundledlibsdir)/vicare/crypto/randomisations
nodist_lib_vicare_crypto_randomisations_distributions_fasl_DATA = lib/vicare/crypto/randomisations/distributions.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_distributions_sls_DATA = lib/vicare/crypto/randomisations/distributions.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations/distributions.sls
CLEANFILES += lib/vicare/crypto/randomisations/distributions.fasl

lib/vicare/crypto/randomisations/lists.fasl: \
		lib/vicare/crypto/randomisations/lists.sls \
		lib/vicare/crypto/randomisations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_lists_fasldir = $(bundledlibsdir)/vicare/crypto/randomisations
lib_vicare_crypto_randomisations_lists_slsdir  = $(bundledlibsdir)/vicare/crypto/randomisations
nodist_lib_vicare_crypto_randomisations_lists_fasl_DATA = lib/vicare/crypto/randomisations/lists.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_lists_sls_DATA = lib/vicare/crypto/randomisations/lists.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations/lists.sls
CLEANFILES += lib/vicare/crypto/randomisations/lists.fasl

lib/vicare/crypto/randomisations/marsaglia.fasl: \
		lib/vicare/crypto/randomisations/marsaglia.sls \
		lib/vicare/crypto/randomisations.fasl \
		lib/vicare/crypto/randomisations/low.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_marsaglia_fasldir = $(bundledlibsdir)/vicare/crypto/randomisations
lib_vicare_crypto_randomisations_marsaglia_slsdir  = $(bundledlibsdir)/vicare/crypto/randomisations
nodist_lib_vicare_crypto_randomisations_marsaglia_fasl_DATA = lib/vicare/crypto/randomisations/marsaglia.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_marsaglia_sls_DATA = lib/vicare/crypto/randomisations/marsaglia.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations/marsaglia.sls
CLEANFILES += lib/vicare/crypto/randomisations/marsaglia.fasl

lib/vicare/crypto/randomisations/mersenne.fasl: \
		lib/vicare/crypto/randomisations/mersenne.sls \
		lib/vicare/crypto/randomisations.fasl \
		lib/vicare/crypto/randomisations/low.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_mersenne_fasldir = $(bundledlibsdir)/vicare/crypto/randomisations
lib_vicare_crypto_randomisations_mersenne_slsdir  = $(bundledlibsdir)/vicare/crypto/randomisations
nodist_lib_vicare_crypto_randomisations_mersenne_fasl_DATA = lib/vicare/crypto/randomisations/mersenne.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_mersenne_sls_DATA = lib/vicare/crypto/randomisations/mersenne.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations/mersenne.sls
CLEANFILES += lib/vicare/crypto/randomisations/mersenne.fasl

lib/vicare/crypto/randomisations/strings.fasl: \
		lib/vicare/crypto/randomisations/strings.sls \
		lib/vicare/crypto/randomisations.fasl \
		lib/vicare/containers/strings/low.fasl \
		lib/vicare/containers/strings.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_strings_fasldir = $(bundledlibsdir)/vicare/crypto/randomisations
lib_vicare_crypto_randomisations_strings_slsdir  = $(bundledlibsdir)/vicare/crypto/randomisations
nodist_lib_vicare_crypto_randomisations_strings_fasl_DATA = lib/vicare/crypto/randomisations/strings.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_strings_sls_DATA = lib/vicare/crypto/randomisations/strings.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations/strings.sls
CLEANFILES += lib/vicare/crypto/randomisations/strings.fasl

lib/vicare/containers/strings/low.fasl: \
		lib/vicare/containers/strings/low.sls \
		lib/vicare/containers/char-sets.fasl \
		lib/vicare/containers/knuth-morris-pratt.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_strings_low_fasldir = $(bundledlibsdir)/vicare/containers/strings
lib_vicare_containers_strings_low_slsdir  = $(bundledlibsdir)/vicare/containers/strings
nodist_lib_vicare_containers_strings_low_fasl_DATA = lib/vicare/containers/strings/low.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_strings_low_sls_DATA = lib/vicare/containers/strings/low.sls
endif
EXTRA_DIST += lib/vicare/containers/strings/low.sls
CLEANFILES += lib/vicare/containers/strings/low.fasl

lib/vicare/containers/char-sets.fasl: \
		lib/vicare/containers/char-sets.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_char_sets_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_char_sets_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_char_sets_fasl_DATA = lib/vicare/containers/char-sets.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_char_sets_sls_DATA = lib/vicare/containers/char-sets.sls
endif
EXTRA_DIST += lib/vicare/containers/char-sets.sls
CLEANFILES += lib/vicare/containers/char-sets.fasl

lib/vicare/containers/knuth-morris-pratt.fasl: \
		lib/vicare/containers/knuth-morris-pratt.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_knuth_morris_pratt_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_knuth_morris_pratt_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_knuth_morris_pratt_fasl_DATA = lib/vicare/containers/knuth-morris-pratt.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_knuth_morris_pratt_sls_DATA = lib/vicare/containers/knuth-morris-pratt.sls
endif
EXTRA_DIST += lib/vicare/containers/knuth-morris-pratt.sls
CLEANFILES += lib/vicare/containers/knuth-morris-pratt.fasl

lib/vicare/containers/strings.fasl: \
		lib/vicare/containers/strings.sls \
		lib/vicare/containers/strings/low.fasl \
		lib/vicare/containers/auxiliary-syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_strings_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_strings_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_strings_fasl_DATA = lib/vicare/containers/strings.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_strings_sls_DATA = lib/vicare/containers/strings.sls
endif
EXTRA_DIST += lib/vicare/containers/strings.sls
CLEANFILES += lib/vicare/containers/strings.fasl

lib/vicare/containers/auxiliary-syntaxes.fasl: \
		lib/vicare/containers/auxiliary-syntaxes.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_auxiliary_syntaxes_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_auxiliary_syntaxes_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_auxiliary_syntaxes_fasl_DATA = lib/vicare/containers/auxiliary-syntaxes.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_auxiliary_syntaxes_sls_DATA = lib/vicare/containers/auxiliary-syntaxes.sls
endif
EXTRA_DIST += lib/vicare/containers/auxiliary-syntaxes.sls
CLEANFILES += lib/vicare/containers/auxiliary-syntaxes.fasl

lib/vicare/crypto/randomisations/vectors.fasl: \
		lib/vicare/crypto/randomisations/vectors.sls \
		lib/vicare/crypto/randomisations.fasl \
		lib/vicare/containers/vectors/low.fasl \
		lib/vicare/containers/vectors.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_randomisations_vectors_fasldir = $(bundledlibsdir)/vicare/crypto/randomisations
lib_vicare_crypto_randomisations_vectors_slsdir  = $(bundledlibsdir)/vicare/crypto/randomisations
nodist_lib_vicare_crypto_randomisations_vectors_fasl_DATA = lib/vicare/crypto/randomisations/vectors.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_randomisations_vectors_sls_DATA = lib/vicare/crypto/randomisations/vectors.sls
endif
EXTRA_DIST += lib/vicare/crypto/randomisations/vectors.sls
CLEANFILES += lib/vicare/crypto/randomisations/vectors.fasl

lib/vicare/containers/vectors/low.fasl: \
		lib/vicare/containers/vectors/low.sls \
		lib/vicare/containers/knuth-morris-pratt.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_vectors_low_fasldir = $(bundledlibsdir)/vicare/containers/vectors
lib_vicare_containers_vectors_low_slsdir  = $(bundledlibsdir)/vicare/containers/vectors
nodist_lib_vicare_containers_vectors_low_fasl_DATA = lib/vicare/containers/vectors/low.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_vectors_low_sls_DATA = lib/vicare/containers/vectors/low.sls
endif
EXTRA_DIST += lib/vicare/containers/vectors/low.sls
CLEANFILES += lib/vicare/containers/vectors/low.fasl

lib/vicare/containers/vectors.fasl: \
		lib/vicare/containers/vectors.sls \
		lib/vicare/containers/vectors/low.fasl \
		lib/vicare/containers/auxiliary-syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_vectors_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_vectors_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_vectors_fasl_DATA = lib/vicare/containers/vectors.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_vectors_sls_DATA = lib/vicare/containers/vectors.sls
endif
EXTRA_DIST += lib/vicare/containers/vectors.sls
CLEANFILES += lib/vicare/containers/vectors.fasl

lib/vicare/numerics/constants.fasl: \
		lib/vicare/numerics/constants.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_numerics_constants_fasldir = $(bundledlibsdir)/vicare/numerics
lib_vicare_numerics_constants_slsdir  = $(bundledlibsdir)/vicare/numerics
nodist_lib_vicare_numerics_constants_fasl_DATA = lib/vicare/numerics/constants.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_numerics_constants_sls_DATA = lib/vicare/numerics/constants.sls
endif
EXTRA_DIST += lib/vicare/numerics/constants.sls
CLEANFILES += lib/vicare/numerics/constants.fasl

lib/vicare/numerics/flonum-parser.fasl: \
		lib/vicare/numerics/flonum-parser.sls \
		lib/vicare/language-extensions/syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_numerics_flonum_parser_fasldir = $(bundledlibsdir)/vicare/numerics
lib_vicare_numerics_flonum_parser_slsdir  = $(bundledlibsdir)/vicare/numerics
nodist_lib_vicare_numerics_flonum_parser_fasl_DATA = lib/vicare/numerics/flonum-parser.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_numerics_flonum_parser_sls_DATA = lib/vicare/numerics/flonum-parser.sls
endif
EXTRA_DIST += lib/vicare/numerics/flonum-parser.sls
CLEANFILES += lib/vicare/numerics/flonum-parser.fasl

lib/vicare/numerics/flonum-formatter.fasl: \
		lib/vicare/numerics/flonum-formatter.sls \
		lib/vicare/language-extensions/syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_numerics_flonum_formatter_fasldir = $(bundledlibsdir)/vicare/numerics
lib_vicare_numerics_flonum_formatter_slsdir  = $(bundledlibsdir)/vicare/numerics
nodist_lib_vicare_numerics_flonum_formatter_fasl_DATA = lib/vicare/numerics/flonum-formatter.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_numerics_flonum_formatter_sls_DATA = lib/vicare/numerics/flonum-formatter.sls
endif
EXTRA_DIST += lib/vicare/numerics/flonum-formatter.sls
CLEANFILES += lib/vicare/numerics/flonum-formatter.fasl

lib/vicare/containers/bytevectors.fasl: \
		lib/vicare/containers/bytevectors.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_bytevectors_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_bytevectors_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_bytevectors_fasl_DATA = lib/vicare/containers/bytevectors.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_bytevectors_sls_DATA = lib/vicare/containers/bytevectors.sls
endif
EXTRA_DIST += lib/vicare/containers/bytevectors.sls
CLEANFILES += lib/vicare/containers/bytevectors.fasl

lib/vicare/containers/object-properties.fasl: \
		lib/vicare/containers/object-properties.sls \
		lib/vicare/containers/weak-hashtables.fasl \
		lib/vicare/language-extensions/sentinels.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_object_properties_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_object_properties_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_object_properties_fasl_DATA = lib/vicare/containers/object-properties.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_object_properties_sls_DATA = lib/vicare/containers/object-properties.sls
endif
EXTRA_DIST += lib/vicare/containers/object-properties.sls
CLEANFILES += lib/vicare/containers/object-properties.fasl

lib/vicare/containers/bytevector-compounds/core.fasl: \
		lib/vicare/containers/bytevector-compounds/core.sls \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_bytevector_compounds_core_fasldir = $(bundledlibsdir)/vicare/containers/bytevector-compounds
lib_vicare_containers_bytevector_compounds_core_slsdir  = $(bundledlibsdir)/vicare/containers/bytevector-compounds
nodist_lib_vicare_containers_bytevector_compounds_core_fasl_DATA = lib/vicare/containers/bytevector-compounds/core.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_bytevector_compounds_core_sls_DATA = lib/vicare/containers/bytevector-compounds/core.sls
endif
EXTRA_DIST += lib/vicare/containers/bytevector-compounds/core.sls
CLEANFILES += lib/vicare/containers/bytevector-compounds/core.fasl

lib/vicare/containers/bytevector-compounds.fasl: \
		lib/vicare/containers/bytevector-compounds.sls \
		lib/vicare/containers/bytevector-compounds/core.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_bytevector_compounds_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_bytevector_compounds_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_bytevector_compounds_fasl_DATA = lib/vicare/containers/bytevector-compounds.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_bytevector_compounds_sls_DATA = lib/vicare/containers/bytevector-compounds.sls
endif
EXTRA_DIST += lib/vicare/containers/bytevector-compounds.sls
CLEANFILES += lib/vicare/containers/bytevector-compounds.fasl

lib/vicare/containers/bytevector-compounds/unsafe.fasl: \
		lib/vicare/containers/bytevector-compounds/unsafe.sls \
		lib/vicare/containers/bytevector-compounds/core.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_bytevector_compounds_unsafe_fasldir = $(bundledlibsdir)/vicare/containers/bytevector-compounds
lib_vicare_containers_bytevector_compounds_unsafe_slsdir  = $(bundledlibsdir)/vicare/containers/bytevector-compounds
nodist_lib_vicare_containers_bytevector_compounds_unsafe_fasl_DATA = lib/vicare/containers/bytevector-compounds/unsafe.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_bytevector_compounds_unsafe_sls_DATA = lib/vicare/containers/bytevector-compounds/unsafe.sls
endif
EXTRA_DIST += lib/vicare/containers/bytevector-compounds/unsafe.sls
CLEANFILES += lib/vicare/containers/bytevector-compounds/unsafe.fasl

lib/vicare/containers/char-sets/blocks.fasl: \
		lib/vicare/containers/char-sets/blocks.sls \
		lib/vicare/containers/char-sets.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_char_sets_blocks_fasldir = $(bundledlibsdir)/vicare/containers/char-sets
lib_vicare_containers_char_sets_blocks_slsdir  = $(bundledlibsdir)/vicare/containers/char-sets
nodist_lib_vicare_containers_char_sets_blocks_fasl_DATA = lib/vicare/containers/char-sets/blocks.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_char_sets_blocks_sls_DATA = lib/vicare/containers/char-sets/blocks.sls
endif
EXTRA_DIST += lib/vicare/containers/char-sets/blocks.sls
CLEANFILES += lib/vicare/containers/char-sets/blocks.fasl

lib/vicare/containers/char-sets/categories.fasl: \
		lib/vicare/containers/char-sets/categories.sls \
		lib/vicare/containers/char-sets.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_char_sets_categories_fasldir = $(bundledlibsdir)/vicare/containers/char-sets
lib_vicare_containers_char_sets_categories_slsdir  = $(bundledlibsdir)/vicare/containers/char-sets
nodist_lib_vicare_containers_char_sets_categories_fasl_DATA = lib/vicare/containers/char-sets/categories.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_char_sets_categories_sls_DATA = lib/vicare/containers/char-sets/categories.sls
endif
EXTRA_DIST += lib/vicare/containers/char-sets/categories.sls
CLEANFILES += lib/vicare/containers/char-sets/categories.fasl

lib/vicare/containers/lists/stx.fasl: \
		lib/vicare/containers/lists/stx.sls \
		lib/vicare/containers/lists/low.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_lists_stx_fasldir = $(bundledlibsdir)/vicare/containers/lists
lib_vicare_containers_lists_stx_slsdir  = $(bundledlibsdir)/vicare/containers/lists
nodist_lib_vicare_containers_lists_stx_fasl_DATA = lib/vicare/containers/lists/stx.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_lists_stx_sls_DATA = lib/vicare/containers/lists/stx.sls
endif
EXTRA_DIST += lib/vicare/containers/lists/stx.sls
CLEANFILES += lib/vicare/containers/lists/stx.fasl

lib/vicare/containers/lists/low.fasl: \
		lib/vicare/containers/lists/low.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_lists_low_fasldir = $(bundledlibsdir)/vicare/containers/lists
lib_vicare_containers_lists_low_slsdir  = $(bundledlibsdir)/vicare/containers/lists
nodist_lib_vicare_containers_lists_low_fasl_DATA = lib/vicare/containers/lists/low.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_lists_low_sls_DATA = lib/vicare/containers/lists/low.sls
endif
EXTRA_DIST += lib/vicare/containers/lists/low.sls
CLEANFILES += lib/vicare/containers/lists/low.fasl

lib/vicare/containers/lists.fasl: \
		lib/vicare/containers/lists.sls \
		lib/vicare/containers/lists/stx.fasl \
		lib/vicare/containers/lists/low.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_lists_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_lists_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_lists_fasl_DATA = lib/vicare/containers/lists.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_lists_sls_DATA = lib/vicare/containers/lists.sls
endif
EXTRA_DIST += lib/vicare/containers/lists.sls
CLEANFILES += lib/vicare/containers/lists.fasl

lib/vicare/containers/strings/rabin-karp.fasl: \
		lib/vicare/containers/strings/rabin-karp.sls \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_strings_rabin_karp_fasldir = $(bundledlibsdir)/vicare/containers/strings
lib_vicare_containers_strings_rabin_karp_slsdir  = $(bundledlibsdir)/vicare/containers/strings
nodist_lib_vicare_containers_strings_rabin_karp_fasl_DATA = lib/vicare/containers/strings/rabin-karp.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_strings_rabin_karp_sls_DATA = lib/vicare/containers/strings/rabin-karp.sls
endif
EXTRA_DIST += lib/vicare/containers/strings/rabin-karp.sls
CLEANFILES += lib/vicare/containers/strings/rabin-karp.fasl

lib/vicare/containers/levenshtein.fasl: \
		lib/vicare/containers/levenshtein.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_levenshtein_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_levenshtein_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_levenshtein_fasl_DATA = lib/vicare/containers/levenshtein.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_levenshtein_sls_DATA = lib/vicare/containers/levenshtein.sls
endif
EXTRA_DIST += lib/vicare/containers/levenshtein.sls
CLEANFILES += lib/vicare/containers/levenshtein.fasl

lib/vicare/containers/one-dimension-co.fasl: \
		lib/vicare/containers/one-dimension-co.sls \
		lib/vicare/containers/lists.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_one_dimension_co_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_one_dimension_co_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_one_dimension_co_fasl_DATA = lib/vicare/containers/one-dimension-co.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_one_dimension_co_sls_DATA = lib/vicare/containers/one-dimension-co.sls
endif
EXTRA_DIST += lib/vicare/containers/one-dimension-co.sls
CLEANFILES += lib/vicare/containers/one-dimension-co.fasl

lib/vicare/containers/one-dimension-cc.fasl: \
		lib/vicare/containers/one-dimension-cc.sls \
		lib/vicare/containers/lists.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_one_dimension_cc_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_one_dimension_cc_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_one_dimension_cc_fasl_DATA = lib/vicare/containers/one-dimension-cc.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_one_dimension_cc_sls_DATA = lib/vicare/containers/one-dimension-cc.sls
endif
EXTRA_DIST += lib/vicare/containers/one-dimension-cc.sls
CLEANFILES += lib/vicare/containers/one-dimension-cc.fasl

lib/vicare/containers/bytevectors/u8.fasl: \
		lib/vicare/containers/bytevectors/u8.sls \
		lib/vicare/language-extensions/ascii-chars.fasl \
		lib/vicare/containers/bytevectors/u8low.fasl \
		lib/vicare/containers/bytevectors/generic.fasl \
		lib/vicare/containers/auxiliary-syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_bytevectors_u8_fasldir = $(bundledlibsdir)/vicare/containers/bytevectors
lib_vicare_containers_bytevectors_u8_slsdir  = $(bundledlibsdir)/vicare/containers/bytevectors
nodist_lib_vicare_containers_bytevectors_u8_fasl_DATA = lib/vicare/containers/bytevectors/u8.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_bytevectors_u8_sls_DATA = lib/vicare/containers/bytevectors/u8.sls
endif
EXTRA_DIST += lib/vicare/containers/bytevectors/u8.sls
CLEANFILES += lib/vicare/containers/bytevectors/u8.fasl

lib/vicare/containers/bytevectors/u8low.fasl: \
		lib/vicare/containers/bytevectors/u8low.sls \
		lib/vicare/containers/bytevectors/generic-low.fasl \
		lib/vicare/containers/char-sets.fasl \
		lib/vicare/containers/knuth-morris-pratt.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_bytevectors_u8low_fasldir = $(bundledlibsdir)/vicare/containers/bytevectors
lib_vicare_containers_bytevectors_u8low_slsdir  = $(bundledlibsdir)/vicare/containers/bytevectors
nodist_lib_vicare_containers_bytevectors_u8low_fasl_DATA = lib/vicare/containers/bytevectors/u8low.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_bytevectors_u8low_sls_DATA = lib/vicare/containers/bytevectors/u8low.sls
endif
EXTRA_DIST += lib/vicare/containers/bytevectors/u8low.sls
CLEANFILES += lib/vicare/containers/bytevectors/u8low.fasl

lib/vicare/containers/bytevectors/generic-low.fasl: \
		lib/vicare/containers/bytevectors/generic-low.sls \
		lib/vicare/containers/auxiliary-syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_bytevectors_generic_low_fasldir = $(bundledlibsdir)/vicare/containers/bytevectors
lib_vicare_containers_bytevectors_generic_low_slsdir  = $(bundledlibsdir)/vicare/containers/bytevectors
nodist_lib_vicare_containers_bytevectors_generic_low_fasl_DATA = lib/vicare/containers/bytevectors/generic-low.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_bytevectors_generic_low_sls_DATA = lib/vicare/containers/bytevectors/generic-low.sls
endif
EXTRA_DIST += lib/vicare/containers/bytevectors/generic-low.sls
CLEANFILES += lib/vicare/containers/bytevectors/generic-low.fasl

lib/vicare/containers/bytevectors/generic.fasl: \
		lib/vicare/containers/bytevectors/generic.sls \
		lib/vicare/containers/auxiliary-syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_bytevectors_generic_fasldir = $(bundledlibsdir)/vicare/containers/bytevectors
lib_vicare_containers_bytevectors_generic_slsdir  = $(bundledlibsdir)/vicare/containers/bytevectors
nodist_lib_vicare_containers_bytevectors_generic_fasl_DATA = lib/vicare/containers/bytevectors/generic.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_bytevectors_generic_sls_DATA = lib/vicare/containers/bytevectors/generic.sls
endif
EXTRA_DIST += lib/vicare/containers/bytevectors/generic.sls
CLEANFILES += lib/vicare/containers/bytevectors/generic.fasl

lib/vicare/containers/bytevectors/s8.fasl: \
		lib/vicare/containers/bytevectors/s8.sls \
		lib/vicare/language-extensions/ascii-chars.fasl \
		lib/vicare/containers/bytevectors/s8low.fasl \
		lib/vicare/containers/bytevectors/generic.fasl \
		lib/vicare/containers/auxiliary-syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_bytevectors_s8_fasldir = $(bundledlibsdir)/vicare/containers/bytevectors
lib_vicare_containers_bytevectors_s8_slsdir  = $(bundledlibsdir)/vicare/containers/bytevectors
nodist_lib_vicare_containers_bytevectors_s8_fasl_DATA = lib/vicare/containers/bytevectors/s8.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_bytevectors_s8_sls_DATA = lib/vicare/containers/bytevectors/s8.sls
endif
EXTRA_DIST += lib/vicare/containers/bytevectors/s8.sls
CLEANFILES += lib/vicare/containers/bytevectors/s8.fasl

lib/vicare/containers/bytevectors/s8low.fasl: \
		lib/vicare/containers/bytevectors/s8low.sls \
		lib/vicare/containers/bytevectors/generic-low.fasl \
		lib/vicare/containers/char-sets.fasl \
		lib/vicare/containers/knuth-morris-pratt.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_bytevectors_s8low_fasldir = $(bundledlibsdir)/vicare/containers/bytevectors
lib_vicare_containers_bytevectors_s8low_slsdir  = $(bundledlibsdir)/vicare/containers/bytevectors
nodist_lib_vicare_containers_bytevectors_s8low_fasl_DATA = lib/vicare/containers/bytevectors/s8low.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_bytevectors_s8low_sls_DATA = lib/vicare/containers/bytevectors/s8low.sls
endif
EXTRA_DIST += lib/vicare/containers/bytevectors/s8low.sls
CLEANFILES += lib/vicare/containers/bytevectors/s8low.fasl

lib/vicare/containers/arrays.fasl: \
		lib/vicare/containers/arrays.sls \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_arrays_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_arrays_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_arrays_fasl_DATA = lib/vicare/containers/arrays.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_arrays_sls_DATA = lib/vicare/containers/arrays.sls
endif
EXTRA_DIST += lib/vicare/containers/arrays.sls
CLEANFILES += lib/vicare/containers/arrays.fasl

lib/vicare/containers/stacks.fasl: \
		lib/vicare/containers/stacks.sls \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_stacks_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_stacks_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_stacks_fasl_DATA = lib/vicare/containers/stacks.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_stacks_sls_DATA = lib/vicare/containers/stacks.sls
endif
EXTRA_DIST += lib/vicare/containers/stacks.sls
CLEANFILES += lib/vicare/containers/stacks.fasl

lib/vicare/containers/queues.fasl: \
		lib/vicare/containers/queues.sls \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_containers_queues_fasldir = $(bundledlibsdir)/vicare/containers
lib_vicare_containers_queues_slsdir  = $(bundledlibsdir)/vicare/containers
nodist_lib_vicare_containers_queues_fasl_DATA = lib/vicare/containers/queues.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_containers_queues_sls_DATA = lib/vicare/containers/queues.sls
endif
EXTRA_DIST += lib/vicare/containers/queues.sls
CLEANFILES += lib/vicare/containers/queues.fasl

lib/vicare/parser-tools/silex/lexer.fasl: \
		lib/vicare/parser-tools/silex/lexer.sls \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/vicare/parser-tools/silex/code-lexer-driver.fasl \
		lib/vicare/parser-tools/silex/tree-lexer-driver.fasl \
		lib/vicare/parser-tools/silex/char-lexer-driver.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_lexer_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_lexer_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_lexer_fasl_DATA = lib/vicare/parser-tools/silex/lexer.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_lexer_sls_DATA = lib/vicare/parser-tools/silex/lexer.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/lexer.sls
CLEANFILES += lib/vicare/parser-tools/silex/lexer.fasl

lib/vicare/parser-tools/silex/input-system.fasl: \
		lib/vicare/parser-tools/silex/input-system.sls \
		lib/vicare/language-extensions/makers.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_input_system_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_input_system_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_input_system_fasl_DATA = lib/vicare/parser-tools/silex/input-system.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_input_system_sls_DATA = lib/vicare/parser-tools/silex/input-system.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/input-system.sls
CLEANFILES += lib/vicare/parser-tools/silex/input-system.fasl

lib/vicare/parser-tools/silex/code-lexer-driver.fasl: \
		lib/vicare/parser-tools/silex/code-lexer-driver.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_code_lexer_driver_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_code_lexer_driver_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_code_lexer_driver_fasl_DATA = lib/vicare/parser-tools/silex/code-lexer-driver.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_code_lexer_driver_sls_DATA = lib/vicare/parser-tools/silex/code-lexer-driver.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/code-lexer-driver.sls
CLEANFILES += lib/vicare/parser-tools/silex/code-lexer-driver.fasl

lib/vicare/parser-tools/silex/tree-lexer-driver.fasl: \
		lib/vicare/parser-tools/silex/tree-lexer-driver.sls \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_tree_lexer_driver_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_tree_lexer_driver_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_tree_lexer_driver_fasl_DATA = lib/vicare/parser-tools/silex/tree-lexer-driver.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_tree_lexer_driver_sls_DATA = lib/vicare/parser-tools/silex/tree-lexer-driver.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/tree-lexer-driver.sls
CLEANFILES += lib/vicare/parser-tools/silex/tree-lexer-driver.fasl

lib/vicare/parser-tools/silex/char-lexer-driver.fasl: \
		lib/vicare/parser-tools/silex/char-lexer-driver.sls \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/vicare/parser-tools/silex/tree-lexer-driver.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_char_lexer_driver_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_char_lexer_driver_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_char_lexer_driver_fasl_DATA = lib/vicare/parser-tools/silex/char-lexer-driver.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_char_lexer_driver_sls_DATA = lib/vicare/parser-tools/silex/char-lexer-driver.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/char-lexer-driver.sls
CLEANFILES += lib/vicare/parser-tools/silex/char-lexer-driver.fasl

lib/vicare/parser-tools/silex.fasl: \
		lib/vicare/parser-tools/silex.sls \
		lib/vicare/parser-tools/silex/lexer.fasl \
		lib/vicare/parser-tools/silex/semantic.fasl \
		lib/vicare/parser-tools/silex/action-l.fasl \
		lib/vicare/parser-tools/silex/class-l.fasl \
		lib/vicare/parser-tools/silex/macro-l.fasl \
		lib/vicare/parser-tools/silex/regexp-l.fasl \
		lib/vicare/parser-tools/silex/string-l.fasl \
		lib/vicare/parser-tools/silex/nested-comment-l.fasl \
		lib/vicare/language-extensions/makers.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_fasldir = $(bundledlibsdir)/vicare/parser-tools
lib_vicare_parser_tools_silex_slsdir  = $(bundledlibsdir)/vicare/parser-tools
nodist_lib_vicare_parser_tools_silex_fasl_DATA = lib/vicare/parser-tools/silex.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_sls_DATA = lib/vicare/parser-tools/silex.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex.sls
CLEANFILES += lib/vicare/parser-tools/silex.fasl

lib/vicare/parser-tools/silex/semantic.fasl: \
		lib/vicare/parser-tools/silex/semantic.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_semantic_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_semantic_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_semantic_fasl_DATA = lib/vicare/parser-tools/silex/semantic.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_semantic_sls_DATA = lib/vicare/parser-tools/silex/semantic.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/semantic.sls
CLEANFILES += lib/vicare/parser-tools/silex/semantic.fasl

lib/vicare/parser-tools/silex/action-l.fasl: \
		lib/vicare/parser-tools/silex/action-l.sls \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/vicare/parser-tools/silex/semantic.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_action_l_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_action_l_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_action_l_fasl_DATA = lib/vicare/parser-tools/silex/action-l.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_action_l_sls_DATA = lib/vicare/parser-tools/silex/action-l.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/action-l.sls
CLEANFILES += lib/vicare/parser-tools/silex/action-l.fasl

lib/vicare/parser-tools/silex/class-l.fasl: \
		lib/vicare/parser-tools/silex/class-l.sls \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/vicare/parser-tools/silex/semantic.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_class_l_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_class_l_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_class_l_fasl_DATA = lib/vicare/parser-tools/silex/class-l.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_class_l_sls_DATA = lib/vicare/parser-tools/silex/class-l.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/class-l.sls
CLEANFILES += lib/vicare/parser-tools/silex/class-l.fasl

lib/vicare/parser-tools/silex/macro-l.fasl: \
		lib/vicare/parser-tools/silex/macro-l.sls \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/vicare/parser-tools/silex/semantic.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_macro_l_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_macro_l_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_macro_l_fasl_DATA = lib/vicare/parser-tools/silex/macro-l.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_macro_l_sls_DATA = lib/vicare/parser-tools/silex/macro-l.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/macro-l.sls
CLEANFILES += lib/vicare/parser-tools/silex/macro-l.fasl

lib/vicare/parser-tools/silex/regexp-l.fasl: \
		lib/vicare/parser-tools/silex/regexp-l.sls \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/vicare/parser-tools/silex/semantic.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_regexp_l_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_regexp_l_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_regexp_l_fasl_DATA = lib/vicare/parser-tools/silex/regexp-l.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_regexp_l_sls_DATA = lib/vicare/parser-tools/silex/regexp-l.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/regexp-l.sls
CLEANFILES += lib/vicare/parser-tools/silex/regexp-l.fasl

lib/vicare/parser-tools/silex/string-l.fasl: \
		lib/vicare/parser-tools/silex/string-l.sls \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/vicare/parser-tools/silex/semantic.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_string_l_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_string_l_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_string_l_fasl_DATA = lib/vicare/parser-tools/silex/string-l.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_string_l_sls_DATA = lib/vicare/parser-tools/silex/string-l.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/string-l.sls
CLEANFILES += lib/vicare/parser-tools/silex/string-l.fasl

lib/vicare/parser-tools/silex/nested-comment-l.fasl: \
		lib/vicare/parser-tools/silex/nested-comment-l.sls \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/vicare/parser-tools/silex/semantic.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_nested_comment_l_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_nested_comment_l_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_nested_comment_l_fasl_DATA = lib/vicare/parser-tools/silex/nested-comment-l.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_nested_comment_l_sls_DATA = lib/vicare/parser-tools/silex/nested-comment-l.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/nested-comment-l.sls
CLEANFILES += lib/vicare/parser-tools/silex/nested-comment-l.fasl

lib/vicare/parser-tools/silex/utilities.fasl: \
		lib/vicare/parser-tools/silex/utilities.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_silex_utilities_fasldir = $(bundledlibsdir)/vicare/parser-tools/silex
lib_vicare_parser_tools_silex_utilities_slsdir  = $(bundledlibsdir)/vicare/parser-tools/silex
nodist_lib_vicare_parser_tools_silex_utilities_fasl_DATA = lib/vicare/parser-tools/silex/utilities.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_silex_utilities_sls_DATA = lib/vicare/parser-tools/silex/utilities.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/silex/utilities.sls
CLEANFILES += lib/vicare/parser-tools/silex/utilities.fasl

lib/vicare/parser-tools/unix-pathnames.fasl: \
		lib/vicare/parser-tools/unix-pathnames.sls \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_tools_unix_pathnames_fasldir = $(bundledlibsdir)/vicare/parser-tools
lib_vicare_parser_tools_unix_pathnames_slsdir  = $(bundledlibsdir)/vicare/parser-tools
nodist_lib_vicare_parser_tools_unix_pathnames_fasl_DATA = lib/vicare/parser-tools/unix-pathnames.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_tools_unix_pathnames_sls_DATA = lib/vicare/parser-tools/unix-pathnames.sls
endif
EXTRA_DIST += lib/vicare/parser-tools/unix-pathnames.sls
CLEANFILES += lib/vicare/parser-tools/unix-pathnames.fasl

lib/vicare/net/channels.fasl: \
		lib/vicare/net/channels.sls \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/language-extensions/syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_net_channels_fasldir = $(bundledlibsdir)/vicare/net
lib_vicare_net_channels_slsdir  = $(bundledlibsdir)/vicare/net
nodist_lib_vicare_net_channels_fasl_DATA = lib/vicare/net/channels.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_net_channels_sls_DATA = lib/vicare/net/channels.sls
endif
EXTRA_DIST += lib/vicare/net/channels.sls
CLEANFILES += lib/vicare/net/channels.fasl

lib/vicare/ffi.fasl: \
		lib/vicare/ffi.sls \
		lib/vicare/platform/errno.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_LIBFFI
lib_vicare_ffi_fasldir = $(bundledlibsdir)/vicare
lib_vicare_ffi_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_ffi_fasl_DATA = lib/vicare/ffi.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_ffi_sls_DATA = lib/vicare/ffi.sls
endif
EXTRA_DIST += lib/vicare/ffi.sls
CLEANFILES += lib/vicare/ffi.fasl
endif

lib/vicare/ffi/foreign-pointer-wrapper.fasl: \
		lib/vicare/ffi/foreign-pointer-wrapper.sls \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_LIBFFI
lib_vicare_ffi_foreign_pointer_wrapper_fasldir = $(bundledlibsdir)/vicare/ffi
lib_vicare_ffi_foreign_pointer_wrapper_slsdir  = $(bundledlibsdir)/vicare/ffi
nodist_lib_vicare_ffi_foreign_pointer_wrapper_fasl_DATA = lib/vicare/ffi/foreign-pointer-wrapper.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_ffi_foreign_pointer_wrapper_sls_DATA = lib/vicare/ffi/foreign-pointer-wrapper.sls
endif
EXTRA_DIST += lib/vicare/ffi/foreign-pointer-wrapper.sls
CLEANFILES += lib/vicare/ffi/foreign-pointer-wrapper.fasl
endif

lib/vicare/iconv.fasl: \
		lib/vicare/iconv.sls \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/platform/constants.fasl \
		lib/vicare/unsafe/capi.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_LIBICONV
lib_vicare_iconv_fasldir = $(bundledlibsdir)/vicare
lib_vicare_iconv_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_iconv_fasl_DATA = lib/vicare/iconv.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_iconv_sls_DATA = lib/vicare/iconv.sls
endif
EXTRA_DIST += lib/vicare/iconv.sls
CLEANFILES += lib/vicare/iconv.fasl
endif

lib/vicare/posix/pid-files.fasl: \
		lib/vicare/posix/pid-files.sls \
		lib/vicare/posix.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_pid_files_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_pid_files_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_pid_files_fasl_DATA = lib/vicare/posix/pid-files.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_pid_files_sls_DATA = lib/vicare/posix/pid-files.sls
endif
EXTRA_DIST += lib/vicare/posix/pid-files.sls
CLEANFILES += lib/vicare/posix/pid-files.fasl
endif

lib/vicare/posix/lock-pid-files.fasl: \
		lib/vicare/posix/lock-pid-files.sls \
		lib/vicare/posix.fasl \
		lib/vicare/platform/constants.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_lock_pid_files_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_lock_pid_files_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_lock_pid_files_fasl_DATA = lib/vicare/posix/lock-pid-files.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_lock_pid_files_sls_DATA = lib/vicare/posix/lock-pid-files.sls
endif
EXTRA_DIST += lib/vicare/posix/lock-pid-files.sls
CLEANFILES += lib/vicare/posix/lock-pid-files.fasl
endif

lib/vicare/posix/log-files.fasl: \
		lib/vicare/posix/log-files.sls \
		lib/vicare/posix.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_log_files_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_log_files_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_log_files_fasl_DATA = lib/vicare/posix/log-files.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_log_files_sls_DATA = lib/vicare/posix/log-files.sls
endif
EXTRA_DIST += lib/vicare/posix/log-files.sls
CLEANFILES += lib/vicare/posix/log-files.fasl
endif

lib/vicare/posix/daemonisations.fasl: \
		lib/vicare/posix/daemonisations.sls \
		lib/vicare/posix.fasl \
		lib/vicare/platform/constants.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_daemonisations_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_daemonisations_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_daemonisations_fasl_DATA = lib/vicare/posix/daemonisations.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_daemonisations_sls_DATA = lib/vicare/posix/daemonisations.sls
endif
EXTRA_DIST += lib/vicare/posix/daemonisations.sls
CLEANFILES += lib/vicare/posix/daemonisations.fasl
endif

lib/vicare/posix/simple-event-loop.fasl: \
		lib/vicare/posix/simple-event-loop.sls \
		lib/vicare/posix.fasl \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/platform/constants.fasl \
		lib/vicare/platform/utilities.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_simple_event_loop_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_simple_event_loop_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_simple_event_loop_fasl_DATA = lib/vicare/posix/simple-event-loop.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_simple_event_loop_sls_DATA = lib/vicare/posix/simple-event-loop.sls
endif
EXTRA_DIST += lib/vicare/posix/simple-event-loop.sls
CLEANFILES += lib/vicare/posix/simple-event-loop.fasl
endif

lib/vicare/posix/tcp-server-sockets.fasl: \
		lib/vicare/posix/tcp-server-sockets.sls \
		lib/vicare/posix.fasl \
		lib/vicare/platform/constants.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_tcp_server_sockets_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_tcp_server_sockets_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_tcp_server_sockets_fasl_DATA = lib/vicare/posix/tcp-server-sockets.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_tcp_server_sockets_sls_DATA = lib/vicare/posix/tcp-server-sockets.sls
endif
EXTRA_DIST += lib/vicare/posix/tcp-server-sockets.sls
CLEANFILES += lib/vicare/posix/tcp-server-sockets.fasl
endif

lib/vicare/posix/sendmail.fasl: \
		lib/vicare/posix/sendmail.sls \
		lib/vicare/posix.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_sendmail_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_sendmail_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_sendmail_fasl_DATA = lib/vicare/posix/sendmail.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_sendmail_sls_DATA = lib/vicare/posix/sendmail.sls
endif
EXTRA_DIST += lib/vicare/posix/sendmail.sls
CLEANFILES += lib/vicare/posix/sendmail.fasl
endif

lib/vicare/posix/mailx.fasl: \
		lib/vicare/posix/mailx.sls \
		lib/vicare/posix.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_mailx_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_mailx_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_mailx_fasl_DATA = lib/vicare/posix/mailx.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_mailx_sls_DATA = lib/vicare/posix/mailx.sls
endif
EXTRA_DIST += lib/vicare/posix/mailx.sls
CLEANFILES += lib/vicare/posix/mailx.fasl
endif

lib/vicare/posix/curl.fasl: \
		lib/vicare/posix/curl.sls \
		lib/vicare/posix.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_curl_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_curl_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_curl_fasl_DATA = lib/vicare/posix/curl.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_curl_sls_DATA = lib/vicare/posix/curl.sls
endif
EXTRA_DIST += lib/vicare/posix/curl.sls
CLEANFILES += lib/vicare/posix/curl.fasl
endif

lib/vicare/posix/wget.fasl: \
		lib/vicare/posix/wget.sls \
		lib/vicare/posix.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_wget_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_wget_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_wget_fasl_DATA = lib/vicare/posix/wget.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_wget_sls_DATA = lib/vicare/posix/wget.sls
endif
EXTRA_DIST += lib/vicare/posix/wget.sls
CLEANFILES += lib/vicare/posix/wget.fasl
endif

lib/vicare/posix/find.fasl: \
		lib/vicare/posix/find.sls \
		lib/vicare/posix.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
lib_vicare_posix_find_fasldir = $(bundledlibsdir)/vicare/posix
lib_vicare_posix_find_slsdir  = $(bundledlibsdir)/vicare/posix
nodist_lib_vicare_posix_find_fasl_DATA = lib/vicare/posix/find.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_posix_find_sls_DATA = lib/vicare/posix/find.sls
endif
EXTRA_DIST += lib/vicare/posix/find.sls
CLEANFILES += lib/vicare/posix/find.fasl
endif

lib/vicare/glibc.fasl: \
		lib/vicare/glibc.sls \
		lib/vicare/posix.fasl \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/platform/constants.fasl \
		lib/vicare/unsafe/capi.fasl \
		lib/vicare/platform/words.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/language-extensions/cond-expand.fasl \
		lib/vicare/platform/features.fasl \
		lib/vicare/language-extensions/cond-expand/helpers.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_GLIBC
lib_vicare_glibc_fasldir = $(bundledlibsdir)/vicare
lib_vicare_glibc_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_glibc_fasl_DATA = lib/vicare/glibc.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_glibc_sls_DATA = lib/vicare/glibc.sls
endif
EXTRA_DIST += lib/vicare/glibc.sls
CLEANFILES += lib/vicare/glibc.fasl
endif

lib/vicare/gcc.fasl: \
		lib/vicare/gcc.sls \
		lib/vicare/ffi.fasl \
		lib/vicare/posix.fasl \
		lib/vicare/glibc.fasl \
		lib/vicare/platform/constants.fasl \
		lib/vicare/language-extensions/syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_LIBFFI
if WANT_POSIX
if WANT_GLIBC
lib_vicare_gcc_fasldir = $(bundledlibsdir)/vicare
lib_vicare_gcc_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_gcc_fasl_DATA = lib/vicare/gcc.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_gcc_sls_DATA = lib/vicare/gcc.sls
endif
EXTRA_DIST += lib/vicare/gcc.sls
CLEANFILES += lib/vicare/gcc.fasl
endif
endif
endif

lib/vicare/linux.fasl: \
		lib/vicare/linux.sls \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/platform/constants.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/arguments/general-c-buffers.fasl \
		lib/vicare/platform/words.fasl \
		lib/vicare/posix.fasl \
		lib/vicare/unsafe/capi.fasl \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/language-extensions/cond-expand.fasl \
		lib/vicare/platform/features.fasl \
		lib/vicare/language-extensions/cond-expand/helpers.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_POSIX
if WANT_LINUX
lib_vicare_linux_fasldir = $(bundledlibsdir)/vicare
lib_vicare_linux_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_linux_fasl_DATA = lib/vicare/linux.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_linux_sls_DATA = lib/vicare/linux.sls
endif
EXTRA_DIST += lib/vicare/linux.sls
CLEANFILES += lib/vicare/linux.fasl
endif
endif

lib/vicare/readline.fasl: \
		lib/vicare/readline.sls \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_READLINE
lib_vicare_readline_fasldir = $(bundledlibsdir)/vicare
lib_vicare_readline_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_readline_fasl_DATA = lib/vicare/readline.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_readline_sls_DATA = lib/vicare/readline.sls
endif
EXTRA_DIST += lib/vicare/readline.sls
CLEANFILES += lib/vicare/readline.fasl
endif

lib/vicare/assembler/inspection.fasl: \
		lib/vicare/assembler/inspection.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_assembler_inspection_fasldir = $(bundledlibsdir)/vicare/assembler
lib_vicare_assembler_inspection_slsdir  = $(bundledlibsdir)/vicare/assembler
nodist_lib_vicare_assembler_inspection_fasl_DATA = lib/vicare/assembler/inspection.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_assembler_inspection_sls_DATA = lib/vicare/assembler/inspection.sls
endif
EXTRA_DIST += lib/vicare/assembler/inspection.sls
CLEANFILES += lib/vicare/assembler/inspection.fasl

lib/vicare/debugging/compiler.fasl: \
		lib/vicare/debugging/compiler.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_debugging_compiler_fasldir = $(bundledlibsdir)/vicare/debugging
lib_vicare_debugging_compiler_slsdir  = $(bundledlibsdir)/vicare/debugging
nodist_lib_vicare_debugging_compiler_fasl_DATA = lib/vicare/debugging/compiler.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_debugging_compiler_sls_DATA = lib/vicare/debugging/compiler.sls
endif
EXTRA_DIST += lib/vicare/debugging/compiler.sls
CLEANFILES += lib/vicare/debugging/compiler.fasl

lib/vicare/parser-logic.fasl: \
		lib/vicare/parser-logic.sls \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_parser_logic_fasldir = $(bundledlibsdir)/vicare
lib_vicare_parser_logic_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_parser_logic_fasl_DATA = lib/vicare/parser-logic.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_parser_logic_sls_DATA = lib/vicare/parser-logic.sls
endif
EXTRA_DIST += lib/vicare/parser-logic.sls
CLEANFILES += lib/vicare/parser-logic.fasl

lib/vicare/irregex.fasl: \
		lib/vicare/irregex.sls \
		lib/vicare/irregex/compat.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_irregex_fasldir = $(bundledlibsdir)/vicare
lib_vicare_irregex_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_irregex_fasl_DATA = lib/vicare/irregex.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_irregex_sls_DATA = lib/vicare/irregex.sls
endif
EXTRA_DIST += lib/vicare/irregex.sls
CLEANFILES += lib/vicare/irregex.fasl

lib/vicare/irregex/compat.fasl: \
		lib/vicare/irregex/compat.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_irregex_compat_fasldir = $(bundledlibsdir)/vicare/irregex
lib_vicare_irregex_compat_vicare_slsdir  = $(bundledlibsdir)/vicare/irregex
nodist_lib_vicare_irregex_compat_fasl_DATA = lib/vicare/irregex/compat.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_irregex_compat_vicare_sls_DATA = lib/vicare/irregex/compat.vicare.sls
endif
EXTRA_DIST += lib/vicare/irregex/compat.vicare.sls
CLEANFILES += lib/vicare/irregex/compat.fasl

lib/vicare/pregexp.fasl: \
		lib/vicare/pregexp.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_pregexp_fasldir = $(bundledlibsdir)/vicare
lib_vicare_pregexp_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_pregexp_fasl_DATA = lib/vicare/pregexp.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_pregexp_sls_DATA = lib/vicare/pregexp.sls
endif
EXTRA_DIST += lib/vicare/pregexp.sls
CLEANFILES += lib/vicare/pregexp.fasl

lib/vicare/getopts.fasl: \
		lib/vicare/getopts.sls \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/language-extensions/syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_getopts_fasldir = $(bundledlibsdir)/vicare
lib_vicare_getopts_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_getopts_fasl_DATA = lib/vicare/getopts.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_getopts_sls_DATA = lib/vicare/getopts.sls
endif
EXTRA_DIST += lib/vicare/getopts.sls
CLEANFILES += lib/vicare/getopts.fasl

lib/vicare/formations.fasl: \
		lib/vicare/formations.sls \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_formations_fasldir = $(bundledlibsdir)/vicare
lib_vicare_formations_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_formations_fasl_DATA = lib/vicare/formations.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_formations_sls_DATA = lib/vicare/formations.sls
endif
EXTRA_DIST += lib/vicare/formations.sls
CLEANFILES += lib/vicare/formations.fasl

lib/vicare/cre2.fasl: \
		lib/vicare/cre2.sls \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_CRE2
lib_vicare_cre2_fasldir = $(bundledlibsdir)/vicare
lib_vicare_cre2_slsdir  = $(bundledlibsdir)/vicare
nodist_lib_vicare_cre2_fasl_DATA = lib/vicare/cre2.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_cre2_sls_DATA = lib/vicare/cre2.sls
endif
EXTRA_DIST += lib/vicare/cre2.sls
CLEANFILES += lib/vicare/cre2.fasl
endif

lib/srfi/%3a0.fasl: \
		lib/srfi/%3a0.sls \
		lib/srfi/%3a0/cond-expand.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a0_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a0_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a0_fasl_DATA = lib/srfi/%3a0.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a0_sls_DATA = lib/srfi/%3a0.sls
endif
EXTRA_DIST += lib/srfi/%3a0.sls
CLEANFILES += lib/srfi/%3a0.fasl
endif

lib/srfi/%3a0/cond-expand.fasl: \
		lib/srfi/%3a0/cond-expand.sls \
		lib/vicare/language-extensions/cond-expand/registry.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a0_cond_expand_fasldir = $(bundledlibsdir)/srfi/%3a0
lib_srfi__3a0_cond_expand_slsdir  = $(bundledlibsdir)/srfi/%3a0
nodist_lib_srfi__3a0_cond_expand_fasl_DATA = lib/srfi/%3a0/cond-expand.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a0_cond_expand_sls_DATA = lib/srfi/%3a0/cond-expand.sls
endif
EXTRA_DIST += lib/srfi/%3a0/cond-expand.sls
CLEANFILES += lib/srfi/%3a0/cond-expand.fasl
endif

lib/srfi/%3a1.fasl: \
		lib/srfi/%3a1.sls \
		lib/srfi/%3a1/lists.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a1_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a1_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a1_fasl_DATA = lib/srfi/%3a1.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a1_sls_DATA = lib/srfi/%3a1.sls
endif
EXTRA_DIST += lib/srfi/%3a1.sls
CLEANFILES += lib/srfi/%3a1.fasl
endif

lib/srfi/%3a1/lists.fasl: \
		lib/srfi/%3a1/lists.sls \
		lib/srfi/%3a8/receive.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a1_lists_fasldir = $(bundledlibsdir)/srfi/%3a1
lib_srfi__3a1_lists_slsdir  = $(bundledlibsdir)/srfi/%3a1
nodist_lib_srfi__3a1_lists_fasl_DATA = lib/srfi/%3a1/lists.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a1_lists_sls_DATA = lib/srfi/%3a1/lists.sls
endif
EXTRA_DIST += lib/srfi/%3a1/lists.sls
CLEANFILES += lib/srfi/%3a1/lists.fasl
endif

lib/srfi/%3a8/receive.fasl: \
		lib/srfi/%3a8/receive.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a8_receive_fasldir = $(bundledlibsdir)/srfi/%3a8
lib_srfi__3a8_receive_slsdir  = $(bundledlibsdir)/srfi/%3a8
nodist_lib_srfi__3a8_receive_fasl_DATA = lib/srfi/%3a8/receive.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a8_receive_sls_DATA = lib/srfi/%3a8/receive.sls
endif
EXTRA_DIST += lib/srfi/%3a8/receive.sls
CLEANFILES += lib/srfi/%3a8/receive.fasl
endif

lib/srfi/%3a2.fasl: \
		lib/srfi/%3a2.sls \
		lib/srfi/%3a2/and-let%2a.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a2_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a2_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a2_fasl_DATA = lib/srfi/%3a2.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a2_sls_DATA = lib/srfi/%3a2.sls
endif
EXTRA_DIST += lib/srfi/%3a2.sls
CLEANFILES += lib/srfi/%3a2.fasl
endif

lib/srfi/%3a2/and-let%2a.fasl: \
		lib/srfi/%3a2/and-let%2a.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a2_and_let_2a_fasldir = $(bundledlibsdir)/srfi/%3a2
lib_srfi__3a2_and_let_2a_slsdir  = $(bundledlibsdir)/srfi/%3a2
nodist_lib_srfi__3a2_and_let_2a_fasl_DATA = lib/srfi/%3a2/and-let%2a.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a2_and_let_2a_sls_DATA = lib/srfi/%3a2/and-let%2a.sls
endif
EXTRA_DIST += lib/srfi/%3a2/and-let%2a.sls
CLEANFILES += lib/srfi/%3a2/and-let%2a.fasl
endif

lib/srfi/%3a6.fasl: \
		lib/srfi/%3a6.sls \
		lib/srfi/%3a6/basic-string-ports.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a6_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a6_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a6_fasl_DATA = lib/srfi/%3a6.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a6_sls_DATA = lib/srfi/%3a6.sls
endif
EXTRA_DIST += lib/srfi/%3a6.sls
CLEANFILES += lib/srfi/%3a6.fasl
endif

lib/srfi/%3a6/basic-string-ports.fasl: \
		lib/srfi/%3a6/basic-string-ports.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a6_basic_string_ports_fasldir = $(bundledlibsdir)/srfi/%3a6
lib_srfi__3a6_basic_string_ports_slsdir  = $(bundledlibsdir)/srfi/%3a6
nodist_lib_srfi__3a6_basic_string_ports_fasl_DATA = lib/srfi/%3a6/basic-string-ports.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a6_basic_string_ports_sls_DATA = lib/srfi/%3a6/basic-string-ports.sls
endif
EXTRA_DIST += lib/srfi/%3a6/basic-string-ports.sls
CLEANFILES += lib/srfi/%3a6/basic-string-ports.fasl
endif

lib/srfi/%3a8.fasl: \
		lib/srfi/%3a8.sls \
		lib/srfi/%3a8/receive.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a8_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a8_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a8_fasl_DATA = lib/srfi/%3a8.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a8_sls_DATA = lib/srfi/%3a8.sls
endif
EXTRA_DIST += lib/srfi/%3a8.sls
CLEANFILES += lib/srfi/%3a8.fasl
endif

lib/srfi/%3a9.fasl: \
		lib/srfi/%3a9.sls \
		lib/srfi/%3a9/records.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a9_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a9_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a9_fasl_DATA = lib/srfi/%3a9.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a9_sls_DATA = lib/srfi/%3a9.sls
endif
EXTRA_DIST += lib/srfi/%3a9.sls
CLEANFILES += lib/srfi/%3a9.fasl
endif

lib/srfi/%3a9/records.fasl: \
		lib/srfi/%3a9/records.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a9_records_fasldir = $(bundledlibsdir)/srfi/%3a9
lib_srfi__3a9_records_slsdir  = $(bundledlibsdir)/srfi/%3a9
nodist_lib_srfi__3a9_records_fasl_DATA = lib/srfi/%3a9/records.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a9_records_sls_DATA = lib/srfi/%3a9/records.sls
endif
EXTRA_DIST += lib/srfi/%3a9/records.sls
CLEANFILES += lib/srfi/%3a9/records.fasl
endif

lib/srfi/%3a11.fasl: \
		lib/srfi/%3a11.sls \
		lib/srfi/%3a11/let-values.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a11_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a11_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a11_fasl_DATA = lib/srfi/%3a11.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a11_sls_DATA = lib/srfi/%3a11.sls
endif
EXTRA_DIST += lib/srfi/%3a11.sls
CLEANFILES += lib/srfi/%3a11.fasl
endif

lib/srfi/%3a11/let-values.fasl: \
		lib/srfi/%3a11/let-values.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a11_let_values_fasldir = $(bundledlibsdir)/srfi/%3a11
lib_srfi__3a11_let_values_slsdir  = $(bundledlibsdir)/srfi/%3a11
nodist_lib_srfi__3a11_let_values_fasl_DATA = lib/srfi/%3a11/let-values.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a11_let_values_sls_DATA = lib/srfi/%3a11/let-values.sls
endif
EXTRA_DIST += lib/srfi/%3a11/let-values.sls
CLEANFILES += lib/srfi/%3a11/let-values.fasl
endif

lib/srfi/%3a13.fasl: \
		lib/srfi/%3a13.sls \
		lib/srfi/%3a13/strings.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a13_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a13_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a13_fasl_DATA = lib/srfi/%3a13.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a13_sls_DATA = lib/srfi/%3a13.sls
endif
EXTRA_DIST += lib/srfi/%3a13.sls
CLEANFILES += lib/srfi/%3a13.fasl
endif

lib/srfi/%3a13/strings.fasl: \
		lib/srfi/%3a13/strings.sls \
		lib/srfi/%3a14/char-sets.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a13_strings_fasldir = $(bundledlibsdir)/srfi/%3a13
lib_srfi__3a13_strings_slsdir  = $(bundledlibsdir)/srfi/%3a13
nodist_lib_srfi__3a13_strings_fasl_DATA = lib/srfi/%3a13/strings.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a13_strings_sls_DATA = lib/srfi/%3a13/strings.sls
endif
EXTRA_DIST += lib/srfi/%3a13/strings.sls
CLEANFILES += lib/srfi/%3a13/strings.fasl
endif

lib/srfi/%3a14/char-sets.fasl: \
		lib/srfi/%3a14/char-sets.sls \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a14_char_sets_fasldir = $(bundledlibsdir)/srfi/%3a14
lib_srfi__3a14_char_sets_slsdir  = $(bundledlibsdir)/srfi/%3a14
nodist_lib_srfi__3a14_char_sets_fasl_DATA = lib/srfi/%3a14/char-sets.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a14_char_sets_sls_DATA = lib/srfi/%3a14/char-sets.sls
endif
EXTRA_DIST += lib/srfi/%3a14/char-sets.sls
CLEANFILES += lib/srfi/%3a14/char-sets.fasl
endif

lib/srfi/%3a14.fasl: \
		lib/srfi/%3a14.sls \
		lib/srfi/%3a14/char-sets.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a14_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a14_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a14_fasl_DATA = lib/srfi/%3a14.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a14_sls_DATA = lib/srfi/%3a14.sls
endif
EXTRA_DIST += lib/srfi/%3a14.sls
CLEANFILES += lib/srfi/%3a14.fasl
endif

lib/srfi/%3a16.fasl: \
		lib/srfi/%3a16.sls \
		lib/srfi/%3a16/case-lambda.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a16_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a16_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a16_fasl_DATA = lib/srfi/%3a16.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a16_sls_DATA = lib/srfi/%3a16.sls
endif
EXTRA_DIST += lib/srfi/%3a16.sls
CLEANFILES += lib/srfi/%3a16.fasl
endif

lib/srfi/%3a16/case-lambda.fasl: \
		lib/srfi/%3a16/case-lambda.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a16_case_lambda_fasldir = $(bundledlibsdir)/srfi/%3a16
lib_srfi__3a16_case_lambda_slsdir  = $(bundledlibsdir)/srfi/%3a16
nodist_lib_srfi__3a16_case_lambda_fasl_DATA = lib/srfi/%3a16/case-lambda.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a16_case_lambda_sls_DATA = lib/srfi/%3a16/case-lambda.sls
endif
EXTRA_DIST += lib/srfi/%3a16/case-lambda.sls
CLEANFILES += lib/srfi/%3a16/case-lambda.fasl
endif

lib/srfi/%3a19.fasl: \
		lib/srfi/%3a19.sls \
		lib/srfi/%3a19/time.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a19_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a19_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a19_fasl_DATA = lib/srfi/%3a19.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a19_sls_DATA = lib/srfi/%3a19.sls
endif
EXTRA_DIST += lib/srfi/%3a19.sls
CLEANFILES += lib/srfi/%3a19.fasl
endif

lib/srfi/%3a19/time.fasl: \
		lib/srfi/%3a19/time.sls \
		lib/srfi/%3a6/basic-string-ports.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a19_time_fasldir = $(bundledlibsdir)/srfi/%3a19
lib_srfi__3a19_time_slsdir  = $(bundledlibsdir)/srfi/%3a19
nodist_lib_srfi__3a19_time_fasl_DATA = lib/srfi/%3a19/time.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a19_time_sls_DATA = lib/srfi/%3a19/time.sls
endif
EXTRA_DIST += lib/srfi/%3a19/time.sls
CLEANFILES += lib/srfi/%3a19/time.fasl
endif

lib/srfi/%3a23.fasl: \
		lib/srfi/%3a23.sls \
		lib/srfi/%3a23/error.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a23_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a23_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a23_fasl_DATA = lib/srfi/%3a23.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a23_sls_DATA = lib/srfi/%3a23.sls
endif
EXTRA_DIST += lib/srfi/%3a23.sls
CLEANFILES += lib/srfi/%3a23.fasl
endif

lib/srfi/%3a23/error.fasl: \
		lib/srfi/%3a23/error.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a23_error_fasldir = $(bundledlibsdir)/srfi/%3a23
lib_srfi__3a23_error_slsdir  = $(bundledlibsdir)/srfi/%3a23
nodist_lib_srfi__3a23_error_fasl_DATA = lib/srfi/%3a23/error.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a23_error_sls_DATA = lib/srfi/%3a23/error.sls
endif
EXTRA_DIST += lib/srfi/%3a23/error.sls
CLEANFILES += lib/srfi/%3a23/error.fasl
endif

lib/srfi/%3a25.fasl: \
		lib/srfi/%3a25.sls \
		lib/srfi/%3a25/multi-dimensional-arrays.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a25_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a25_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a25_fasl_DATA = lib/srfi/%3a25.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a25_sls_DATA = lib/srfi/%3a25.sls
endif
EXTRA_DIST += lib/srfi/%3a25.sls
CLEANFILES += lib/srfi/%3a25.fasl
endif

lib/srfi/%3a25/multi-dimensional-arrays.fasl: \
		lib/srfi/%3a25/multi-dimensional-arrays.sls \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/language-extensions/syntaxes.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a25_multi_dimensional_arrays_fasldir = $(bundledlibsdir)/srfi/%3a25
lib_srfi__3a25_multi_dimensional_arrays_slsdir  = $(bundledlibsdir)/srfi/%3a25
nodist_lib_srfi__3a25_multi_dimensional_arrays_fasl_DATA = lib/srfi/%3a25/multi-dimensional-arrays.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a25_multi_dimensional_arrays_sls_DATA = lib/srfi/%3a25/multi-dimensional-arrays.sls
endif
EXTRA_DIST += lib/srfi/%3a25/multi-dimensional-arrays.sls
CLEANFILES += lib/srfi/%3a25/multi-dimensional-arrays.fasl
endif

lib/srfi/%3a26.fasl: \
		lib/srfi/%3a26.sls \
		lib/srfi/%3a26/cut.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a26_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a26_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a26_fasl_DATA = lib/srfi/%3a26.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a26_sls_DATA = lib/srfi/%3a26.sls
endif
EXTRA_DIST += lib/srfi/%3a26.sls
CLEANFILES += lib/srfi/%3a26.fasl
endif

lib/srfi/%3a26/cut.fasl: \
		lib/srfi/%3a26/cut.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a26_cut_fasldir = $(bundledlibsdir)/srfi/%3a26
lib_srfi__3a26_cut_slsdir  = $(bundledlibsdir)/srfi/%3a26
nodist_lib_srfi__3a26_cut_fasl_DATA = lib/srfi/%3a26/cut.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a26_cut_sls_DATA = lib/srfi/%3a26/cut.sls
endif
EXTRA_DIST += lib/srfi/%3a26/cut.sls
CLEANFILES += lib/srfi/%3a26/cut.fasl
endif

lib/srfi/%3a27.fasl: \
		lib/srfi/%3a27.sls \
		lib/srfi/%3a27/random-bits.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a27_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a27_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a27_fasl_DATA = lib/srfi/%3a27.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a27_sls_DATA = lib/srfi/%3a27.sls
endif
EXTRA_DIST += lib/srfi/%3a27.sls
CLEANFILES += lib/srfi/%3a27.fasl
endif

lib/srfi/%3a27/random-bits.fasl: \
		lib/srfi/%3a27/random-bits.sls \
		lib/srfi/%3a19/time.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a27_random_bits_fasldir = $(bundledlibsdir)/srfi/%3a27
lib_srfi__3a27_random_bits_slsdir  = $(bundledlibsdir)/srfi/%3a27
nodist_lib_srfi__3a27_random_bits_fasl_DATA = lib/srfi/%3a27/random-bits.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a27_random_bits_sls_DATA = lib/srfi/%3a27/random-bits.sls
endif
EXTRA_DIST += lib/srfi/%3a27/random-bits.sls
CLEANFILES += lib/srfi/%3a27/random-bits.fasl
endif

lib/srfi/%3a28.fasl: \
		lib/srfi/%3a28.sls \
		lib/srfi/%3a28/basic-format-strings.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a28_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a28_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a28_fasl_DATA = lib/srfi/%3a28.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a28_sls_DATA = lib/srfi/%3a28.sls
endif
EXTRA_DIST += lib/srfi/%3a28.sls
CLEANFILES += lib/srfi/%3a28.fasl
endif

lib/srfi/%3a28/basic-format-strings.fasl: \
		lib/srfi/%3a28/basic-format-strings.sls \
		lib/srfi/%3a6.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a28_basic_format_strings_fasldir = $(bundledlibsdir)/srfi/%3a28
lib_srfi__3a28_basic_format_strings_slsdir  = $(bundledlibsdir)/srfi/%3a28
nodist_lib_srfi__3a28_basic_format_strings_fasl_DATA = lib/srfi/%3a28/basic-format-strings.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a28_basic_format_strings_sls_DATA = lib/srfi/%3a28/basic-format-strings.sls
endif
EXTRA_DIST += lib/srfi/%3a28/basic-format-strings.sls
CLEANFILES += lib/srfi/%3a28/basic-format-strings.fasl
endif

lib/srfi/%3a31.fasl: \
		lib/srfi/%3a31.sls \
		lib/srfi/%3a31/rec.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a31_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a31_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a31_fasl_DATA = lib/srfi/%3a31.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a31_sls_DATA = lib/srfi/%3a31.sls
endif
EXTRA_DIST += lib/srfi/%3a31.sls
CLEANFILES += lib/srfi/%3a31.fasl
endif

lib/srfi/%3a31/rec.fasl: \
		lib/srfi/%3a31/rec.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a31_rec_fasldir = $(bundledlibsdir)/srfi/%3a31
lib_srfi__3a31_rec_slsdir  = $(bundledlibsdir)/srfi/%3a31
nodist_lib_srfi__3a31_rec_fasl_DATA = lib/srfi/%3a31/rec.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a31_rec_sls_DATA = lib/srfi/%3a31/rec.sls
endif
EXTRA_DIST += lib/srfi/%3a31/rec.sls
CLEANFILES += lib/srfi/%3a31/rec.fasl
endif

lib/srfi/%3a37.fasl: \
		lib/srfi/%3a37.sls \
		lib/srfi/%3a37/args-fold.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a37_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a37_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a37_fasl_DATA = lib/srfi/%3a37.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a37_sls_DATA = lib/srfi/%3a37.sls
endif
EXTRA_DIST += lib/srfi/%3a37.sls
CLEANFILES += lib/srfi/%3a37.fasl
endif

lib/srfi/%3a37/args-fold.fasl: \
		lib/srfi/%3a37/args-fold.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a37_args_fold_fasldir = $(bundledlibsdir)/srfi/%3a37
lib_srfi__3a37_args_fold_slsdir  = $(bundledlibsdir)/srfi/%3a37
nodist_lib_srfi__3a37_args_fold_fasl_DATA = lib/srfi/%3a37/args-fold.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a37_args_fold_sls_DATA = lib/srfi/%3a37/args-fold.sls
endif
EXTRA_DIST += lib/srfi/%3a37/args-fold.sls
CLEANFILES += lib/srfi/%3a37/args-fold.fasl
endif

lib/srfi/%3a38.fasl: \
		lib/srfi/%3a38.sls \
		lib/srfi/%3a38/with-shared-structure.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a38_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a38_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a38_fasl_DATA = lib/srfi/%3a38.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a38_sls_DATA = lib/srfi/%3a38.sls
endif
EXTRA_DIST += lib/srfi/%3a38.sls
CLEANFILES += lib/srfi/%3a38.fasl
endif

lib/srfi/%3a38/with-shared-structure.fasl: \
		lib/srfi/%3a38/with-shared-structure.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a38_with_shared_structure_fasldir = $(bundledlibsdir)/srfi/%3a38
lib_srfi__3a38_with_shared_structure_slsdir  = $(bundledlibsdir)/srfi/%3a38
nodist_lib_srfi__3a38_with_shared_structure_fasl_DATA = lib/srfi/%3a38/with-shared-structure.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a38_with_shared_structure_sls_DATA = lib/srfi/%3a38/with-shared-structure.sls
endif
EXTRA_DIST += lib/srfi/%3a38/with-shared-structure.sls
CLEANFILES += lib/srfi/%3a38/with-shared-structure.fasl
endif

lib/srfi/%3a39.fasl: \
		lib/srfi/%3a39.sls \
		lib/srfi/%3a39/parameters.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a39_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a39_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a39_fasl_DATA = lib/srfi/%3a39.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a39_sls_DATA = lib/srfi/%3a39.sls
endif
EXTRA_DIST += lib/srfi/%3a39.sls
CLEANFILES += lib/srfi/%3a39.fasl
endif

lib/srfi/%3a39/parameters.fasl: \
		lib/srfi/%3a39/parameters.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a39_parameters_fasldir = $(bundledlibsdir)/srfi/%3a39
lib_srfi__3a39_parameters_slsdir  = $(bundledlibsdir)/srfi/%3a39
nodist_lib_srfi__3a39_parameters_fasl_DATA = lib/srfi/%3a39/parameters.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a39_parameters_sls_DATA = lib/srfi/%3a39/parameters.sls
endif
EXTRA_DIST += lib/srfi/%3a39/parameters.sls
CLEANFILES += lib/srfi/%3a39/parameters.fasl
endif

lib/srfi/%3a41.fasl: \
		lib/srfi/%3a41.sls \
		lib/srfi/%3a41/streams.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a41_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a41_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a41_fasl_DATA = lib/srfi/%3a41.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a41_sls_DATA = lib/srfi/%3a41.sls
endif
EXTRA_DIST += lib/srfi/%3a41.sls
CLEANFILES += lib/srfi/%3a41.fasl
endif

lib/srfi/%3a41/streams.fasl: \
		lib/srfi/%3a41/streams.sls \
		lib/srfi/%3a41/streams/primitive.fasl \
		lib/srfi/%3a41/streams/derived.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a41_streams_fasldir = $(bundledlibsdir)/srfi/%3a41
lib_srfi__3a41_streams_slsdir  = $(bundledlibsdir)/srfi/%3a41
nodist_lib_srfi__3a41_streams_fasl_DATA = lib/srfi/%3a41/streams.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a41_streams_sls_DATA = lib/srfi/%3a41/streams.sls
endif
EXTRA_DIST += lib/srfi/%3a41/streams.sls
CLEANFILES += lib/srfi/%3a41/streams.fasl
endif

lib/srfi/%3a41/streams/primitive.fasl: \
		lib/srfi/%3a41/streams/primitive.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a41_streams_primitive_fasldir = $(bundledlibsdir)/srfi/%3a41/streams
lib_srfi__3a41_streams_primitive_slsdir  = $(bundledlibsdir)/srfi/%3a41/streams
nodist_lib_srfi__3a41_streams_primitive_fasl_DATA = lib/srfi/%3a41/streams/primitive.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a41_streams_primitive_sls_DATA = lib/srfi/%3a41/streams/primitive.sls
endif
EXTRA_DIST += lib/srfi/%3a41/streams/primitive.sls
CLEANFILES += lib/srfi/%3a41/streams/primitive.fasl
endif

lib/srfi/%3a41/streams/derived.fasl: \
		lib/srfi/%3a41/streams/derived.sls \
		lib/srfi/%3a41/streams/primitive.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a41_streams_derived_fasldir = $(bundledlibsdir)/srfi/%3a41/streams
lib_srfi__3a41_streams_derived_slsdir  = $(bundledlibsdir)/srfi/%3a41/streams
nodist_lib_srfi__3a41_streams_derived_fasl_DATA = lib/srfi/%3a41/streams/derived.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a41_streams_derived_sls_DATA = lib/srfi/%3a41/streams/derived.sls
endif
EXTRA_DIST += lib/srfi/%3a41/streams/derived.sls
CLEANFILES += lib/srfi/%3a41/streams/derived.fasl
endif

lib/srfi/%3a42.fasl: \
		lib/srfi/%3a42.sls \
		lib/srfi/%3a42/eager-comprehensions.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a42_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a42_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a42_fasl_DATA = lib/srfi/%3a42.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a42_sls_DATA = lib/srfi/%3a42.sls
endif
EXTRA_DIST += lib/srfi/%3a42.sls
CLEANFILES += lib/srfi/%3a42.fasl
endif

lib/srfi/%3a42/eager-comprehensions.fasl: \
		lib/srfi/%3a42/eager-comprehensions.sls \
		lib/srfi/%3a39/parameters.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a42_eager_comprehensions_fasldir = $(bundledlibsdir)/srfi/%3a42
lib_srfi__3a42_eager_comprehensions_slsdir  = $(bundledlibsdir)/srfi/%3a42
nodist_lib_srfi__3a42_eager_comprehensions_fasl_DATA = lib/srfi/%3a42/eager-comprehensions.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a42_eager_comprehensions_sls_DATA = lib/srfi/%3a42/eager-comprehensions.sls
endif
EXTRA_DIST += lib/srfi/%3a42/eager-comprehensions.sls
CLEANFILES += lib/srfi/%3a42/eager-comprehensions.fasl
endif

lib/srfi/%3a43.fasl: \
		lib/srfi/%3a43.sls \
		lib/srfi/%3a43/vectors.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a43_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a43_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a43_fasl_DATA = lib/srfi/%3a43.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a43_sls_DATA = lib/srfi/%3a43.sls
endif
EXTRA_DIST += lib/srfi/%3a43.sls
CLEANFILES += lib/srfi/%3a43.fasl
endif

lib/srfi/%3a43/vectors.fasl: \
		lib/srfi/%3a43/vectors.sls \
		lib/srfi/%3a8/receive.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a43_vectors_fasldir = $(bundledlibsdir)/srfi/%3a43
lib_srfi__3a43_vectors_slsdir  = $(bundledlibsdir)/srfi/%3a43
nodist_lib_srfi__3a43_vectors_fasl_DATA = lib/srfi/%3a43/vectors.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a43_vectors_sls_DATA = lib/srfi/%3a43/vectors.sls
endif
EXTRA_DIST += lib/srfi/%3a43/vectors.sls
CLEANFILES += lib/srfi/%3a43/vectors.fasl
endif

lib/srfi/%3a45.fasl: \
		lib/srfi/%3a45.sls \
		lib/srfi/%3a45/lazy.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a45_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a45_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a45_fasl_DATA = lib/srfi/%3a45.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a45_sls_DATA = lib/srfi/%3a45.sls
endif
EXTRA_DIST += lib/srfi/%3a45.sls
CLEANFILES += lib/srfi/%3a45.fasl
endif

lib/srfi/%3a45/lazy.fasl: \
		lib/srfi/%3a45/lazy.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a45_lazy_fasldir = $(bundledlibsdir)/srfi/%3a45
lib_srfi__3a45_lazy_slsdir  = $(bundledlibsdir)/srfi/%3a45
nodist_lib_srfi__3a45_lazy_fasl_DATA = lib/srfi/%3a45/lazy.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a45_lazy_sls_DATA = lib/srfi/%3a45/lazy.sls
endif
EXTRA_DIST += lib/srfi/%3a45/lazy.sls
CLEANFILES += lib/srfi/%3a45/lazy.fasl
endif

lib/srfi/%3a48.fasl: \
		lib/srfi/%3a48.sls \
		lib/srfi/%3a48/intermediate-format-strings.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a48_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a48_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a48_fasl_DATA = lib/srfi/%3a48.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a48_sls_DATA = lib/srfi/%3a48.sls
endif
EXTRA_DIST += lib/srfi/%3a48.sls
CLEANFILES += lib/srfi/%3a48.fasl
endif

lib/srfi/%3a48/intermediate-format-strings.fasl: \
		lib/srfi/%3a48/intermediate-format-strings.sls \
		lib/srfi/%3a6/basic-string-ports.fasl \
		lib/srfi/%3a38/with-shared-structure.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a48_intermediate_format_strings_fasldir = $(bundledlibsdir)/srfi/%3a48
lib_srfi__3a48_intermediate_format_strings_slsdir  = $(bundledlibsdir)/srfi/%3a48
nodist_lib_srfi__3a48_intermediate_format_strings_fasl_DATA = lib/srfi/%3a48/intermediate-format-strings.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a48_intermediate_format_strings_sls_DATA = lib/srfi/%3a48/intermediate-format-strings.sls
endif
EXTRA_DIST += lib/srfi/%3a48/intermediate-format-strings.sls
CLEANFILES += lib/srfi/%3a48/intermediate-format-strings.fasl
endif

lib/srfi/%3a61.fasl: \
		lib/srfi/%3a61.sls \
		lib/srfi/%3a61/cond.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a61_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a61_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a61_fasl_DATA = lib/srfi/%3a61.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a61_sls_DATA = lib/srfi/%3a61.sls
endif
EXTRA_DIST += lib/srfi/%3a61.sls
CLEANFILES += lib/srfi/%3a61.fasl
endif

lib/srfi/%3a61/cond.fasl: \
		lib/srfi/%3a61/cond.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a61_cond_fasldir = $(bundledlibsdir)/srfi/%3a61
lib_srfi__3a61_cond_slsdir  = $(bundledlibsdir)/srfi/%3a61
nodist_lib_srfi__3a61_cond_fasl_DATA = lib/srfi/%3a61/cond.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a61_cond_sls_DATA = lib/srfi/%3a61/cond.sls
endif
EXTRA_DIST += lib/srfi/%3a61/cond.sls
CLEANFILES += lib/srfi/%3a61/cond.fasl
endif

lib/srfi/%3a64.fasl: \
		lib/srfi/%3a64.sls \
		lib/srfi/%3a64/testing.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a64_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a64_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a64_fasl_DATA = lib/srfi/%3a64.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a64_sls_DATA = lib/srfi/%3a64.sls
endif
EXTRA_DIST += lib/srfi/%3a64.sls
CLEANFILES += lib/srfi/%3a64.fasl
endif

lib/srfi/%3a64/testing.fasl: \
		lib/srfi/%3a64/testing.sls \
		lib/srfi/%3a0/cond-expand.fasl \
		lib/srfi/%3a1/lists.fasl \
		lib/srfi/%3a6/basic-string-ports.fasl \
		lib/srfi/%3a9/records.fasl \
		lib/srfi/%3a39/parameters.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a64_testing_fasldir = $(bundledlibsdir)/srfi/%3a64
lib_srfi__3a64_testing_slsdir  = $(bundledlibsdir)/srfi/%3a64
nodist_lib_srfi__3a64_testing_fasl_DATA = lib/srfi/%3a64/testing.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a64_testing_sls_DATA = lib/srfi/%3a64/testing.sls
endif
EXTRA_DIST += lib/srfi/%3a64/testing.sls
CLEANFILES += lib/srfi/%3a64/testing.fasl
endif

lib/srfi/%3a67.fasl: \
		lib/srfi/%3a67.sls \
		lib/srfi/%3a67/compare-procedures.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a67_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a67_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a67_fasl_DATA = lib/srfi/%3a67.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a67_sls_DATA = lib/srfi/%3a67.sls
endif
EXTRA_DIST += lib/srfi/%3a67.sls
CLEANFILES += lib/srfi/%3a67.fasl
endif

lib/srfi/%3a67/compare-procedures.fasl: \
		lib/srfi/%3a67/compare-procedures.sls \
		lib/srfi/%3a27/random-bits.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a67_compare_procedures_fasldir = $(bundledlibsdir)/srfi/%3a67
lib_srfi__3a67_compare_procedures_slsdir  = $(bundledlibsdir)/srfi/%3a67
nodist_lib_srfi__3a67_compare_procedures_fasl_DATA = lib/srfi/%3a67/compare-procedures.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a67_compare_procedures_sls_DATA = lib/srfi/%3a67/compare-procedures.sls
endif
EXTRA_DIST += lib/srfi/%3a67/compare-procedures.sls
CLEANFILES += lib/srfi/%3a67/compare-procedures.fasl
endif

lib/srfi/%3a69.fasl: \
		lib/srfi/%3a69.sls \
		lib/srfi/%3a69/basic-hash-tables.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a69_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a69_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a69_fasl_DATA = lib/srfi/%3a69.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a69_sls_DATA = lib/srfi/%3a69.sls
endif
EXTRA_DIST += lib/srfi/%3a69.sls
CLEANFILES += lib/srfi/%3a69.fasl
endif

lib/srfi/%3a69/basic-hash-tables.fasl: \
		lib/srfi/%3a69/basic-hash-tables.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a69_basic_hash_tables_fasldir = $(bundledlibsdir)/srfi/%3a69
lib_srfi__3a69_basic_hash_tables_slsdir  = $(bundledlibsdir)/srfi/%3a69
nodist_lib_srfi__3a69_basic_hash_tables_fasl_DATA = lib/srfi/%3a69/basic-hash-tables.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a69_basic_hash_tables_sls_DATA = lib/srfi/%3a69/basic-hash-tables.sls
endif
EXTRA_DIST += lib/srfi/%3a69/basic-hash-tables.sls
CLEANFILES += lib/srfi/%3a69/basic-hash-tables.fasl
endif

lib/srfi/%3a78.fasl: \
		lib/srfi/%3a78.sls \
		lib/srfi/%3a78/lightweight-testing.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a78_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a78_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a78_fasl_DATA = lib/srfi/%3a78.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a78_sls_DATA = lib/srfi/%3a78.sls
endif
EXTRA_DIST += lib/srfi/%3a78.sls
CLEANFILES += lib/srfi/%3a78.fasl
endif

lib/srfi/%3a78/lightweight-testing.fasl: \
		lib/srfi/%3a78/lightweight-testing.sls \
		lib/srfi/%3a39/parameters.fasl \
		lib/srfi/%3a42/eager-comprehensions.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a78_lightweight_testing_fasldir = $(bundledlibsdir)/srfi/%3a78
lib_srfi__3a78_lightweight_testing_slsdir  = $(bundledlibsdir)/srfi/%3a78
nodist_lib_srfi__3a78_lightweight_testing_fasl_DATA = lib/srfi/%3a78/lightweight-testing.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a78_lightweight_testing_sls_DATA = lib/srfi/%3a78/lightweight-testing.sls
endif
EXTRA_DIST += lib/srfi/%3a78/lightweight-testing.sls
CLEANFILES += lib/srfi/%3a78/lightweight-testing.fasl
endif

lib/srfi/%3a98.fasl: \
		lib/srfi/%3a98.sls \
		lib/srfi/%3a98/os-environment-variables.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a98_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a98_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a98_fasl_DATA = lib/srfi/%3a98.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a98_sls_DATA = lib/srfi/%3a98.sls
endif
EXTRA_DIST += lib/srfi/%3a98.sls
CLEANFILES += lib/srfi/%3a98.fasl
endif

lib/srfi/%3a98/os-environment-variables.fasl: \
		lib/srfi/%3a98/os-environment-variables.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a98_os_environment_variables_fasldir = $(bundledlibsdir)/srfi/%3a98
lib_srfi__3a98_os_environment_variables_slsdir  = $(bundledlibsdir)/srfi/%3a98
nodist_lib_srfi__3a98_os_environment_variables_fasl_DATA = lib/srfi/%3a98/os-environment-variables.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a98_os_environment_variables_sls_DATA = lib/srfi/%3a98/os-environment-variables.sls
endif
EXTRA_DIST += lib/srfi/%3a98/os-environment-variables.sls
CLEANFILES += lib/srfi/%3a98/os-environment-variables.fasl
endif

lib/srfi/%3a99.fasl: \
		lib/srfi/%3a99.sls \
		lib/srfi/%3a99/records.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a99_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a99_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a99_fasl_DATA = lib/srfi/%3a99.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a99_sls_DATA = lib/srfi/%3a99.sls
endif
EXTRA_DIST += lib/srfi/%3a99.sls
CLEANFILES += lib/srfi/%3a99.fasl
endif

lib/srfi/%3a99/records.fasl: \
		lib/srfi/%3a99/records.sls \
		lib/srfi/%3a99/records/inspection.fasl \
		lib/srfi/%3a99/records/procedural.fasl \
		lib/srfi/%3a99/records/syntactic.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a99_records_fasldir = $(bundledlibsdir)/srfi/%3a99
lib_srfi__3a99_records_slsdir  = $(bundledlibsdir)/srfi/%3a99
nodist_lib_srfi__3a99_records_fasl_DATA = lib/srfi/%3a99/records.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a99_records_sls_DATA = lib/srfi/%3a99/records.sls
endif
EXTRA_DIST += lib/srfi/%3a99/records.sls
CLEANFILES += lib/srfi/%3a99/records.fasl
endif

lib/srfi/%3a99/records/inspection.fasl: \
		lib/srfi/%3a99/records/inspection.sls \
		lib/srfi/%3a99/records/helper.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a99_records_inspection_fasldir = $(bundledlibsdir)/srfi/%3a99/records
lib_srfi__3a99_records_inspection_slsdir  = $(bundledlibsdir)/srfi/%3a99/records
nodist_lib_srfi__3a99_records_inspection_fasl_DATA = lib/srfi/%3a99/records/inspection.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a99_records_inspection_sls_DATA = lib/srfi/%3a99/records/inspection.sls
endif
EXTRA_DIST += lib/srfi/%3a99/records/inspection.sls
CLEANFILES += lib/srfi/%3a99/records/inspection.fasl
endif

lib/srfi/%3a99/records/helper.fasl: \
		lib/srfi/%3a99/records/helper.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a99_records_helper_fasldir = $(bundledlibsdir)/srfi/%3a99/records
lib_srfi__3a99_records_helper_slsdir  = $(bundledlibsdir)/srfi/%3a99/records
nodist_lib_srfi__3a99_records_helper_fasl_DATA = lib/srfi/%3a99/records/helper.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a99_records_helper_sls_DATA = lib/srfi/%3a99/records/helper.sls
endif
EXTRA_DIST += lib/srfi/%3a99/records/helper.sls
CLEANFILES += lib/srfi/%3a99/records/helper.fasl
endif

lib/srfi/%3a99/records/procedural.fasl: \
		lib/srfi/%3a99/records/procedural.sls \
		lib/srfi/%3a99/records/inspection.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a99_records_procedural_fasldir = $(bundledlibsdir)/srfi/%3a99/records
lib_srfi__3a99_records_procedural_slsdir  = $(bundledlibsdir)/srfi/%3a99/records
nodist_lib_srfi__3a99_records_procedural_fasl_DATA = lib/srfi/%3a99/records/procedural.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a99_records_procedural_sls_DATA = lib/srfi/%3a99/records/procedural.sls
endif
EXTRA_DIST += lib/srfi/%3a99/records/procedural.sls
CLEANFILES += lib/srfi/%3a99/records/procedural.fasl
endif

lib/srfi/%3a99/records/syntactic.fasl: \
		lib/srfi/%3a99/records/syntactic.sls \
		lib/srfi/%3a99/records/procedural.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a99_records_syntactic_fasldir = $(bundledlibsdir)/srfi/%3a99/records
lib_srfi__3a99_records_syntactic_slsdir  = $(bundledlibsdir)/srfi/%3a99/records
nodist_lib_srfi__3a99_records_syntactic_fasl_DATA = lib/srfi/%3a99/records/syntactic.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a99_records_syntactic_sls_DATA = lib/srfi/%3a99/records/syntactic.sls
endif
EXTRA_DIST += lib/srfi/%3a99/records/syntactic.sls
CLEANFILES += lib/srfi/%3a99/records/syntactic.fasl
endif

lib/srfi/%3a101.fasl: \
		lib/srfi/%3a101.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a101_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a101_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a101_fasl_DATA = lib/srfi/%3a101.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a101_sls_DATA = lib/srfi/%3a101.sls
endif
EXTRA_DIST += lib/srfi/%3a101.sls
CLEANFILES += lib/srfi/%3a101.fasl
endif

lib/srfi/%3a101/random-access-lists.fasl: \
		lib/srfi/%3a101/random-access-lists.sls \
		lib/srfi/%3a101.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a101_random_access_lists_fasldir = $(bundledlibsdir)/srfi/%3a101
lib_srfi__3a101_random_access_lists_slsdir  = $(bundledlibsdir)/srfi/%3a101
nodist_lib_srfi__3a101_random_access_lists_fasl_DATA = lib/srfi/%3a101/random-access-lists.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a101_random_access_lists_sls_DATA = lib/srfi/%3a101/random-access-lists.sls
endif
EXTRA_DIST += lib/srfi/%3a101/random-access-lists.sls
CLEANFILES += lib/srfi/%3a101/random-access-lists.fasl
endif

lib/srfi/%3a101/random-access-lists/procedures.fasl: \
		lib/srfi/%3a101/random-access-lists/procedures.sls \
		lib/srfi/%3a101.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a101_random_access_lists_procedures_fasldir = $(bundledlibsdir)/srfi/%3a101/random-access-lists
lib_srfi__3a101_random_access_lists_procedures_slsdir  = $(bundledlibsdir)/srfi/%3a101/random-access-lists
nodist_lib_srfi__3a101_random_access_lists_procedures_fasl_DATA = lib/srfi/%3a101/random-access-lists/procedures.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a101_random_access_lists_procedures_sls_DATA = lib/srfi/%3a101/random-access-lists/procedures.sls
endif
EXTRA_DIST += lib/srfi/%3a101/random-access-lists/procedures.sls
CLEANFILES += lib/srfi/%3a101/random-access-lists/procedures.fasl
endif

lib/srfi/%3a101/random-access-lists/syntax.fasl: \
		lib/srfi/%3a101/random-access-lists/syntax.sls \
		lib/srfi/%3a101.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a101_random_access_lists_syntax_fasldir = $(bundledlibsdir)/srfi/%3a101/random-access-lists
lib_srfi__3a101_random_access_lists_syntax_slsdir  = $(bundledlibsdir)/srfi/%3a101/random-access-lists
nodist_lib_srfi__3a101_random_access_lists_syntax_fasl_DATA = lib/srfi/%3a101/random-access-lists/syntax.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a101_random_access_lists_syntax_sls_DATA = lib/srfi/%3a101/random-access-lists/syntax.sls
endif
EXTRA_DIST += lib/srfi/%3a101/random-access-lists/syntax.sls
CLEANFILES += lib/srfi/%3a101/random-access-lists/syntax.fasl
endif

lib/srfi/%3a101/random-access-lists/equal.fasl: \
		lib/srfi/%3a101/random-access-lists/equal.sls \
		lib/srfi/%3a101.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a101_random_access_lists_equal_fasldir = $(bundledlibsdir)/srfi/%3a101/random-access-lists
lib_srfi__3a101_random_access_lists_equal_slsdir  = $(bundledlibsdir)/srfi/%3a101/random-access-lists
nodist_lib_srfi__3a101_random_access_lists_equal_fasl_DATA = lib/srfi/%3a101/random-access-lists/equal.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a101_random_access_lists_equal_sls_DATA = lib/srfi/%3a101/random-access-lists/equal.sls
endif
EXTRA_DIST += lib/srfi/%3a101/random-access-lists/equal.sls
CLEANFILES += lib/srfi/%3a101/random-access-lists/equal.fasl
endif

lib/srfi/%3a111.fasl: \
		lib/srfi/%3a111.sls \
		lib/srfi/%3a111/boxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a111_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a111_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a111_fasl_DATA = lib/srfi/%3a111.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a111_sls_DATA = lib/srfi/%3a111.sls
endif
EXTRA_DIST += lib/srfi/%3a111.sls
CLEANFILES += lib/srfi/%3a111.fasl
endif

lib/srfi/%3a111/boxes.fasl: \
		lib/srfi/%3a111/boxes.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a111_boxes_fasldir = $(bundledlibsdir)/srfi/%3a111
lib_srfi__3a111_boxes_slsdir  = $(bundledlibsdir)/srfi/%3a111
nodist_lib_srfi__3a111_boxes_fasl_DATA = lib/srfi/%3a111/boxes.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a111_boxes_sls_DATA = lib/srfi/%3a111/boxes.sls
endif
EXTRA_DIST += lib/srfi/%3a111/boxes.sls
CLEANFILES += lib/srfi/%3a111/boxes.fasl
endif

lib/srfi/%3a112.fasl: \
		lib/srfi/%3a112.sls \
		lib/srfi/%3a112/environment-inquiry.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a112_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a112_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a112_fasl_DATA = lib/srfi/%3a112.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a112_sls_DATA = lib/srfi/%3a112.sls
endif
EXTRA_DIST += lib/srfi/%3a112.sls
CLEANFILES += lib/srfi/%3a112.fasl
endif

lib/srfi/%3a112/environment-inquiry.fasl: \
		lib/srfi/%3a112/environment-inquiry.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
lib_srfi__3a112_environment_inquiry_fasldir = $(bundledlibsdir)/srfi/%3a112
lib_srfi__3a112_environment_inquiry_slsdir  = $(bundledlibsdir)/srfi/%3a112
nodist_lib_srfi__3a112_environment_inquiry_fasl_DATA = lib/srfi/%3a112/environment-inquiry.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a112_environment_inquiry_sls_DATA = lib/srfi/%3a112/environment-inquiry.sls
endif
EXTRA_DIST += lib/srfi/%3a112/environment-inquiry.sls
CLEANFILES += lib/srfi/%3a112/environment-inquiry.fasl
endif

lib/srfi/%3a106.fasl: \
		lib/srfi/%3a106.sls \
		lib/srfi/%3a106/socket.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
if WANT_POSIX
lib_srfi__3a106_fasldir = $(bundledlibsdir)/srfi
lib_srfi__3a106_slsdir  = $(bundledlibsdir)/srfi
nodist_lib_srfi__3a106_fasl_DATA = lib/srfi/%3a106.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a106_sls_DATA = lib/srfi/%3a106.sls
endif
EXTRA_DIST += lib/srfi/%3a106.sls
CLEANFILES += lib/srfi/%3a106.fasl
endif
endif

lib/srfi/%3a106/socket.fasl: \
		lib/srfi/%3a106/socket.sls \
		lib/srfi/%3a106/compat.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
if WANT_POSIX
lib_srfi__3a106_socket_fasldir = $(bundledlibsdir)/srfi/%3a106
lib_srfi__3a106_socket_slsdir  = $(bundledlibsdir)/srfi/%3a106
nodist_lib_srfi__3a106_socket_fasl_DATA = lib/srfi/%3a106/socket.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a106_socket_sls_DATA = lib/srfi/%3a106/socket.sls
endif
EXTRA_DIST += lib/srfi/%3a106/socket.sls
CLEANFILES += lib/srfi/%3a106/socket.fasl
endif
endif

lib/srfi/%3a106/compat.fasl: \
		lib/srfi/%3a106/compat.vicare.sls \
		lib/vicare/platform/constants.fasl \
		lib/vicare/posix.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_SRFI
if WANT_POSIX
lib_srfi__3a106_compat_fasldir = $(bundledlibsdir)/srfi/%3a106
lib_srfi__3a106_compat_vicare_slsdir  = $(bundledlibsdir)/srfi/%3a106
nodist_lib_srfi__3a106_compat_fasl_DATA = lib/srfi/%3a106/compat.fasl
if WANT_INSTALL_SOURCES
dist_lib_srfi__3a106_compat_vicare_sls_DATA = lib/srfi/%3a106/compat.vicare.sls
endif
EXTRA_DIST += lib/srfi/%3a106/compat.vicare.sls
CLEANFILES += lib/srfi/%3a106/compat.fasl
endif
endif

lib/nausicaa/language/auxiliary-syntaxes.fasl: \
		lib/nausicaa/language/auxiliary-syntaxes.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_auxiliary_syntaxes_fasldir = $(bundledlibsdir)/nausicaa/language
lib_nausicaa_language_auxiliary_syntaxes_slsdir  = $(bundledlibsdir)/nausicaa/language
nodist_lib_nausicaa_language_auxiliary_syntaxes_fasl_DATA = lib/nausicaa/language/auxiliary-syntaxes.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_auxiliary_syntaxes_sls_DATA = lib/nausicaa/language/auxiliary-syntaxes.sls
endif
EXTRA_DIST += lib/nausicaa/language/auxiliary-syntaxes.sls
CLEANFILES += lib/nausicaa/language/auxiliary-syntaxes.fasl
endif

lib/nausicaa/language/oopp.fasl: \
		lib/nausicaa/language/oopp.sls \
		lib/nausicaa/language/oopp/auxiliary-syntaxes.fasl \
		lib/nausicaa/language/oopp/conditions.fasl \
		lib/nausicaa/language/oopp/oopp-syntax-helpers.fasl \
		lib/nausicaa/language/oopp/definition-parser-helpers.fasl \
		lib/nausicaa/language/oopp/configuration.fasl \
		lib/nausicaa/language/auxiliary-syntaxes.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_oopp_fasldir = $(bundledlibsdir)/nausicaa/language
lib_nausicaa_language_oopp_slsdir  = $(bundledlibsdir)/nausicaa/language
nodist_lib_nausicaa_language_oopp_fasl_DATA = lib/nausicaa/language/oopp.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_oopp_sls_DATA = lib/nausicaa/language/oopp.sls
endif
EXTRA_DIST += lib/nausicaa/language/oopp.sls
CLEANFILES += lib/nausicaa/language/oopp.fasl
endif

lib/nausicaa/language/oopp/auxiliary-syntaxes.fasl: \
		lib/nausicaa/language/oopp/auxiliary-syntaxes.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_oopp_auxiliary_syntaxes_fasldir = $(bundledlibsdir)/nausicaa/language/oopp
lib_nausicaa_language_oopp_auxiliary_syntaxes_slsdir  = $(bundledlibsdir)/nausicaa/language/oopp
nodist_lib_nausicaa_language_oopp_auxiliary_syntaxes_fasl_DATA = lib/nausicaa/language/oopp/auxiliary-syntaxes.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_oopp_auxiliary_syntaxes_sls_DATA = lib/nausicaa/language/oopp/auxiliary-syntaxes.sls
endif
EXTRA_DIST += lib/nausicaa/language/oopp/auxiliary-syntaxes.sls
CLEANFILES += lib/nausicaa/language/oopp/auxiliary-syntaxes.fasl
endif

lib/nausicaa/language/oopp/conditions.fasl: \
		lib/nausicaa/language/oopp/conditions.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_oopp_conditions_fasldir = $(bundledlibsdir)/nausicaa/language/oopp
lib_nausicaa_language_oopp_conditions_slsdir  = $(bundledlibsdir)/nausicaa/language/oopp
nodist_lib_nausicaa_language_oopp_conditions_fasl_DATA = lib/nausicaa/language/oopp/conditions.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_oopp_conditions_sls_DATA = lib/nausicaa/language/oopp/conditions.sls
endif
EXTRA_DIST += lib/nausicaa/language/oopp/conditions.sls
CLEANFILES += lib/nausicaa/language/oopp/conditions.fasl
endif

lib/nausicaa/language/oopp/oopp-syntax-helpers.fasl: \
		lib/nausicaa/language/oopp/oopp-syntax-helpers.sls \
		lib/nausicaa/language/oopp/configuration.fasl \
		lib/nausicaa/language/oopp/auxiliary-syntaxes.fasl \
		lib/nausicaa/language/auxiliary-syntaxes.fasl \
		lib/nausicaa/language/oopp/conditions.fasl \
		lib/vicare/language-extensions/identifier-substitutions.fasl \
		lib/nausicaa/language/oopp/definition-parser-helpers.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_oopp_oopp_syntax_helpers_fasldir = $(bundledlibsdir)/nausicaa/language/oopp
lib_nausicaa_language_oopp_oopp_syntax_helpers_slsdir  = $(bundledlibsdir)/nausicaa/language/oopp
nodist_lib_nausicaa_language_oopp_oopp_syntax_helpers_fasl_DATA = lib/nausicaa/language/oopp/oopp-syntax-helpers.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_oopp_oopp_syntax_helpers_sls_DATA = lib/nausicaa/language/oopp/oopp-syntax-helpers.sls
endif
EXTRA_DIST += lib/nausicaa/language/oopp/oopp-syntax-helpers.sls
CLEANFILES += lib/nausicaa/language/oopp/oopp-syntax-helpers.fasl
endif

lib/nausicaa/language/oopp/configuration.fasl: \
		lib/nausicaa/language/oopp/configuration.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_oopp_configuration_fasldir = $(bundledlibsdir)/nausicaa/language/oopp
lib_nausicaa_language_oopp_configuration_slsdir  = $(bundledlibsdir)/nausicaa/language/oopp
nodist_lib_nausicaa_language_oopp_configuration_fasl_DATA = lib/nausicaa/language/oopp/configuration.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_oopp_configuration_sls_DATA = lib/nausicaa/language/oopp/configuration.sls
endif
EXTRA_DIST += lib/nausicaa/language/oopp/configuration.sls
CLEANFILES += lib/nausicaa/language/oopp/configuration.fasl
endif

lib/nausicaa/language/oopp/definition-parser-helpers.fasl: \
		lib/nausicaa/language/oopp/definition-parser-helpers.sls \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/language-extensions/identifier-substitutions.fasl \
		lib/nausicaa/language/oopp/configuration.fasl \
		lib/nausicaa/language/oopp/auxiliary-syntaxes.fasl \
		lib/nausicaa/language/auxiliary-syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_oopp_definition_parser_helpers_fasldir = $(bundledlibsdir)/nausicaa/language/oopp
lib_nausicaa_language_oopp_definition_parser_helpers_slsdir  = $(bundledlibsdir)/nausicaa/language/oopp
nodist_lib_nausicaa_language_oopp_definition_parser_helpers_fasl_DATA = lib/nausicaa/language/oopp/definition-parser-helpers.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_oopp_definition_parser_helpers_sls_DATA = lib/nausicaa/language/oopp/definition-parser-helpers.sls
endif
EXTRA_DIST += lib/nausicaa/language/oopp/definition-parser-helpers.sls
CLEANFILES += lib/nausicaa/language/oopp/definition-parser-helpers.fasl
endif

lib/nausicaa/language/multimethods.fasl: \
		lib/nausicaa/language/multimethods.sls \
		lib/nausicaa/language/multimethods/types.fasl \
		lib/nausicaa/language/multimethods/methods-table.fasl \
		lib/nausicaa/language/auxiliary-syntaxes.fasl \
		lib/nausicaa/language/multimethods/auxiliary-syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_multimethods_fasldir = $(bundledlibsdir)/nausicaa/language
lib_nausicaa_language_multimethods_slsdir  = $(bundledlibsdir)/nausicaa/language
nodist_lib_nausicaa_language_multimethods_fasl_DATA = lib/nausicaa/language/multimethods.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_multimethods_sls_DATA = lib/nausicaa/language/multimethods.sls
endif
EXTRA_DIST += lib/nausicaa/language/multimethods.sls
CLEANFILES += lib/nausicaa/language/multimethods.fasl
endif

lib/nausicaa/language/multimethods/types.fasl: \
		lib/nausicaa/language/multimethods/types.sls \
		lib/nausicaa/language/oopp.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_multimethods_types_fasldir = $(bundledlibsdir)/nausicaa/language/multimethods
lib_nausicaa_language_multimethods_types_slsdir  = $(bundledlibsdir)/nausicaa/language/multimethods
nodist_lib_nausicaa_language_multimethods_types_fasl_DATA = lib/nausicaa/language/multimethods/types.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_multimethods_types_sls_DATA = lib/nausicaa/language/multimethods/types.sls
endif
EXTRA_DIST += lib/nausicaa/language/multimethods/types.sls
CLEANFILES += lib/nausicaa/language/multimethods/types.fasl
endif

lib/nausicaa/language/multimethods/methods-table.fasl: \
		lib/nausicaa/language/multimethods/methods-table.sls \
		lib/nausicaa/language/symbols-tree.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_multimethods_methods_table_fasldir = $(bundledlibsdir)/nausicaa/language/multimethods
lib_nausicaa_language_multimethods_methods_table_slsdir  = $(bundledlibsdir)/nausicaa/language/multimethods
nodist_lib_nausicaa_language_multimethods_methods_table_fasl_DATA = lib/nausicaa/language/multimethods/methods-table.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_multimethods_methods_table_sls_DATA = lib/nausicaa/language/multimethods/methods-table.sls
endif
EXTRA_DIST += lib/nausicaa/language/multimethods/methods-table.sls
CLEANFILES += lib/nausicaa/language/multimethods/methods-table.fasl
endif

lib/nausicaa/language/symbols-tree.fasl: \
		lib/nausicaa/language/symbols-tree.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_symbols_tree_fasldir = $(bundledlibsdir)/nausicaa/language
lib_nausicaa_language_symbols_tree_slsdir  = $(bundledlibsdir)/nausicaa/language
nodist_lib_nausicaa_language_symbols_tree_fasl_DATA = lib/nausicaa/language/symbols-tree.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_symbols_tree_sls_DATA = lib/nausicaa/language/symbols-tree.sls
endif
EXTRA_DIST += lib/nausicaa/language/symbols-tree.sls
CLEANFILES += lib/nausicaa/language/symbols-tree.fasl
endif

lib/nausicaa/language/multimethods/auxiliary-syntaxes.fasl: \
		lib/nausicaa/language/multimethods/auxiliary-syntaxes.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_multimethods_auxiliary_syntaxes_fasldir = $(bundledlibsdir)/nausicaa/language/multimethods
lib_nausicaa_language_multimethods_auxiliary_syntaxes_slsdir  = $(bundledlibsdir)/nausicaa/language/multimethods
nodist_lib_nausicaa_language_multimethods_auxiliary_syntaxes_fasl_DATA = lib/nausicaa/language/multimethods/auxiliary-syntaxes.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_multimethods_auxiliary_syntaxes_sls_DATA = lib/nausicaa/language/multimethods/auxiliary-syntaxes.sls
endif
EXTRA_DIST += lib/nausicaa/language/multimethods/auxiliary-syntaxes.sls
CLEANFILES += lib/nausicaa/language/multimethods/auxiliary-syntaxes.fasl
endif

lib/nausicaa/language/builtins.fasl: \
		lib/nausicaa/language/builtins.sls \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/language-extensions/sentinels.fasl \
		lib/vicare/containers/bytevectors.fasl \
		lib/nausicaa/language/oopp.fasl \
		lib/nausicaa/language/multimethods.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_builtins_fasldir = $(bundledlibsdir)/nausicaa/language
lib_nausicaa_language_builtins_slsdir  = $(bundledlibsdir)/nausicaa/language
nodist_lib_nausicaa_language_builtins_fasl_DATA = lib/nausicaa/language/builtins.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_builtins_sls_DATA = lib/nausicaa/language/builtins.sls
endif
EXTRA_DIST += lib/nausicaa/language/builtins.sls
CLEANFILES += lib/nausicaa/language/builtins.fasl
endif

lib/nausicaa/language/conditions.fasl: \
		lib/nausicaa/language/conditions.sls \
		lib/nausicaa/language/oopp.fasl \
		lib/nausicaa/language/builtins.fasl \
		lib/nausicaa/language/auxiliary-syntaxes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_conditions_fasldir = $(bundledlibsdir)/nausicaa/language
lib_nausicaa_language_conditions_slsdir  = $(bundledlibsdir)/nausicaa/language
nodist_lib_nausicaa_language_conditions_fasl_DATA = lib/nausicaa/language/conditions.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_conditions_sls_DATA = lib/nausicaa/language/conditions.sls
endif
EXTRA_DIST += lib/nausicaa/language/conditions.sls
CLEANFILES += lib/nausicaa/language/conditions.fasl
endif

lib/nausicaa/language/simple-match.fasl: \
		lib/nausicaa/language/simple-match.sls \
		lib/nausicaa/language/oopp.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_language_simple_match_fasldir = $(bundledlibsdir)/nausicaa/language
lib_nausicaa_language_simple_match_slsdir  = $(bundledlibsdir)/nausicaa/language
nodist_lib_nausicaa_language_simple_match_fasl_DATA = lib/nausicaa/language/simple-match.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_language_simple_match_sls_DATA = lib/nausicaa/language/simple-match.sls
endif
EXTRA_DIST += lib/nausicaa/language/simple-match.sls
CLEANFILES += lib/nausicaa/language/simple-match.fasl
endif

lib/nausicaa.fasl: \
		lib/nausicaa.sls \
		lib/nausicaa/language/oopp.fasl \
		lib/nausicaa/language/multimethods.fasl \
		lib/nausicaa/language/builtins.fasl \
		lib/nausicaa/language/conditions.fasl \
		lib/nausicaa/language/simple-match.fasl \
		lib/vicare/language-extensions/namespaces.fasl \
		lib/vicare/language-extensions/sentinels.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_fasldir = $(bundledlibsdir)/
lib_nausicaa_slsdir  = $(bundledlibsdir)/
nodist_lib_nausicaa_fasl_DATA = lib/nausicaa.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_sls_DATA = lib/nausicaa.sls
endif
EXTRA_DIST += lib/nausicaa.sls
CLEANFILES += lib/nausicaa.fasl
endif

lib/nausicaa/containers/lists.fasl: \
		lib/nausicaa/containers/lists.sls \
		lib/nausicaa.fasl \
		lib/vicare/containers/lists.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_containers_lists_fasldir = $(bundledlibsdir)/nausicaa/containers
lib_nausicaa_containers_lists_slsdir  = $(bundledlibsdir)/nausicaa/containers
nodist_lib_nausicaa_containers_lists_fasl_DATA = lib/nausicaa/containers/lists.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_containers_lists_sls_DATA = lib/nausicaa/containers/lists.sls
endif
EXTRA_DIST += lib/nausicaa/containers/lists.sls
CLEANFILES += lib/nausicaa/containers/lists.fasl
endif

lib/nausicaa/containers/vectors.fasl: \
		lib/nausicaa/containers/vectors.sls \
		lib/nausicaa.fasl \
		lib/vicare/containers/vectors.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_containers_vectors_fasldir = $(bundledlibsdir)/nausicaa/containers
lib_nausicaa_containers_vectors_slsdir  = $(bundledlibsdir)/nausicaa/containers
nodist_lib_nausicaa_containers_vectors_fasl_DATA = lib/nausicaa/containers/vectors.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_containers_vectors_sls_DATA = lib/nausicaa/containers/vectors.sls
endif
EXTRA_DIST += lib/nausicaa/containers/vectors.sls
CLEANFILES += lib/nausicaa/containers/vectors.fasl
endif

lib/nausicaa/containers/strings.fasl: \
		lib/nausicaa/containers/strings.sls \
		lib/nausicaa.fasl \
		lib/vicare/containers/strings.fasl \
		lib/vicare/containers/strings/low.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_containers_strings_fasldir = $(bundledlibsdir)/nausicaa/containers
lib_nausicaa_containers_strings_slsdir  = $(bundledlibsdir)/nausicaa/containers
nodist_lib_nausicaa_containers_strings_fasl_DATA = lib/nausicaa/containers/strings.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_containers_strings_sls_DATA = lib/nausicaa/containers/strings.sls
endif
EXTRA_DIST += lib/nausicaa/containers/strings.sls
CLEANFILES += lib/nausicaa/containers/strings.fasl
endif

lib/nausicaa/containers/arrays.fasl: \
		lib/nausicaa/containers/arrays.sls \
		lib/nausicaa.fasl \
		lib/vicare/containers/strings.fasl \
		lib/vicare/containers/lists.fasl \
		lib/vicare/containers/vectors.fasl \
		lib/nausicaa/containers/lists.fasl \
		lib/nausicaa/containers/vectors.fasl \
		lib/nausicaa/containers/strings.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_containers_arrays_fasldir = $(bundledlibsdir)/nausicaa/containers
lib_nausicaa_containers_arrays_slsdir  = $(bundledlibsdir)/nausicaa/containers
nodist_lib_nausicaa_containers_arrays_fasl_DATA = lib/nausicaa/containers/arrays.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_containers_arrays_sls_DATA = lib/nausicaa/containers/arrays.sls
endif
EXTRA_DIST += lib/nausicaa/containers/arrays.sls
CLEANFILES += lib/nausicaa/containers/arrays.fasl
endif

lib/nausicaa/containers/stacks.fasl: \
		lib/nausicaa/containers/stacks.sls \
		lib/nausicaa.fasl \
		lib/vicare/containers/stacks.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_containers_stacks_fasldir = $(bundledlibsdir)/nausicaa/containers
lib_nausicaa_containers_stacks_slsdir  = $(bundledlibsdir)/nausicaa/containers
nodist_lib_nausicaa_containers_stacks_fasl_DATA = lib/nausicaa/containers/stacks.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_containers_stacks_sls_DATA = lib/nausicaa/containers/stacks.sls
endif
EXTRA_DIST += lib/nausicaa/containers/stacks.sls
CLEANFILES += lib/nausicaa/containers/stacks.fasl
endif

lib/nausicaa/containers/queues.fasl: \
		lib/nausicaa/containers/queues.sls \
		lib/nausicaa.fasl \
		lib/vicare/containers/queues.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_containers_queues_fasldir = $(bundledlibsdir)/nausicaa/containers
lib_nausicaa_containers_queues_slsdir  = $(bundledlibsdir)/nausicaa/containers
nodist_lib_nausicaa_containers_queues_fasl_DATA = lib/nausicaa/containers/queues.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_containers_queues_sls_DATA = lib/nausicaa/containers/queues.sls
endif
EXTRA_DIST += lib/nausicaa/containers/queues.sls
CLEANFILES += lib/nausicaa/containers/queues.fasl
endif

lib/nausicaa/containers/bitvectors.fasl: \
		lib/nausicaa/containers/bitvectors.sls \
		lib/nausicaa.fasl \
		lib/vicare/platform/words.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_containers_bitvectors_fasldir = $(bundledlibsdir)/nausicaa/containers
lib_nausicaa_containers_bitvectors_slsdir  = $(bundledlibsdir)/nausicaa/containers
nodist_lib_nausicaa_containers_bitvectors_fasl_DATA = lib/nausicaa/containers/bitvectors.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_containers_bitvectors_sls_DATA = lib/nausicaa/containers/bitvectors.sls
endif
EXTRA_DIST += lib/nausicaa/containers/bitvectors.sls
CLEANFILES += lib/nausicaa/containers/bitvectors.fasl
endif

lib/nausicaa/containers/iterators.fasl: \
		lib/nausicaa/containers/iterators.sls \
		lib/nausicaa.fasl \
		lib/vicare/language-extensions/makers.fasl \
		lib/vicare/arguments/validation.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_containers_iterators_fasldir = $(bundledlibsdir)/nausicaa/containers
lib_nausicaa_containers_iterators_slsdir  = $(bundledlibsdir)/nausicaa/containers
nodist_lib_nausicaa_containers_iterators_fasl_DATA = lib/nausicaa/containers/iterators.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_containers_iterators_sls_DATA = lib/nausicaa/containers/iterators.sls
endif
EXTRA_DIST += lib/nausicaa/containers/iterators.sls
CLEANFILES += lib/nausicaa/containers/iterators.fasl
endif

lib/nausicaa/parser-tools/source-locations.fasl: \
		lib/nausicaa/parser-tools/source-locations.sls \
		lib/nausicaa.fasl \
		lib/vicare/language-extensions/makers.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_source_locations_fasldir = $(bundledlibsdir)/nausicaa/parser-tools
lib_nausicaa_parser_tools_source_locations_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools
nodist_lib_nausicaa_parser_tools_source_locations_fasl_DATA = lib/nausicaa/parser-tools/source-locations.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_source_locations_sls_DATA = lib/nausicaa/parser-tools/source-locations.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/source-locations.sls
CLEANFILES += lib/nausicaa/parser-tools/source-locations.fasl
endif

lib/nausicaa/parser-tools/lexical-tokens.fasl: \
		lib/nausicaa/parser-tools/lexical-tokens.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		lib/vicare/language-extensions/makers.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_lexical_tokens_fasldir = $(bundledlibsdir)/nausicaa/parser-tools
lib_nausicaa_parser_tools_lexical_tokens_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools
nodist_lib_nausicaa_parser_tools_lexical_tokens_fasl_DATA = lib/nausicaa/parser-tools/lexical-tokens.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_lexical_tokens_sls_DATA = lib/nausicaa/parser-tools/lexical-tokens.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/lexical-tokens.sls
CLEANFILES += lib/nausicaa/parser-tools/lexical-tokens.fasl
endif

lib/nausicaa/parser-tools/silex/default-error-handler.fasl: \
		lib/nausicaa/parser-tools/silex/default-error-handler.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		lib/nausicaa/parser-tools/lexical-tokens.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_silex_default_error_handler_fasldir = $(bundledlibsdir)/nausicaa/parser-tools/silex
lib_nausicaa_parser_tools_silex_default_error_handler_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools/silex
nodist_lib_nausicaa_parser_tools_silex_default_error_handler_fasl_DATA = lib/nausicaa/parser-tools/silex/default-error-handler.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_silex_default_error_handler_sls_DATA = lib/nausicaa/parser-tools/silex/default-error-handler.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/silex/default-error-handler.sls
CLEANFILES += lib/nausicaa/parser-tools/silex/default-error-handler.fasl
endif

lib/nausicaa/parser-tools/lalr/lr-driver.fasl: \
		lib/nausicaa/parser-tools/lalr/lr-driver.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/lexical-tokens.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_lalr_lr_driver_fasldir = $(bundledlibsdir)/nausicaa/parser-tools/lalr
lib_nausicaa_parser_tools_lalr_lr_driver_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools/lalr
nodist_lib_nausicaa_parser_tools_lalr_lr_driver_fasl_DATA = lib/nausicaa/parser-tools/lalr/lr-driver.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_lalr_lr_driver_sls_DATA = lib/nausicaa/parser-tools/lalr/lr-driver.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/lalr/lr-driver.sls
CLEANFILES += lib/nausicaa/parser-tools/lalr/lr-driver.fasl
endif

lib/nausicaa/parser-tools/lalr/glr-driver.fasl: \
		lib/nausicaa/parser-tools/lalr/glr-driver.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/lexical-tokens.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_lalr_glr_driver_fasldir = $(bundledlibsdir)/nausicaa/parser-tools/lalr
lib_nausicaa_parser_tools_lalr_glr_driver_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools/lalr
nodist_lib_nausicaa_parser_tools_lalr_glr_driver_fasl_DATA = lib/nausicaa/parser-tools/lalr/glr-driver.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_lalr_glr_driver_sls_DATA = lib/nausicaa/parser-tools/lalr/glr-driver.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/lalr/glr-driver.sls
CLEANFILES += lib/nausicaa/parser-tools/lalr/glr-driver.fasl
endif

lib/nausicaa/parser-tools/lalr.fasl: \
		lib/nausicaa/parser-tools/lalr.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/lexical-tokens.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		lib/vicare/language-extensions/makers.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_lalr_fasldir = $(bundledlibsdir)/nausicaa/parser-tools
lib_nausicaa_parser_tools_lalr_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools
nodist_lib_nausicaa_parser_tools_lalr_fasl_DATA = lib/nausicaa/parser-tools/lalr.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_lalr_sls_DATA = lib/nausicaa/parser-tools/lalr.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/lalr.sls
CLEANFILES += lib/nausicaa/parser-tools/lalr.fasl
endif

lib/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.fasl: \
		lib/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.sls \
		lib/nausicaa.fasl \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/nausicaa/parser-tools/silex/default-error-handler.fasl \
		lib/nausicaa/parser-tools/lexical-tokens.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_ip_addresses_ipv4_address_lexer_fasldir = $(bundledlibsdir)/nausicaa/parser-tools/ip-addresses
lib_nausicaa_parser_tools_ip_addresses_ipv4_address_lexer_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools/ip-addresses
nodist_lib_nausicaa_parser_tools_ip_addresses_ipv4_address_lexer_fasl_DATA = lib/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_ip_addresses_ipv4_address_lexer_sls_DATA = lib/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.sls
CLEANFILES += lib/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.fasl
endif

lib/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.fasl: \
		lib/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/lalr/lr-driver.fasl \
		lib/nausicaa/parser-tools/lexical-tokens.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_ip_addresses_ipv4_address_parser_fasldir = $(bundledlibsdir)/nausicaa/parser-tools/ip-addresses
lib_nausicaa_parser_tools_ip_addresses_ipv4_address_parser_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools/ip-addresses
nodist_lib_nausicaa_parser_tools_ip_addresses_ipv4_address_parser_fasl_DATA = lib/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_ip_addresses_ipv4_address_parser_sls_DATA = lib/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.sls
CLEANFILES += lib/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.fasl
endif

lib/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.fasl: \
		lib/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.sls \
		lib/nausicaa.fasl \
		lib/vicare/parser-tools/silex/input-system.fasl \
		lib/nausicaa/parser-tools/silex/default-error-handler.fasl \
		lib/nausicaa/parser-tools/lexical-tokens.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_ip_addresses_ipv6_address_lexer_fasldir = $(bundledlibsdir)/nausicaa/parser-tools/ip-addresses
lib_nausicaa_parser_tools_ip_addresses_ipv6_address_lexer_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools/ip-addresses
nodist_lib_nausicaa_parser_tools_ip_addresses_ipv6_address_lexer_fasl_DATA = lib/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_ip_addresses_ipv6_address_lexer_sls_DATA = lib/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.sls
CLEANFILES += lib/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.fasl
endif

lib/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.fasl: \
		lib/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/lalr/lr-driver.fasl \
		lib/nausicaa/parser-tools/lexical-tokens.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_ip_addresses_ipv6_address_parser_fasldir = $(bundledlibsdir)/nausicaa/parser-tools/ip-addresses
lib_nausicaa_parser_tools_ip_addresses_ipv6_address_parser_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools/ip-addresses
nodist_lib_nausicaa_parser_tools_ip_addresses_ipv6_address_parser_fasl_DATA = lib/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_ip_addresses_ipv6_address_parser_sls_DATA = lib/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.sls
CLEANFILES += lib/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.fasl
endif

lib/nausicaa/parser-tools/ipv4-addresses.fasl: \
		lib/nausicaa/parser-tools/ipv4-addresses.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/ip-addresses/ipv4-address-lexer.fasl \
		lib/nausicaa/parser-tools/ip-addresses/ipv4-address-parser.fasl \
		lib/vicare/language-extensions/makers.fasl \
		lib/vicare/parser-tools/silex/lexer.fasl \
		lib/nausicaa/parser-tools/lexical-tokens.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_ipv4_addresses_fasldir = $(bundledlibsdir)/nausicaa/parser-tools
lib_nausicaa_parser_tools_ipv4_addresses_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools
nodist_lib_nausicaa_parser_tools_ipv4_addresses_fasl_DATA = lib/nausicaa/parser-tools/ipv4-addresses.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_ipv4_addresses_sls_DATA = lib/nausicaa/parser-tools/ipv4-addresses.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/ipv4-addresses.sls
CLEANFILES += lib/nausicaa/parser-tools/ipv4-addresses.fasl
endif

lib/nausicaa/parser-tools/ipv6-addresses.fasl: \
		lib/nausicaa/parser-tools/ipv6-addresses.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/ip-addresses/ipv6-address-lexer.fasl \
		lib/nausicaa/parser-tools/ip-addresses/ipv6-address-parser.fasl \
		lib/vicare/language-extensions/makers.fasl \
		lib/vicare/parser-tools/silex/lexer.fasl \
		lib/nausicaa/parser-tools/lexical-tokens.fasl \
		lib/nausicaa/parser-tools/source-locations.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_ipv6_addresses_fasldir = $(bundledlibsdir)/nausicaa/parser-tools
lib_nausicaa_parser_tools_ipv6_addresses_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools
nodist_lib_nausicaa_parser_tools_ipv6_addresses_fasl_DATA = lib/nausicaa/parser-tools/ipv6-addresses.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_ipv6_addresses_sls_DATA = lib/nausicaa/parser-tools/ipv6-addresses.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/ipv6-addresses.sls
CLEANFILES += lib/nausicaa/parser-tools/ipv6-addresses.fasl
endif

lib/nausicaa/parser-tools/uri.fasl: \
		lib/nausicaa/parser-tools/uri.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/ipv4-addresses.fasl \
		lib/nausicaa/parser-tools/ipv6-addresses.fasl \
		lib/vicare/language-extensions/makers.fasl \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/language-extensions/ascii-chars.fasl \
		lib/vicare/arguments/validation.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_uri_fasldir = $(bundledlibsdir)/nausicaa/parser-tools
lib_nausicaa_parser_tools_uri_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools
nodist_lib_nausicaa_parser_tools_uri_fasl_DATA = lib/nausicaa/parser-tools/uri.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_uri_sls_DATA = lib/nausicaa/parser-tools/uri.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/uri.sls
CLEANFILES += lib/nausicaa/parser-tools/uri.fasl
endif

lib/nausicaa/uri/ip.fasl: \
		lib/nausicaa/uri/ip.sls \
		lib/nausicaa.fasl \
		lib/vicare/language-extensions/keywords.fasl \
		lib/vicare/language-extensions/ascii-chars.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_uri_ip_fasldir = $(bundledlibsdir)/nausicaa/uri
lib_nausicaa_uri_ip_slsdir  = $(bundledlibsdir)/nausicaa/uri
nodist_lib_nausicaa_uri_ip_fasl_DATA = lib/nausicaa/uri/ip.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_uri_ip_sls_DATA = lib/nausicaa/uri/ip.sls
endif
EXTRA_DIST += lib/nausicaa/uri/ip.sls
CLEANFILES += lib/nausicaa/uri/ip.fasl
endif

lib/nausicaa/uri.fasl: \
		lib/nausicaa/uri.sls \
		lib/nausicaa.fasl \
		lib/vicare/language-extensions/makers.fasl \
		lib/vicare/unsafe/operations.fasl \
		lib/vicare/language-extensions/ascii-chars.fasl \
		lib/nausicaa/uri/ip.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_uri_fasldir = $(bundledlibsdir)/nausicaa
lib_nausicaa_uri_slsdir  = $(bundledlibsdir)/nausicaa
nodist_lib_nausicaa_uri_fasl_DATA = lib/nausicaa/uri.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_uri_sls_DATA = lib/nausicaa/uri.sls
endif
EXTRA_DIST += lib/nausicaa/uri.sls
CLEANFILES += lib/nausicaa/uri.fasl
endif

lib/nausicaa/parser-tools/uri/utilities.fasl: \
		lib/nausicaa/parser-tools/uri/utilities.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/parser-tools/uri.fasl \
		lib/nausicaa/uri.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_parser_tools_uri_utilities_fasldir = $(bundledlibsdir)/nausicaa/parser-tools/uri
lib_nausicaa_parser_tools_uri_utilities_slsdir  = $(bundledlibsdir)/nausicaa/parser-tools/uri
nodist_lib_nausicaa_parser_tools_uri_utilities_fasl_DATA = lib/nausicaa/parser-tools/uri/utilities.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_parser_tools_uri_utilities_sls_DATA = lib/nausicaa/parser-tools/uri/utilities.sls
endif
EXTRA_DIST += lib/nausicaa/parser-tools/uri/utilities.sls
CLEANFILES += lib/nausicaa/parser-tools/uri/utilities.fasl
endif

lib/nausicaa/uri/pathnames/abstract.fasl: \
		lib/nausicaa/uri/pathnames/abstract.sls \
		lib/nausicaa.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_uri_pathnames_abstract_fasldir = $(bundledlibsdir)/nausicaa/uri/pathnames
lib_nausicaa_uri_pathnames_abstract_slsdir  = $(bundledlibsdir)/nausicaa/uri/pathnames
nodist_lib_nausicaa_uri_pathnames_abstract_fasl_DATA = lib/nausicaa/uri/pathnames/abstract.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_uri_pathnames_abstract_sls_DATA = lib/nausicaa/uri/pathnames/abstract.sls
endif
EXTRA_DIST += lib/nausicaa/uri/pathnames/abstract.sls
CLEANFILES += lib/nausicaa/uri/pathnames/abstract.fasl
endif

lib/nausicaa/uri/pathnames/unix.fasl: \
		lib/nausicaa/uri/pathnames/unix.sls \
		lib/nausicaa.fasl \
		lib/vicare/unsafe/operations.fasl \
		lib/nausicaa/uri/pathnames/abstract.fasl \
		lib/vicare/parser-tools/unix-pathnames.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_uri_pathnames_unix_fasldir = $(bundledlibsdir)/nausicaa/uri/pathnames
lib_nausicaa_uri_pathnames_unix_slsdir  = $(bundledlibsdir)/nausicaa/uri/pathnames
nodist_lib_nausicaa_uri_pathnames_unix_fasl_DATA = lib/nausicaa/uri/pathnames/unix.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_uri_pathnames_unix_sls_DATA = lib/nausicaa/uri/pathnames/unix.sls
endif
EXTRA_DIST += lib/nausicaa/uri/pathnames/unix.sls
CLEANFILES += lib/nausicaa/uri/pathnames/unix.fasl
endif

lib/nausicaa/uri/pathnames.fasl: \
		lib/nausicaa/uri/pathnames.sls \
		lib/nausicaa/uri/pathnames/unix.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_uri_pathnames_fasldir = $(bundledlibsdir)/nausicaa/uri
lib_nausicaa_uri_pathnames_slsdir  = $(bundledlibsdir)/nausicaa/uri
nodist_lib_nausicaa_uri_pathnames_fasl_DATA = lib/nausicaa/uri/pathnames.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_uri_pathnames_sls_DATA = lib/nausicaa/uri/pathnames.sls
endif
CLEANFILES += lib/nausicaa/uri/pathnames.fasl
endif

lib/nausicaa/mehve.fasl: \
		lib/nausicaa/mehve.sls \
		lib/nausicaa.fasl \
		lib/nausicaa/mehve/language/numerics/predicates.fasl \
		lib/nausicaa/mehve/language/numerics/arithmetics.fasl \
		lib/nausicaa/mehve/language/numerics/parts.fasl \
		lib/nausicaa/mehve/language/numerics/transcendental.fasl \
		lib/nausicaa/mehve/language/input-output.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_mehve_fasldir = $(bundledlibsdir)/nausicaa
lib_nausicaa_mehve_slsdir  = $(bundledlibsdir)/nausicaa
nodist_lib_nausicaa_mehve_fasl_DATA = lib/nausicaa/mehve.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_mehve_sls_DATA = lib/nausicaa/mehve.sls
endif
EXTRA_DIST += lib/nausicaa/mehve.sls
CLEANFILES += lib/nausicaa/mehve.fasl
endif

lib/nausicaa/mehve/language/numerics/predicates.fasl: \
		lib/nausicaa/mehve/language/numerics/predicates.sls \
		lib/nausicaa.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_mehve_language_numerics_predicates_fasldir = $(bundledlibsdir)/nausicaa/mehve/language/numerics
lib_nausicaa_mehve_language_numerics_predicates_slsdir  = $(bundledlibsdir)/nausicaa/mehve/language/numerics
nodist_lib_nausicaa_mehve_language_numerics_predicates_fasl_DATA = lib/nausicaa/mehve/language/numerics/predicates.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_mehve_language_numerics_predicates_sls_DATA = lib/nausicaa/mehve/language/numerics/predicates.sls
endif
EXTRA_DIST += lib/nausicaa/mehve/language/numerics/predicates.sls
CLEANFILES += lib/nausicaa/mehve/language/numerics/predicates.fasl
endif

lib/nausicaa/mehve/language/numerics/arithmetics.fasl: \
		lib/nausicaa/mehve/language/numerics/arithmetics.sls \
		lib/nausicaa.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_mehve_language_numerics_arithmetics_fasldir = $(bundledlibsdir)/nausicaa/mehve/language/numerics
lib_nausicaa_mehve_language_numerics_arithmetics_slsdir  = $(bundledlibsdir)/nausicaa/mehve/language/numerics
nodist_lib_nausicaa_mehve_language_numerics_arithmetics_fasl_DATA = lib/nausicaa/mehve/language/numerics/arithmetics.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_mehve_language_numerics_arithmetics_sls_DATA = lib/nausicaa/mehve/language/numerics/arithmetics.sls
endif
EXTRA_DIST += lib/nausicaa/mehve/language/numerics/arithmetics.sls
CLEANFILES += lib/nausicaa/mehve/language/numerics/arithmetics.fasl
endif

lib/nausicaa/mehve/language/numerics/parts.fasl: \
		lib/nausicaa/mehve/language/numerics/parts.sls \
		lib/nausicaa.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_mehve_language_numerics_parts_fasldir = $(bundledlibsdir)/nausicaa/mehve/language/numerics
lib_nausicaa_mehve_language_numerics_parts_slsdir  = $(bundledlibsdir)/nausicaa/mehve/language/numerics
nodist_lib_nausicaa_mehve_language_numerics_parts_fasl_DATA = lib/nausicaa/mehve/language/numerics/parts.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_mehve_language_numerics_parts_sls_DATA = lib/nausicaa/mehve/language/numerics/parts.sls
endif
EXTRA_DIST += lib/nausicaa/mehve/language/numerics/parts.sls
CLEANFILES += lib/nausicaa/mehve/language/numerics/parts.fasl
endif

lib/nausicaa/mehve/language/numerics/transcendental.fasl: \
		lib/nausicaa/mehve/language/numerics/transcendental.sls \
		lib/nausicaa.fasl \
		lib/vicare/unsafe/operations.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_mehve_language_numerics_transcendental_fasldir = $(bundledlibsdir)/nausicaa/mehve/language/numerics
lib_nausicaa_mehve_language_numerics_transcendental_slsdir  = $(bundledlibsdir)/nausicaa/mehve/language/numerics
nodist_lib_nausicaa_mehve_language_numerics_transcendental_fasl_DATA = lib/nausicaa/mehve/language/numerics/transcendental.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_mehve_language_numerics_transcendental_sls_DATA = lib/nausicaa/mehve/language/numerics/transcendental.sls
endif
EXTRA_DIST += lib/nausicaa/mehve/language/numerics/transcendental.sls
CLEANFILES += lib/nausicaa/mehve/language/numerics/transcendental.fasl
endif

lib/nausicaa/mehve/language/input-output.fasl: \
		lib/nausicaa/mehve/language/input-output.sls \
		lib/nausicaa.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

if WANT_NAUSICAA
lib_nausicaa_mehve_language_input_output_fasldir = $(bundledlibsdir)/nausicaa/mehve/language
lib_nausicaa_mehve_language_input_output_slsdir  = $(bundledlibsdir)/nausicaa/mehve/language
nodist_lib_nausicaa_mehve_language_input_output_fasl_DATA = lib/nausicaa/mehve/language/input-output.fasl
if WANT_INSTALL_SOURCES
dist_lib_nausicaa_mehve_language_input_output_sls_DATA = lib/nausicaa/mehve/language/input-output.sls
endif
EXTRA_DIST += lib/nausicaa/mehve/language/input-output.sls
CLEANFILES += lib/nausicaa/mehve/language/input-output.fasl
endif


### end of file
# Local Variables:
# mode: makefile-automake
# End:
