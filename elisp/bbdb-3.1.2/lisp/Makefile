# Makefile.in generated by automake 1.14 from Makefile.am.
# lisp/Makefile.  Generated from Makefile.in by configure.

# Copyright (C) 1994-2013 Free Software Foundation, Inc.

# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.



# lisp/Makefile.am for BBDB
#
# Copyright (C) 2010-2014 Roland Winkler <winkler@gnu.org>

# Author: Roland Winkler <winkler@gnu.org>
#  	  Christian Egli <christian.egli@sbs.ch>
# 
# This file is part of the Insidious Big Brother Database (aka BBDB),
# 
# BBDB is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# BBDB is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with BBDB.  If not, see <http://www.gnu.org/licenses/>.



am__is_gnu_make = test -n '$(MAKEFILE_LIST)' && test -n '$(MAKELEVEL)'
am__make_running_with_option = \
  case $${target_option-} in \
      ?) ;; \
      *) echo "am__make_running_with_option: internal error: invalid" \
              "target option '$${target_option-}' specified" >&2; \
         exit 1;; \
  esac; \
  has_opt=no; \
  sane_makeflags=$$MAKEFLAGS; \
  if $(am__is_gnu_make); then \
    sane_makeflags=$$MFLAGS; \
  else \
    case $$MAKEFLAGS in \
      *\\[\ \	]*) \
        bs=\\; \
        sane_makeflags=`printf '%s\n' "$$MAKEFLAGS" \
          | sed "s/$$bs$$bs[$$bs $$bs	]*//g"`;; \
    esac; \
  fi; \
  skip_next=no; \
  strip_trailopt () \
  { \
    flg=`printf '%s\n' "$$flg" | sed "s/$$1.*$$//"`; \
  }; \
  for flg in $$sane_makeflags; do \
    test $$skip_next = yes && { skip_next=no; continue; }; \
    case $$flg in \
      *=*|--*) continue;; \
        -*I) strip_trailopt 'I'; skip_next=yes;; \
      -*I?*) strip_trailopt 'I';; \
        -*O) strip_trailopt 'O'; skip_next=yes;; \
      -*O?*) strip_trailopt 'O';; \
        -*l) strip_trailopt 'l'; skip_next=yes;; \
      -*l?*) strip_trailopt 'l';; \
      -[dEDm]) skip_next=yes;; \
      -[JT]) skip_next=yes;; \
    esac; \
    case $$flg in \
      *$$target_option*) has_opt=yes; break;; \
    esac; \
  done; \
  test $$has_opt = yes
am__make_dryrun = (target_option=n; $(am__make_running_with_option))
am__make_keepgoing = (target_option=k; $(am__make_running_with_option))
pkgdatadir = $(datadir)/bbdb
pkgincludedir = $(includedir)/bbdb
pkglibdir = $(libdir)/bbdb
pkglibexecdir = $(libexecdir)/bbdb
am__cd = CDPATH="$${ZSH_VERSION+.}$(PATH_SEPARATOR)" && cd
install_sh_DATA = $(install_sh) -c -m 644
install_sh_PROGRAM = $(install_sh) -c
install_sh_SCRIPT = $(install_sh) -c
INSTALL_HEADER = $(INSTALL_DATA)
transform = $(program_transform_name)
NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
#am__append_1 = bbdb-vm.el
subdir = lisp
DIST_COMMON = $(srcdir)/Makefile.in $(srcdir)/Makefile.am \
	$(srcdir)/bbdb-pkg.el.in $(am__dist_lisp_LISP_DIST)
ACLOCAL_M4 = $(top_srcdir)/aclocal.m4
am__aclocal_m4_deps = $(top_srcdir)/m4/emacs_vm.m4 \
	$(top_srcdir)/m4/package_date.m4 $(top_srcdir)/configure.ac
am__configure_deps = $(am__aclocal_m4_deps) $(CONFIGURE_DEPENDENCIES) \
	$(ACLOCAL_M4)
mkinstalldirs = $(install_sh) -d
CONFIG_CLEAN_FILES = bbdb-pkg.el
CONFIG_CLEAN_VPATH_FILES =
AM_V_P = $(am__v_P_$(V))
am__v_P_ = $(am__v_P_$(AM_DEFAULT_VERBOSITY))
am__v_P_0 = false
am__v_P_1 = :
AM_V_GEN = $(am__v_GEN_$(V))
am__v_GEN_ = $(am__v_GEN_$(AM_DEFAULT_VERBOSITY))
am__v_GEN_0 = @echo "  GEN     " $@;
am__v_GEN_1 = 
AM_V_at = $(am__v_at_$(V))
am__v_at_ = $(am__v_at_$(AM_DEFAULT_VERBOSITY))
am__v_at_0 = @
am__v_at_1 = 
SOURCES =
DIST_SOURCES =
am__can_run_installinfo = \
  case $$AM_UPDATE_INFO_DIR in \
    n|no|NO) false;; \
    *) (install-info --version) >/dev/null 2>&1;; \
  esac
am__dist_lisp_LISP_DIST = bbdb.el bbdb-anniv.el bbdb-com.el \
	bbdb-gnus.el bbdb-ispell.el bbdb-message.el bbdb-mhe.el \
	bbdb-migrate.el bbdb-mua.el bbdb-pgp.el bbdb-print.el \
	bbdb-rmail.el bbdb-sc.el bbdb-snarf.el bbdb-site.el bbdb-vm.el
am__vpath_adj_setup = srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`;
am__vpath_adj = case $$p in \
    $(srcdir)/*) f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`;; \
    *) f=$$p;; \
  esac;
am__strip_dir = f=`echo $$p | sed -e 's|^.*/||'`;
am__install_max = 40
am__nobase_strip_setup = \
  srcdirstrip=`echo "$(srcdir)" | sed 's/[].[^$$\\*|]/\\\\&/g'`
am__nobase_strip = \
  for p in $$list; do echo "$$p"; done | sed -e "s|$$srcdirstrip/||"
am__nobase_list = $(am__nobase_strip_setup); \
  for p in $$list; do echo "$$p $$p"; done | \
  sed "s| $$srcdirstrip/| |;"' / .*\//!s/ .*/ ./; s,\( .*\)/[^/]*$$,\1,' | \
  $(AWK) 'BEGIN { files["."] = "" } { files[$$2] = files[$$2] " " $$1; \
    if (++n[$$2] == $(am__install_max)) \
      { print $$2, files[$$2]; n[$$2] = 0; files[$$2] = "" } } \
    END { for (dir in files) print dir, files[dir] }'
am__base_list = \
  sed '$$!N;$$!N;$$!N;$$!N;$$!N;$$!N;$$!N;s/\n/ /g' | \
  sed '$$!N;$$!N;$$!N;$$!N;s/\n/ /g'
am__uninstall_files_from_dir = { \
  test -z "$$files" \
    || { test ! -d "$$dir" && test ! -f "$$dir" && test ! -r "$$dir"; } \
    || { echo " ( cd '$$dir' && rm -f" $$files ")"; \
         $(am__cd) "$$dir" && rm -f $$files; }; \
  }
am__installdirs = "$(DESTDIR)$(lispdir)" "$(DESTDIR)$(lispdir)"
dist_lispLISP_INSTALL = $(INSTALL_DATA)
LISP = $(dist_lisp_LISP)
am__ELFILES = bbdb-anniv.el bbdb-com.el bbdb-gnus.el bbdb-ispell.el \
	bbdb-message.el bbdb-mhe.el bbdb-migrate.el bbdb-mua.el \
	bbdb-pgp.el bbdb-print.el bbdb-rmail.el bbdb-sc.el \
	bbdb-site.el bbdb-snarf.el bbdb-vm.el bbdb.el
am__ELCFILES = $(am__ELFILES:.el=.elc)
ELCFILES = $(LISP:.el=.elc)
DATA = $(lisp_DATA)
am__tagged_files = $(HEADERS) $(SOURCES) $(TAGS_FILES) $(LISP)
# Read a list of newline-separated strings from the standard input,
# and print each of them once, without duplicates.  Input order is
# *not* preserved.
am__uniquify_input = $(AWK) '\
  BEGIN { nonempty = 0; } \
  { items[$$0] = 1; nonempty = 1; } \
  END { if (nonempty) { for (i in items) print i; }; } \
'
# Make sure the list of sources is unique.  This is necessary because,
# e.g., the same source file might be shared among _SOURCES variables
# for different programs/libraries.
am__define_uniq_tagged_files = \
  list='$(am__tagged_files)'; \
  unique=`for i in $$list; do \
    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
  done | $(am__uniquify_input)`
ETAGS = etags
CTAGS = ctags
DISTFILES = $(DIST_COMMON) $(DIST_SOURCES) $(TEXINFOS) $(EXTRA_DIST)
ACLOCAL = ${SHELL} /Users/mannd/.emacs.d/elisp/bbdb-3.1.2/missing aclocal-1.14
AMTAR = $${TAR-tar}
AM_DEFAULT_VERBOSITY = 1
AM_ELCFLAGS = 
AUTOCONF = ${SHELL} /Users/mannd/.emacs.d/elisp/bbdb-3.1.2/missing autoconf
AUTOHEADER = ${SHELL} /Users/mannd/.emacs.d/elisp/bbdb-3.1.2/missing autoheader
AUTOMAKE = ${SHELL} /Users/mannd/.emacs.d/elisp/bbdb-3.1.2/missing automake-1.14
AWK = awk
CYGPATH_W = echo
DEFS = -DPACKAGE_NAME=\"bbdb\" -DPACKAGE_TARNAME=\"bbdb\" -DPACKAGE_VERSION=\"3.1.2\" -DPACKAGE_STRING=\"bbdb\ 3.1.2\" -DPACKAGE_BUGREPORT=\"bbdb-info@lists.sourceforge.net\" -DPACKAGE_URL=\"http://savannah.nongnu.org/projects/bbdb/\" -DPACKAGE=\"bbdb\" -DVERSION=\"3.1.2\"
ECHO_C = \c
ECHO_N = 
ECHO_T = 
EMACS = emacs
EMACSLOADPATH = 
INSTALL = /usr/local/opt/coreutils/libexec/gnubin/install -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}
INSTALL_STRIP_PROGRAM = $(install_sh) -c -s
LIBOBJS = 
LIBS = 
LTLIBOBJS = 
MAKEINFO = ${SHELL} /Users/mannd/.emacs.d/elisp/bbdb-3.1.2/missing makeinfo
MKDIR_P = /usr/local/opt/coreutils/libexec/gnubin/mkdir -p
PACKAGE = bbdb
PACKAGE_BUGREPORT = bbdb-info@lists.sourceforge.net
PACKAGE_DATE = 2017-02-11 20:13:43 -0700
PACKAGE_NAME = bbdb
PACKAGE_STRING = bbdb 3.1.2
PACKAGE_TARNAME = bbdb
PACKAGE_URL = http://savannah.nongnu.org/projects/bbdb/
PACKAGE_VERSION = 3.1.2
PATH_SEPARATOR = :
SET_MAKE = 
SHELL = /bin/sh
STRIP = 
VERSION = 3.1.2
abs_builddir = /Users/mannd/.emacs.d/elisp/bbdb-3.1.2/lisp
abs_srcdir = /Users/mannd/.emacs.d/elisp/bbdb-3.1.2/lisp
abs_top_builddir = /Users/mannd/.emacs.d/elisp/bbdb-3.1.2
abs_top_srcdir = /Users/mannd/.emacs.d/elisp/bbdb-3.1.2
am__leading_dot = .
am__tar = $${TAR-tar} chof - "$$tardir"
am__untar = $${TAR-tar} xf -
bindir = ${exec_prefix}/bin
build_alias = 
builddir = .
datadir = ${datarootdir}
datarootdir = ${prefix}/share
docdir = ${datarootdir}/doc/${PACKAGE_TARNAME}
dvidir = ${docdir}
exec_prefix = ${prefix}
host_alias = 
htmldir = ${docdir}
includedir = ${prefix}/include
infodir = ${datarootdir}/info
install_sh = ${SHELL} /Users/mannd/.emacs.d/elisp/bbdb-3.1.2/install-sh
libdir = ${exec_prefix}/lib
libexecdir = ${exec_prefix}/libexec
lispdir = ${datadir}/emacs/site-lisp
localedir = ${datarootdir}/locale
localstatedir = ${prefix}/var
mandir = ${datarootdir}/man
mkdir_p = $(MKDIR_P)
oldincludedir = /usr/include
pdfdir = ${docdir}
prefix = /usr/local
program_transform_name = s,x,x,
psdir = ${docdir}
sbindir = ${exec_prefix}/sbin
sharedstatedir = ${prefix}/com
srcdir = .
sysconfdir = ${prefix}/etc
target_alias = 
top_build_prefix = ../
top_builddir = ..
top_srcdir = ..
dist_lisp_LISP = bbdb.el bbdb-anniv.el bbdb-com.el bbdb-gnus.el \
	bbdb-ispell.el bbdb-message.el bbdb-mhe.el bbdb-migrate.el \
	bbdb-mua.el bbdb-pgp.el bbdb-print.el bbdb-rmail.el bbdb-sc.el \
	bbdb-snarf.el bbdb-site.el $(am__append_1)
# We use the Automake variable AM_ELCFLAGS to include the VM lisp directory
# in the Emacs load path when compiling BBDB with VM support.
lisp_DATA = bbdb-loaddefs.el
MOSTLYCLEANFILES = bbdb-loaddefs.el
CLEANFILES = bbdb-site.el bbdb-pkg.el

# The Emacs function define-package is not autoloaded.
# So we assume we need not compile bbdb-pkg.el (which otherwise
# results in a compiler warning that define-package is not defined).
EXTRA_DIST = bbdb-site.el.in bbdb-pkg.el makefile-temp

# Be sure bbdb-site.el exists early for "(require 'bbdb-site)"
BUILT_SOURCES = bbdb-site.el
all: $(BUILT_SOURCES)
	$(MAKE) $(AM_MAKEFLAGS) all-am

.SUFFIXES:
.SUFFIXES: .el .elc
$(srcdir)/Makefile.in:  $(srcdir)/Makefile.am  $(am__configure_deps)
	@for dep in $?; do \
	  case '$(am__configure_deps)' in \
	    *$$dep*) \
	      ( cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh ) \
	        && { if test -f $@; then exit 0; else break; fi; }; \
	      exit 1;; \
	  esac; \
	done; \
	echo ' cd $(top_srcdir) && $(AUTOMAKE) --gnu lisp/Makefile'; \
	$(am__cd) $(top_srcdir) && \
	  $(AUTOMAKE) --gnu lisp/Makefile
.PRECIOUS: Makefile
Makefile: $(srcdir)/Makefile.in $(top_builddir)/config.status
	@case '$?' in \
	  *config.status*) \
	    cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh;; \
	  *) \
	    echo ' cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@ $(am__depfiles_maybe)'; \
	    cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@ $(am__depfiles_maybe);; \
	esac;

$(top_builddir)/config.status: $(top_srcdir)/configure $(CONFIG_STATUS_DEPENDENCIES)
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh

$(top_srcdir)/configure:  $(am__configure_deps)
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh
$(ACLOCAL_M4):  $(am__aclocal_m4_deps)
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh
$(am__aclocal_m4_deps):
bbdb-pkg.el: $(top_builddir)/config.status $(srcdir)/bbdb-pkg.el.in
	cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

.el.elc:
	if test "$(EMACS)" != "no"; then \
	  am__dir=. am__subdir_includes=''; \
	  case $@ in */*) \
	    am__dir=`echo '$@' | sed 's,/[^/]*$$,,'`; \
	    am__subdir_includes="-L $$am__dir -L $(srcdir)/$$am__dir"; \
	  esac; \
	  test -d "$$am__dir" || $(MKDIR_P) "$$am__dir" || exit 1; \
	  $(EMACS) --batch \
	    $(AM_ELCFLAGS) $(ELCFLAGS) \
	    $$am__subdir_includes -L $(builddir) -L $(srcdir) \
	    --eval "(defun byte-compile-dest-file (f) \"$@\")" \
	    --eval "(unless (byte-compile-file \"$<\") (kill-emacs 1))"; \
	else :; fi

install-dist_lispLISP: $(dist_lisp_LISP) $(ELCFILES)
	@$(NORMAL_INSTALL)
	@if test "$(EMACS)" != no && test -n "$(lispdir)"; then \
	  list='$(dist_lisp_LISP)'; \
	  if test -n "$$list"; then \
	    echo " $(MKDIR_P) '$(DESTDIR)$(lispdir)'"; \
	    $(MKDIR_P) "$(DESTDIR)$(lispdir)" || exit 1; \
	  fi; \
	  for p in $$list; do \
	    if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	    $(am__strip_dir) \
	    echo " $(dist_lispLISP_INSTALL) '$$d$$p' '$(DESTDIR)$(lispdir)/$$f'"; \
	    $(dist_lispLISP_INSTALL) "$$d$$p" "$(DESTDIR)$(lispdir)/$$f" || exit $$?; \
	    if test -f $${p}c; then \
	      echo " $(dist_lispLISP_INSTALL) '$${p}c' '$(DESTDIR)$(lispdir)/$${f}c'"; \
	      $(dist_lispLISP_INSTALL) "$${p}c" "$(DESTDIR)$(lispdir)/$${f}c" || exit $$?; \
	    else : ; fi; \
	  done; \
	else : ; fi

uninstall-dist_lispLISP:
	@$(NORMAL_UNINSTALL)
	@test "$(EMACS)" != no && test -n "$(lispdir)" || exit 0; \
	list='$(dist_lisp_LISP)'; \
	files=`for p in $$list; do echo $$p; done | sed -e 's|^.*/||'`; \
	files="$$files "`echo "$$files" | sed 's|$$|c|'`; \
	dir='$(DESTDIR)$(lispdir)'; $(am__uninstall_files_from_dir)

clean-lisp:
	-rm -f $(ELCFILES)
install-lispDATA: $(lisp_DATA)
	@$(NORMAL_INSTALL)
	@list='$(lisp_DATA)'; test -n "$(lispdir)" || list=; \
	if test -n "$$list"; then \
	  echo " $(MKDIR_P) '$(DESTDIR)$(lispdir)'"; \
	  $(MKDIR_P) "$(DESTDIR)$(lispdir)" || exit 1; \
	fi; \
	for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  echo "$$d$$p"; \
	done | $(am__base_list) | \
	while read files; do \
	  echo " $(INSTALL_DATA) $$files '$(DESTDIR)$(lispdir)'"; \
	  $(INSTALL_DATA) $$files "$(DESTDIR)$(lispdir)" || exit $$?; \
	done

uninstall-lispDATA:
	@$(NORMAL_UNINSTALL)
	@list='$(lisp_DATA)'; test -n "$(lispdir)" || list=; \
	files=`for p in $$list; do echo $$p; done | sed -e 's|^.*/||'`; \
	dir='$(DESTDIR)$(lispdir)'; $(am__uninstall_files_from_dir)

ID: $(am__tagged_files)
	$(am__define_uniq_tagged_files); mkid -fID $$unique
tags: tags-am
TAGS: tags

tags-am: $(TAGS_DEPENDENCIES) $(am__tagged_files)
	set x; \
	here=`pwd`; \
	$(am__define_uniq_tagged_files); \
	shift; \
	if test -z "$(ETAGS_ARGS)$$*$$unique"; then :; else \
	  test -n "$$unique" || unique=$$empty_fix; \
	  if test $$# -gt 0; then \
	    $(ETAGS) $(ETAGSFLAGS) $(AM_ETAGSFLAGS) $(ETAGS_ARGS) \
	      "$$@" $$unique; \
	  else \
	    $(ETAGS) $(ETAGSFLAGS) $(AM_ETAGSFLAGS) $(ETAGS_ARGS) \
	      $$unique; \
	  fi; \
	fi
ctags: ctags-am

CTAGS: ctags
ctags-am: $(TAGS_DEPENDENCIES) $(am__tagged_files)
	$(am__define_uniq_tagged_files); \
	test -z "$(CTAGS_ARGS)$$unique" \
	  || $(CTAGS) $(CTAGSFLAGS) $(AM_CTAGSFLAGS) $(CTAGS_ARGS) \
	     $$unique

GTAGS:
	here=`$(am__cd) $(top_builddir) && pwd` \
	  && $(am__cd) $(top_srcdir) \
	  && gtags -i $(GTAGS_ARGS) "$$here"
cscopelist: cscopelist-am

cscopelist-am: $(am__tagged_files)
	list='$(am__tagged_files)'; \
	case "$(srcdir)" in \
	  [\\/]* | ?:[\\/]*) sdir="$(srcdir)" ;; \
	  *) sdir=$(subdir)/$(srcdir) ;; \
	esac; \
	for i in $$list; do \
	  if test -f "$$i"; then \
	    echo "$(subdir)/$$i"; \
	  else \
	    echo "$$sdir/$$i"; \
	  fi; \
	done >> $(top_builddir)/cscope.files

distclean-tags:
	-rm -f TAGS ID GTAGS GRTAGS GSYMS GPATH tags

distdir: $(DISTFILES)
	@srcdirstrip=`echo "$(srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	topsrcdirstrip=`echo "$(top_srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	list='$(DISTFILES)'; \
	  dist_files=`for file in $$list; do echo $$file; done | \
	  sed -e "s|^$$srcdirstrip/||;t" \
	      -e "s|^$$topsrcdirstrip/|$(top_builddir)/|;t"`; \
	case $$dist_files in \
	  */*) $(MKDIR_P) `echo "$$dist_files" | \
			   sed '/\//!d;s|^|$(distdir)/|;s,/[^/]*$$,,' | \
			   sort -u` ;; \
	esac; \
	for file in $$dist_files; do \
	  if test -f $$file || test -d $$file; then d=.; else d=$(srcdir); fi; \
	  if test -d $$d/$$file; then \
	    dir=`echo "/$$file" | sed -e 's,/[^/]*$$,,'`; \
	    if test -d "$(distdir)/$$file"; then \
	      find "$(distdir)/$$file" -type d ! -perm -700 -exec chmod u+rwx {} \;; \
	    fi; \
	    if test -d $(srcdir)/$$file && test $$d != $(srcdir); then \
	      cp -fpR $(srcdir)/$$file "$(distdir)$$dir" || exit 1; \
	      find "$(distdir)/$$file" -type d ! -perm -700 -exec chmod u+rwx {} \;; \
	    fi; \
	    cp -fpR $$d/$$file "$(distdir)$$dir" || exit 1; \
	  else \
	    test -f "$(distdir)/$$file" \
	    || cp -p $$d/$$file "$(distdir)/$$file" \
	    || exit 1; \
	  fi; \
	done
check-am: all-am
check: $(BUILT_SOURCES)
	$(MAKE) $(AM_MAKEFLAGS) check-am
all-am: Makefile $(LISP) $(ELCFILES) $(DATA)
installdirs:
	for dir in "$(DESTDIR)$(lispdir)" "$(DESTDIR)$(lispdir)"; do \
	  test -z "$$dir" || $(MKDIR_P) "$$dir"; \
	done
install: $(BUILT_SOURCES)
	$(MAKE) $(AM_MAKEFLAGS) install-am
install-exec: install-exec-am
install-data: install-data-am
uninstall: uninstall-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am

installcheck: installcheck-am
install-strip:
	if test -z '$(STRIP)'; then \
	  $(MAKE) $(AM_MAKEFLAGS) INSTALL_PROGRAM="$(INSTALL_STRIP_PROGRAM)" \
	    install_sh_PROGRAM="$(INSTALL_STRIP_PROGRAM)" INSTALL_STRIP_FLAG=-s \
	      install; \
	else \
	  $(MAKE) $(AM_MAKEFLAGS) INSTALL_PROGRAM="$(INSTALL_STRIP_PROGRAM)" \
	    install_sh_PROGRAM="$(INSTALL_STRIP_PROGRAM)" INSTALL_STRIP_FLAG=-s \
	    "INSTALL_PROGRAM_ENV=STRIPPROG='$(STRIP)'" install; \
	fi
mostlyclean-generic:
	-test -z "$(MOSTLYCLEANFILES)" || rm -f $(MOSTLYCLEANFILES)

clean-generic:
	-test -z "$(CLEANFILES)" || rm -f $(CLEANFILES)

distclean-generic:
	-test -z "$(CONFIG_CLEAN_FILES)" || rm -f $(CONFIG_CLEAN_FILES)
	-test . = "$(srcdir)" || test -z "$(CONFIG_CLEAN_VPATH_FILES)" || rm -f $(CONFIG_CLEAN_VPATH_FILES)

maintainer-clean-generic:
	@echo "This command is intended for maintainers to use"
	@echo "it deletes files that may require special tools to rebuild."
	-test -z "$(BUILT_SOURCES)" || rm -f $(BUILT_SOURCES)
clean: clean-am

clean-am: clean-generic clean-lisp mostlyclean-am

distclean: distclean-am
	-rm -f Makefile
distclean-am: clean-am distclean-generic distclean-tags

dvi: dvi-am

dvi-am:

html: html-am

html-am:

info: info-am

info-am:

install-data-am: install-dist_lispLISP install-lispDATA

install-dvi: install-dvi-am

install-dvi-am:

install-exec-am:

install-html: install-html-am

install-html-am:

install-info: install-info-am

install-info-am:

install-man:

install-pdf: install-pdf-am

install-pdf-am:

install-ps: install-ps-am

install-ps-am:

installcheck-am:

maintainer-clean: maintainer-clean-am
	-rm -f Makefile
maintainer-clean-am: distclean-am maintainer-clean-generic

mostlyclean: mostlyclean-am

mostlyclean-am: mostlyclean-generic

pdf: pdf-am

pdf-am:

ps: ps-am

ps-am:

uninstall-am: uninstall-dist_lispLISP uninstall-lispDATA

.MAKE: all check install install-am install-strip

.PHONY: CTAGS GTAGS TAGS all all-am check check-am clean clean-generic \
	clean-lisp cscopelist-am ctags ctags-am distclean \
	distclean-generic distclean-tags distdir dvi dvi-am html \
	html-am info info-am install install-am install-data \
	install-data-am install-dist_lispLISP install-dvi \
	install-dvi-am install-exec install-exec-am install-html \
	install-html-am install-info install-info-am install-lispDATA \
	install-man install-pdf install-pdf-am install-ps \
	install-ps-am install-strip installcheck installcheck-am \
	installdirs maintainer-clean maintainer-clean-generic \
	mostlyclean mostlyclean-generic pdf pdf-am ps ps-am tags \
	tags-am uninstall uninstall-am uninstall-dist_lispLISP \
	uninstall-lispDATA


bbdb-loaddefs.el: $(dist_lisp_LISP)
#	2011-12-11: We switched from bbdb-autoloads.el to bbdb-loaddefs.el.
#	If the user still has an old bbdb-autoloads.el in the BBDB
#	lisp directory (and keeps loading it from the emacs init file),
#	we might get strange error messages that things fail.
#	So we throw an error if these old files are found.
	@if test -f bbdb-autoloads.el -o -f bbdb-autoloads.elc; then \
	  (echo "*** ERROR: Old file(s) \`bbdb-autoloads.el(c)' found ***" ; \
	  echo "*** Delete these files; do not load them from your init file ***") && \
	  false ; \
	fi
	@echo "(provide 'bbdb-loaddefs)" > $@;
	@echo "(if (and load-file-name (file-name-directory load-file-name))" >> $@;
	@echo "    (add-to-list 'load-path (file-name-directory load-file-name)))" >> $@;
	@echo "" >> $@;
#	Generated autoload-file must have an absolute path,
#	$srcdir can be relative.
	$(EMACS) -batch -l autoload \
		--eval '(setq generated-autoload-file "'$(abs_builddir)/$@'")' \
		--eval '(setq make-backup-files nil)' \
		-f batch-update-autoloads $(srcdir)

# Generate bbdb-site.el here as pkgdatadir is only known at "make" time.
# We protect the autoconf variables in the sed regular expressions
# so as not to substitute them when processing Makefile.am.
# Warning: the sed expressions will break if PACKAGE_VERSION, PACKAGE_DATE,
# or pkgdatadir contain '='.

bbdb-site.el: $(top_builddir)/config.status bbdb-site.el.in
	sed -e "s=[@]pkgdatadir[@]=$(pkgdatadir)=" \
	    -e "s=[@]PACKAGE_VERSION[@]=$(PACKAGE_VERSION)=" \
	    -e "s=[@]PACKAGE_DATE[@]=$(PACKAGE_DATE)=" \
	    < $@.in > $@

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
