all: scmtap.3 scmtap.html scmtap.info

scmtap.3: scmtap.t2t
	txt2tags --target man -o- $< | sed -e '/^$$/d' -e 's/^\\e$$//' > $@

scmtap.html: scmtap.t2t
	txt2tags --target xhtml --css-sugar -o- $< | sed -e '/^$$/d' -e 's/^\\e$$//' > $@

scmtap.texi: scmtap.t2t
	sed -e 's,\*\*\([^*]*\)\*\*,\1,g' -e 's,//\([^/]*\)//,\1,g' -e 's,__\([^_]*\)__,\1,g' < $< | txt2tags --no-style -t tex -i - -o - | pandoc -f latex -t texinfo -o $@

scmtap.info: scmtap.texi
	makeinfo scmtap.texi

clean:
	rm -f *.3 *.texi *.info *.html

.PHONY: all clean
